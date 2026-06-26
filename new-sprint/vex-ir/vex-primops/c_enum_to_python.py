#!/usr/bin/env python3
"""
c_enum_to_python.py — Convert a C enum body to a Python IntEnum/auto() class body.

Input: a snippet of C enum content (just the members, no braces needed),
starting at the first member (e.g. Iop_INVALID=0x1400).
The common prefix (e.g. "Iop_") is auto-detected and stripped.

Output: Python enum members, one per line, with comments preserved:
  - Multi-line /* ... */ block comments → one or more # lines (correctly handled)
  - Inline /* comment */ after a member → appended as  # comment on that member's line
  - // line comments → appended as  # comment on that member's line
  - Multiple members per C line are split, each on its own output line

Usage:
  python c_enum_to_python.py input_enum.txt [prefix]
  Output goes to stdout.  Detected prefix is printed to stderr.
"""

import re
import sys


MEMBER_RE = re.compile(r'([A-Za-z_]\w*)\s*(?:=\s*([^,/\n{}]+))?')
LINE_CMT  = re.compile(r'//([^\n]*)')


def split_segments(source: str) -> list:
    """
    Split source into a list of segments preserving order:
      ('code',  text)    — raw code text (may contain // comments, no /* */)
      ('block', [lines]) — a /* ... */ comment, split into non-empty text lines
    """
    segments = []
    i, n = 0, len(source)
    buf = []

    while i < n:
        if source[i:i+2] == '/*':
            code = ''.join(buf)
            if code.strip():
                segments.append(('code', code))
            buf = []
            end = source.find('*/', i + 2)
            if end == -1:
                end = n - 2
            body = source[i+2 : end]
            lines = [l.strip().lstrip('*').strip() for l in body.splitlines()]
            lines = [l for l in lines if l]
            if lines:
                segments.append(('block', lines))
            i = end + 2
        else:
            buf.append(source[i])
            i += 1

    code = ''.join(buf)
    if code.strip():
        segments.append(('code', code))
    return segments


def parse_code_segment(text: str) -> list:
    """
    Parse a code segment (no block comments) into a list of dicts:
      {'kind': 'member', 'name': str, 'val': str|None, 'comment': str|None}
      {'kind': 'blank'}
    Line structure is preserved so blanks between groups carry through.
    A // comment is attached to the last member on its line.
    """
    items = []
    for line in text.splitlines():
        raw = line.strip()
        if not raw:
            items.append({'kind': 'blank'})
            continue

        # Extract trailing // comment
        lc = LINE_CMT.search(raw)
        inline = None
        if lc:
            inline = lc.group(1).strip()
            raw = raw[:lc.start()].strip()

        # Parse members (comma-separated)
        members_on_line = []
        for chunk in raw.split(','):
            chunk = chunk.strip()
            if not chunk:
                continue
            m = MEMBER_RE.match(chunk)
            if m:
                name = m.group(1).strip()
                val  = m.group(2).strip() if m.group(2) else None
                members_on_line.append({'kind': 'member', 'name': name, 'val': val, 'comment': None})

        # Attach inline comment to last member on this line
        if members_on_line and inline:
            members_on_line[-1]['comment'] = inline

        items.extend(members_on_line)

    return items


def detect_prefix(items: list) -> str:
    for item in items:
        if item['kind'] == 'member':
            name = item['name']
            idx = name.rfind('_')
            return name[:idx + 1] if idx != -1 else ''
    return ''


def strip_prefix(name: str, prefix: str) -> str:
    return name[len(prefix):] if prefix and name.startswith(prefix) else name


def convert(source: str, prefix: str = None) -> str:
    segments = split_segments(source)

    # First pass: build flat item list to detect prefix
    all_items = []
    for kind, data in segments:
        if kind == 'code':
            all_items.extend(parse_code_segment(data))
        else:
            all_items.append({'kind': 'block_comment', 'lines': data})

    if prefix is None:
        prefix = detect_prefix(all_items)

    # Second pass: attach a following ('block', ...) segment as inline comment
    # to the last member of the preceding code segment, when the code segment
    # ends with a member (i.e. no trailing blank lines).
    merged = []
    seg_list = list(segments)
    i = 0
    while i < len(seg_list):
        kind, data = seg_list[i]
        if kind == 'code':
            items = parse_code_segment(data)
            # Check if next segment is a block comment that should be inline
            if (i + 1 < len(seg_list)
                    and seg_list[i+1][0] == 'block'):
                # Find the last member in items (skip trailing blanks)
                last_member_idx = None
                for j in range(len(items) - 1, -1, -1):
                    if items[j]['kind'] == 'member':
                        last_member_idx = j
                        break
                    elif items[j]['kind'] == 'blank':
                        # Trailing blank means comment is stand-alone, not inline
                        break
                if last_member_idx is not None:
                    comment_lines = seg_list[i+1][1]
                    existing = items[last_member_idx]['comment']
                    extra = '  '.join(comment_lines)
                    items[last_member_idx]['comment'] = (
                        (existing + '  ' + extra) if existing else extra
                    )
                    i += 2  # consume the block segment too
                    merged.extend(items)
                    continue
            merged.extend(items)
        else:  # block comment standing alone
            merged.append({'kind': 'block_comment', 'lines': data})
        i += 1

    # Render
    out = []
    prev_blank = False

    for item in merged:
        if item['kind'] == 'blank':
            if out and not prev_blank:
                out.append('')
            prev_blank = True
        elif item['kind'] == 'block_comment':
            for l in item['lines']:
                out.append(f'    # {l}')
            prev_blank = False
        elif item['kind'] == 'member':
            name = strip_prefix(item['name'], prefix)
            val  = item['val']
            cmt  = item['comment']
            line = f'    {name} = {val.strip()}' if val else f'    {name} = auto()'
            if cmt:
                line += f'  # {cmt}'
            out.append(line)
            prev_blank = False

    while out and out[0]  == '': out.pop(0)
    while out and out[-1] == '': out.pop()
    return '\n'.join(out)


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(1)
    with open(sys.argv[1], 'r', encoding='utf-8') as f:
        source = f.read()
    prefix = sys.argv[2] if len(sys.argv) >= 3 else None
    result = convert(source, prefix)
    # Report detected prefix
    segs = split_segments(source)
    items = []
    for k, d in segs:
        if k == 'code':
            items.extend(parse_code_segment(d))
    p = detect_prefix(items) if prefix is None else prefix
    if p:
        print(f"# Detected prefix: {p!r}", file=sys.stderr)
    print(result)


if __name__ == '__main__':
    main()
