#!/usr/bin/env python3
"""
vex_to_json.py — Convert vex_ops.txt + vex_op_types.txt into a single JSON.

JSON structure:
  [ { "title": "Section name",       // optional
      "comment": "section prose",    // optional
      "ops": [
        { "name": "Add8",
          "value": "0x1400",         // only for first enum member
          "comment": "prose...",     // from block/inline comments, newlines preserved
          "type": { "args": ["Ity_I8","Ity_I8"], "result": "Ity_I8" }
        }, ...
      ]
    }, ...
  ]

Usage:
  python vex_to_json.py vex_ops.txt vex_op_types.txt [output.json]
  If output is omitted, prints to stdout.
"""

import re, json, sys

# ── Shared: block-comment splitter ────────────────────────────────────────────

def split_segments(source):
    segs, i, n, buf = [], 0, len(source), []
    while i < n:
        if source[i:i+2] == "/*":
            if "".join(buf).strip():
                segs.append(("code", "".join(buf)))
            buf = []
            end = source.find("*/", i+2)
            if end == -1: end = n-2
            body = source[i+2:end]
            lines = [l.strip().lstrip("*").strip() for l in body.splitlines()]
            lines = [l for l in lines if l]
            if lines:
                segs.append(("block", lines))
            i = end+2
        else:
            buf.append(source[i]); i += 1
    if "".join(buf).strip():
        segs.append(("code", "".join(buf)))
    return segs

# ── Parse vex_ops.txt ─────────────────────────────────────────────────────────

MEMBER_RE = re.compile(r"\bIop_([A-Za-z0-9_]+)\s*(?:=\s*(0x[0-9a-fA-F]+|\d+))?")
LINE_CMT  = re.compile(r"//([^\n]*)")
SEC_HDR   = re.compile(r"^-+\s*(.+?)\s*-+$")

def is_section_header(lines): return bool(SEC_HDR.match(lines[0]))
def section_title(lines):
    m = SEC_HDR.match(lines[0]); return m.group(1) if m else lines[0]

def parse_ops(path):
    src = open(path).read()
    enum_match = re.search(r"enum\s*\{(.*)\}", src, re.DOTALL)
    assert enum_match, "Could not find enum body"
    body = enum_match.group(1)

    # Step 1: extract MULTI-LINE block comments as standalone segments,
    # leaving single-line /* */ intact for per-line handling.
    # Build a list of (start, end) for multi-line /* ... */ spans.
    ml_spans = []
    i = 0
    while i < len(body):
        if body[i:i+2] == '/*':
            end = body.find('*/', i+2)
            if end == -1: end = len(body)-2
            if '\n' in body[i:end+2]:
                ml_spans.append((i, end+2))
            i = end+2
        else:
            i += 1

    # Step 2: split body into chunks, replacing multi-line block comments
    # with sentinel tokens we can detect later.
    chunks = []   # list of ('code', text) or ('block', [lines])
    prev = 0
    for (s, e) in ml_spans:
        if prev < s:
            chunks.append(('code', body[prev:s]))
        inner = body[s+2:e-2]
        lines = [l.strip().lstrip('*').strip() for l in inner.splitlines()]
        lines = [l for l in lines if l]
        if lines:
            chunks.append(('block', '\n'.join(lines)))
        prev = e
    if prev < len(body):
        chunks.append(('code', body[prev:]))

    # Step 3: walk chunks, parsing code line by line,
    # handling single-line /* */ as inline comments.
    SINGLE_BLK = re.compile(r'/\*([^*]|\*(?!/))*\*/')
    MEMBER_RE2 = re.compile(r'\bIop_([A-Za-z0-9_]+)\s*(?:=\s*(0x[0-9a-fA-F]+|\d+))?')
    LINE_CMT2  = re.compile(r'//([^\n]*)')
    SEC_HDR2   = re.compile(r'^-+\s*(.+?)\s*-+$')

    def is_sec_hdr(s): return bool(SEC_HDR2.match(s.splitlines()[0]))
    def sec_title(s):
        m = SEC_HDR2.match(s.splitlines()[0]); return m.group(1) if m else s.splitlines()[0]

    sections, cur_sec, pending = [], None, []

    def flush():
        if cur_sec is not None:
            sections.append(cur_sec)

    for chunk in chunks:
        kind = chunk[0]
        if kind == 'block':
            s = chunk[1]
            if is_sec_hdr(s):
                flush()
                rest = '\n'.join(s.splitlines()[1:])
                cur_sec = {'title': sec_title(s),
                           'comment': rest if rest.strip() else None,
                           'ops': []}
                pending = []
            else:
                pending.append(s)
        else:  # code
            if cur_sec is None:
                cur_sec = {'title': None, 'comment': None, 'ops': []}
            for line in chunk[1].splitlines():
                raw = line.strip()
                if not raw:
                    continue

                # Extract single-line /* */ inline comments from this line
                inline_parts = []
                for m in SINGLE_BLK.finditer(raw):
                    text = m.group(0)[2:-2].strip()
                    if text:
                        inline_parts.append(text)
                raw_no_blk = SINGLE_BLK.sub('', raw).strip()

                # Extract // comment
                lc = LINE_CMT2.search(raw_no_blk)
                if lc:
                    inline_parts.append(lc.group(1).strip())
                    raw_no_blk = raw_no_blk[:lc.start()].strip()

                inline = '  '.join(p for p in inline_parts if p) or None

                members = MEMBER_RE2.findall(raw_no_blk)
                if not members:
                    # standalone single-line /* */ comment (no members on this line)
                    if inline_parts:
                        text = '  '.join(p for p in inline_parts if p)
                        if is_sec_hdr(text):
                            flush()
                            cur_sec = {'title': sec_title(text), 'comment': None, 'ops': []}
                            pending = []
                        else:
                            pending.append(text)
                    continue

                group_comment = '\n'.join(pending)
                pending = []

                for mi, (name, val) in enumerate(members):
                    op = {'name': name}
                    if val: op['value'] = val
                    if mi == 0 and group_comment.strip():
                        op['comment'] = group_comment
                    if mi == len(members)-1 and inline:
                        existing = op.get('comment', '')
                        op['comment'] = (existing + '  ' + inline).strip() if existing else inline
                    cur_sec['ops'].append(op)
    flush()
    return sections

# ── Parse vex_op_types.txt ────────────────────────────────────────────────────

def clean_type_name(t):
    if t=="ity_RMode": return "RoundingMode"
    if t.startswith("Ity_"): return t[4:]
    if t.startswith("ity_"): return t[4:]
    return t

def parse_types(path):
    src = re.sub(r"/\*.*?\*/", "", open(path).read(), flags=re.DOTALL)
    CASE_RE  = re.compile(r"\bcase\s+Iop_(\w+)\s*:")
    MACRO_RE = re.compile(
        r"\b(UNARY|BINARY|TERNARY|QUATERNARY|COMPARISON|UNARY_COMPARISON)\s*\(([^)]+)\)")

    def expand(macro, args_str):
        args = [(clean_type_name(a.strip())) for a in args_str.split(",")]
        if macro=="UNARY":             return {"args":[args[0]],"result":args[1]}
        if macro=="BINARY":            return {"args":args[:2],"result":args[2]}
        if macro=="TERNARY":           return {"args":args[:3],"result":args[3]}
        if macro=="QUATERNARY":        return {"args":args[:4],"result":args[4]}
        if macro=="COMPARISON":        return {"args":[args[0],args[0]],"result":"Ity_I1"}
        if macro=="UNARY_COMPARISON":  return {"args":[args[0]],"result":"Ity_I1"}

    tokens = list(re.finditer(
        r"\bcase\s+Iop_(\w+)\s*:|"
        r"\b(UNARY|BINARY|TERNARY|QUATERNARY|COMPARISON|UNARY_COMPARISON)\s*\([^)]+\)",
        src))
    op_types, pending = {}, []
    for m in tokens:
        text = m.group(0)
        if text.startswith("case "):
            pending.append(CASE_RE.match(text).group(1))
        else:
            mm = MACRO_RE.match(text)
            if mm and pending:
                t = expand(mm.group(1), mm.group(2))
                for name in pending: op_types[name] = t
                pending = []
    return op_types

# ── Merge ─────────────────────────────────────────────────────────────────────

def merge(sections, op_types):
    out = []
    for sec in sections:
        ops_out = []
        for op in sec["ops"]:
            e = {"name": op["name"]}
            if "value" in op: e["value"] = op["value"]
            raw_c = op.get("comment")
            if raw_c:
                e["comment"] = raw_c.strip()
            if op["name"] in op_types:
                e["type"] = op_types[op["name"]]
            ops_out.append(e)
        sec_out = {}
        if sec["title"]:    sec_out["title"]   = sec["title"]
        if sec["comment"]:  sec_out["comment"] = sec["comment"]
        sec_out["ops"] = ops_out
        out.append(sec_out)
    return out

# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    if len(sys.argv) < 3:
        print(__doc__); sys.exit(1)
    sections = parse_ops(sys.argv[1])
    op_types = parse_types(sys.argv[2])
    result   = merge(sections, op_types)
    out_json = json.dumps(result, indent=2)
    if len(sys.argv) >= 4:
        open(sys.argv[3], "w").write(out_json)
        print(f"Written to {sys.argv[3]}", file=sys.stderr)
    else:
        print(out_json)

if __name__ == "__main__":
    main()
