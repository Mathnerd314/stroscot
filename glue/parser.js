
/* The core Earley Predictor and Completer.
At each stage of the input, we handling any completed items (things
that matched on the last cycle) and use those to predict what should
come next in the input stream. The completions and any predicted
non-terminals are recursively processed until we reach a set of,
which can be added to the scan list for the next scanner cycle. */
node_cache = {}
held_completions = {} // H in E.Scotts paper

column = columns[i]
# R (items) = Ei (column.items)
items = deque(column)
while items:
item = items.pop()    # remove an element, A say, from R

// The Earley completer
if item.is_complete:   ### (item.s == string)
    if item.node is None:
        label = (item.s, item.start, i)
        item.node = node_cache[label] if label in node_cache else node_cache.setdefault(label, SymbolNode(*label))
        item.node.add_family(item.s, item.rule, item.start, None, None)

    # create_leo_transitives(item.rule.origin, item.start)

    // R Joop Leo right recursion Completer
    if item.rule.origin in transitives[item.start]:
        transitive = transitives[item.start][item.s]
        if transitive.previous in transitives[transitive.column]:
            root_transitive = transitives[transitive.column][transitive.previous]
        else:
            root_transitive = transitive

        new_item = Item(transitive.rule, transitive.ptr, transitive.start)
        label = (root_transitive.s, root_transitive.start, i)
        new_item.node = node_cache[label] if label in node_cache else node_cache.setdefault(label, SymbolNode(*label))
        new_item.node.add_path(root_transitive, item.node)
        if new_item.expect in self.TERMINALS:
            # Add (B :: aC.B, h, y) to Q
            to_scan.add(new_item)
        elif new_item not in column:
            # Add (B :: aC.B, h, y) to Ei and R
            column.add(new_item)
            items.append(new_item)
    // R Regular Earley completer
    else:
        /* Empty has 0 length. If we complete an empty symbol in a particular
         parse step, we need to be able to use that same empty symbol to complete
         any predictions that result, that themselves require empty. Avoids
         infinite recursion on empty symbols.
         held_completions is 'H' in E.Scott's paper. */
        is_empty_item = item.start == i
        if is_empty_item:
            held_completions[item.rule.origin] = item.node

        originators = [originator for originator in columns[item.start] if originator.expect is not None and originator.expect == item.s]
        for originator in originators:
            new_item = originator.advance()
            label = (new_item.s, originator.start, i)
            new_item.node = node_cache[label] if label in node_cache else node_cache.setdefault(label, SymbolNode(*label))
            new_item.node.add_family(new_item.s, new_item.rule, i, originator.node, item.node)
            if new_item.expect in self.TERMINALS:
                # Add (B :: aC.B, h, y) to Q
                to_scan.add(new_item)
            elif new_item not in column:
                # Add (B :: aC.B, h, y) to Ei and R
                column.add(new_item)
                items.append(new_item)

// The Earley predictor
elif item.expect in self.NON_TERMINALS: // (item.s == lr0)
    new_items = []
    for rule in self.predictions[item.expect]:
        new_item = Item(rule, 0, i)
        new_items.append(new_item)

    // Process any held completions (H).
    if item.expect in held_completions:
        new_item = item.advance()
        label = (new_item.s, item.start, i)
        new_item.node = node_cache[label] if label in node_cache else node_cache.setdefault(label, SymbolNode(*label))
        new_item.node.add_family(new_item.s, new_item.rule, new_item.start, item.node, held_completions[item.expect])
        new_items.append(new_item)

    for new_item in new_items:
        if new_item.expect in self.TERMINALS:
            to_scan.add(new_item)
        elif new_item not in column:
            column.add(new_item)
            items.append(new_item)


function earleyParse(input) {
  var s0 = process(predict(new Set(this, 0), this.S));
  var sN = foldl(input, parse_symbol, s0);
  return find_item(this.S, s0, sN);
}

function parse_symbol(set, sym) { return process(scan(set, sym)); }

function process(set) {
  do {
    var len = set.items.length;
    for (var i = 0; i < set.items.length; ++i) {
      var item = set.items[i];
      if (item.tag.nextSymbol) predict(set, item.tag.nextSymbol);
      else complete(item);
    }
  } while (set.items.length > len);  // cheesy nullable-rule handling
  return set;
}

function scan(s1, sym) {
  var s2 = new Set(s1.grammar, s1.position + 1);
  var item = add_item(sym, s1, s2);
  return s2;
}

function complete(c) {
  var items = c.start.wants[c.tag];
  if (items) for (var i = 0; i < items.length; ++i) {
    var item = items[i], tag = item.tag;
    add_derivation(add_item(tag.advance, item.start, c.end, item.rule),
      item, c, item.rule);
  }

  items = c.start.wants_many;
  if (items) for (var i = 0; i < items.length; ++i) {
    var item = items[i], tag = item.tag;
    // rule can get deleted by add_second_derivation
    if (item.rule.wants_sym(c.tag)) {
      add_derivation(add_item(tag.advance, item.start, c.end, item.rule),
        item, c, item.rule);
    }
  }
}

function predict(set, sym) {
  var rules = set.grammar.rules[sym];
  if (rules) for (var i = 0; i < rules.length; ++i) {
    var item = add_item(rules[i].advance, set, set, rules[i]);
    if (!item.tag.production) {
      var empty = add_item('', set, set);
      add_derivation(item, undefined, empty, item.rule);
    }
  }
  return set;
}


S[0..words] = EMPTY - ORDERED - SET
S[0].add((γ → •S, 0))
for k ← from 0 to LENGTH(words) do
  for each state in S[k] do  // S[k] can expand during this loop
    if not FINISHED(state) then
                if NEXT - ELEMENT - OF(state) is a nonterminal then
PREDICTOR(state, k, grammar)         // non-terminal
                else do
  SCANNER(state, k, words)             // terminal
            else do
  COMPLETER(state, k)
        end
    end
return chart

procedure PREDICTOR((A → α•Bβ, j), k, grammar)
for each(B → γ) in GRAMMAR - RULES - FOR(B, grammar) do
  ADD - TO - SET((B → •γ, k), S[k])
end

procedure SCANNER((A → α•aβ, j), k, words)
if a ⊂ PARTS - OF - SPEECH(words[k]) then
ADD - TO - SET((A → αa•β, j), S[k + 1])
end

procedure COMPLETER((B → γ•, x), k)
for each(A → α•Bβ, j) in S[x] do
  ADD - TO - SET((A → αB•β, j), S[k])
end
deriv EmptySet = EmptySet
deriv EmptyString = EmptySet
deriv AnyByte = EmptyString
deriv Byte = if wanted == expected then EmptyString else EmptySet
deriv ByteRange = if wanted in range then EmptyString else EmptySet
deriv KleeneClosure = // ∂a(r∗) = ∂ar · r∗
  Concatenation(deriv subexpression, KleeneClosure)
deriv Concatenation = // ∂a(r · s) = ∂ar · s + ν(r) · ∂as
      if (IsNullable(exp -> head())) {
  return Disjunction(Concatenation(Derivative(exp -> head(), byte),
    exp -> tail()),
    Derivative(exp -> tail(), byte));
} else {
  return Concatenation(Derivative(exp -> head(), byte),
    exp -> tail());
}

    case kComplement:
// 
return Complement(Derivative(exp -> sub(), byte));

    case kConjunction: {
  // 
  std:: list < Exp > subs;
  for (Exp sub : exp -> subexpressions()) {
    sub = Derivative(sub, byte);
    subs.push_back(sub);
  }
  return Conjunction(subs, false);
}

    case kDisjunction: {
  // 
  std:: list < Exp > subs;
  for (Exp sub : exp -> subexpressions()) {
    sub = Derivative(sub, byte);
    subs.push_back(sub);
  }
  return Disjunction(subs, false);
}

    case kCharacterClass:
    case kQuantifier:
break;
  }
abort();
}

@<Initialize | current_earleme |@>@;
@<Return 0 if no alternatives@>@;
@<Initialize | current_earley_set |@>@;
@<Scan from the alternative stack@>@;
@<Pre-populate the completion stack@>@;
while ((cause_p = MARPA_DSTACK_POP(r -> t_completion_stack, YIM))) {
  YIM cause = * cause_p;
  @<Add new Earley items for | cause |@>@;
}
@<Add predictions to | current_earley_set |@>@;
postdot_items_create(r, bv_ok_for_chain, current_earley_set);

@t}\comment{
@>
  /* If no terminals are expected, and there are no Earley items in
       uncompleted Earley sets, we can make no further progress.
       The parse is ``exhausted". */
  count_of_expected_terminals = bv_count(r -> t_bv_nsyid_is_expected);
  if (count_of_expected_terminals <= 0
    && MARPA_DSTACK_LENGTH(r -> t_alternatives) <= 0) {
    @<Set | r | exhausted@>@;
  }
  earley_set_update_items(r, current_earley_set);
  if (r -> t_active_event_count > 0) {
    trigger_events(r);
  }
  return_value = G_EVENT_COUNT(g);
  CLEANUP: ;
  @<Destroy | marpa_r_earleme_complete | locals@>@;
}
return return_value;
}