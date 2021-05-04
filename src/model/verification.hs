type C -- concrete state
type E

data CPA = CPA
  { transfer :: E -> Set (G, E)
  , merge :: E -> E -> E
  , stopop :: E -> Set E -> Bool
  }

merge_sep e e' = e'
merge_join e e' = e \sqcup e'

stopop_sep e R = \exists e' ∈ R : e \sqsubseteq e'
stopop_join e R = e \sqsubseteq \bigsqcup R

CPA(reached, wait)
INPUT
    a CPA cpa = (T, merge, stopop)
    a set reached of abstract states in E (initially a single state e0)
    a set wait of frontier abstract states, a subset of reached (also e0)
OUTPUT
    a set reached of reachable abstract states
    a set wait of frontier abstract states (empty if the algorithm terminated correctly)

WHILE not wait.empty
  choose eo from wait; remove eo from wait;
  e, p = prec(eo,reached)
  FOR each e' with T(e, e', p)
    FOR each e'' in reached
      // Combine with existing abstract state.
      e_new := merge(e', e'', p);
      IF e_new != e''
        wait    := (wait    union {e_new}) setminus e'';
        reached := (reached union {e_new}) setminus e'';
      ENDIF
    ENDFOR
    // Add new abstract state?
    IF not stop(e', reached, p)
      wait := wait union e';
      reached := reached union e';
    ENDIF
  ENDFOR
ENDWHILE
// wait is empty
return reached
