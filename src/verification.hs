type C -- concrete state
type E


data CPA = CPA
  { transfer :: E -> Set (G, E)
  , merge :: E -> E -> E
  , stopop :: E -> Set E -> Bool
  }

* an abstract domain :math:`D = (C, {\cal E}, \sem{\cdot})`, consisting of

  * a set :math:`C` of concrete states,
  * a bounded join `semi-lattice <https://en.wikipedia.org/wiki/Semilattice>`__ :math:`({\cal E}, \sqsubseteq, \sqcup, \top)` over abstract-domain elements, and
  * a concretization function :math:`\sem{\cdot} : E \to 2^C` that maps each abstract-domain element to its represented set of concrete states.

* a merge operator :math:`\merge :  E × E → E` specifies if and how to merge abstract states when control flow meets. The operator weakens the abstract state (also called widening) that is given as second parameter depending on the first parameter. Note that the operator :math:`\merge` is not commutative, and is not necessarily the same as the join operator of the lattice. The result of :math:`\merge(e, e')` can be anything between :math:`e'` and :math:`\top`. Two simple ones are :math:

merge_sep e e' = e'
merge_join e e' = e \sqcup e'

stopop_sep e R = \exists e' ∈ R : e \sqsubseteq e'
stopop_join e R = e \sqsubseteq \bigsqcup R

CPA(reached, wait)
INPUT
    a CPA cpa = (D, T, merge, stopop)
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
