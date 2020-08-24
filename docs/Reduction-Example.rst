Reduction example
#################

For example, the term ``M = o $ \h. o (h I)`` from pages 26-35 of :cite:`aspertiOptimalImplementationFunctional1999`. To write the derivation tree we must define the recursive type :math:`\Omega = \, !\Omega \to \Omega`.

.. image:: _static/Stroscot_M_Proof_Tree.svg

::

  Root ret
    Cut o1 o1l
      o1/[]/[] = PiRight ^func, [x_o1], [ret_o1], [], [] ->
        x_o1 = BangC x1_o1 x2_o1
          x2_o1 = BangD x2i_o1
            x2i_o1 = PiLeft ^func [(x1_o1r, x1_o1/x1_o1r = Identity)] [(ret_o1l, ret_o1l/ret_o1 = Identity)]
      o1l = PiLeft ^func [(fp,
        fp/[]/[] = Bang f []/[]
          f/[]/[] = PiRight ^func, [h_d], [f_ret], [], [] ->
            Cut o2 o2l
              o2/[]/[] = PiRight ^func, [x_o2], [ret_o2], [], [] ->
                x_o2 = BangC x1_o2 x2_o2
                  x2_o2 = BangD x2i_o2
                    x2i_o2 = PiLeft ^func [(x1_o2r, x1_o2/x1_o2r = Identity)] [(ret_o2l, ret_o2l/ret_o2 = Identity)]
              o2l = PiLeft ^func [(hi_retp,
                hi_retp/[h_d]/[] = Bang hi_ret [h_di]/[]
                  h_di = BangD h
                    Cut i h_app
                      i/[]/[] = Bang i_i []/[]
                        i_i/[]/[] = PiRight ^func, [il], [i_ret], [], [] ->
                          il = BangD ild
                            ild/i_ret = Identity
                      h = PiLeft ^func [(h_appr, h_app/h_appr = Identity)] [(hi_retl, hi_retl/hi_ret = Identity)]
              )] [(f_retl, f_retl/f_ret = Identity)]
      )] [(ret_l, ret_l/ret = Identity)]

1.

.. graphviz::

  digraph {
  Root -> c1
  f -> c2
  h_di -> c3

  Root -> ret [color="red",penwidth=2]

  c1 [label="Cut",shape=doublecircle]
  c1 -> o1 [color="red"]
  c1 -> o1l [color="blue",penwidth=2]

  o1 [label="PiR"]
  o1 -> x_o1 [color="blue"]
  o1 -> ret_o1 [color="red"]

  x_o1 [label="!c"]
  x_o1 -> x1_o1 [color="blue"]
  x_o1 -> x2_o1 [color="blue"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret_o1 /* ret_o1l */ [color="blue"]

  o1l [label="PiL"]
  o1l -> fp [color="red",label="0"]
  o1l -> ret /* ret_l */ [color="blue",penwidth=2]

  fp [label="!p"]
  fp -> f [color="red",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  ret_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]
  ret [label="I"]

  }

2. First we reduce the top cut.

.. graphviz::

  digraph {
  Root -> ret [color="red",penwidth=2]

  c1a [label="Cut",shape=doublecircle]
  c1a -> ret_o1 [color="red"]
  c1a -> ret /* ret_l */ [color="blue",penwidth=2]

  ret_o1 [label="I"]
  ret [label="I"]

  c1b [label="Cut"]
  c1b -> fp [color="red",label="0"]
  c1b -> x_o1 [color="blue"]

  x_o1 [label="!c"]
  x_o1 -> x1_o1 [color="blue"]
  x_o1 -> x2_o1 [color="blue"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret_o1 /* ret_o1l */ [color="blue"]

  fp [label="!p"]
  fp -> f [color="red",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

3.

.. graphviz::

  digraph {
  Root -> ret [color="red",penwidth=2]

  ret [label="I"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fp [color="red",label="0"]
  c1b -> x_o1 [color="blue",penwidth=2]

  x_o1 [label="!c"]
  x_o1 -> x1_o1 [color="blue"]
  x_o1 -> x2_o1 [color="blue",penwidth=2]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue",penwidth=2]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret [color="blue",penwidth=2]

  fp [label="!p"]
  fp -> f [color="red",label="1"]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

4. To handle the duplication from the contraction we duplicate the cuts/promotion rule and introduce a duplication node Dup to incrementally duplicate the rest of the structure:

.. graphviz::

  digraph {
  Root -> ret [color="red",penwidth=2]

  ret [label="I"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fp2 [color="red",label="0"]
  c1b -> x2_o1 [color="blue",penwidth=2]

  fp2 [label="!p"]
  fp2 -> d1 [color="red",label="1"]

  x2_o1 [label="!d"]
  x2_o1 -> x2i_o1 [color="blue",penwidth=2]

  d1 [label="Dup"]
  d1 -> f [color="red"]

  fp1 [label="!p"]
  fp1 -> d1 [color="red",label="1",arrowhead=odot]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret [color="blue",penwidth=2]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

5. The !d/!p pair removes a box; to track level we have renumber the levels of the contents of the box. Instead we retain the levels on the cut node. We also need to remember which side had the box, but in our example the higher side is always the box (this is not true in general because !d can renumber arbitrarily high).

.. graphviz::

  digraph {
  Root -> ret [color="red",penwidth=2]

  ret [label="I"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  c1b [label="Cut"]
  c1b -> d1 [color="red",label="1",penwidth=2]
  c1b -> x2i_o1 [color="blue",penwidth=2,label="0"]

  d1 [label="Dup",shape=doublecircle]
  d1 -> f [color="red"]

  fp1 [label="!p"]
  fp1 -> d1 [color="red",label="1",arrowhead=odot]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret [color="blue",penwidth=2]

  f [label="PiR"]
  f -> hi_retp /* h_d */ [color="blue",label="1"]
  f -> f_ret [color="red"]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

6. Next we move the dup down; this is duplicating the binder as discussed on page 29.

When it encounters the PiR, technically the Dup node grows in size and becomes a 4-input 2-output node. But since dot doesn't have enough edge labels for the monster node this is depicted in the graph as a series of 2-input 1-output Dup nodes linked with dashed lines.

.. graphviz::

  digraph {
  Root -> ret [color="red",penwidth=2]

  ret [label="I"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> f1 [color="red",label="1"]
  c1b -> x2i_o1 [color="blue",penwidth=2,label="0"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup"]
  d2 -> hi_retp [color="blue",label="1"]

  f1 [label="PiR"]
  f1 -> d1 [color="red"]
  f1 -> d2 [color="blue"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  x2i_o1 [label="PiL"]
  x2i_o1 -> x1_o1 /* x1_o1r */ [color="red"]
  x2i_o1 -> ret [color="blue",penwidth=2]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

7. On to the reduction in Figure 2.14 (4)-(5).

.. graphviz::

  digraph {
  Root -> ret [color="red",penwidth=2]

  ret [label="I"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  c1ba [label="Cut",shape=doublecircle]
  c1ba -> d1 [color="red",label="1"]
  c1ba -> ret [color="blue",penwidth=2,label="0"]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red"]

  d2 [label="Dup"]
  d2 -> hi_retp [color="blue",label="1"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  c2 [label="Cut"]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue"]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret /* f_retl */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

8

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> hi_retp [color="blue",label="1"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  c2 [label="Cut",shape=doublecircle]
  c2 -> o2 [color="red"]
  c2 -> o2l [color="blue",penwidth=2]

  o2 [label="PiR"]
  o2 -> x_o2 [color="blue"]
  o2 -> ret_o2 [color="red"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  o2l [label="PiL"]
  o2l -> hi_retp [color="red",label="1"]
  o2l -> f_ret [color="blue",penwidth=2]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

9. We are doing things a little out of order compared to Asperti; next is the reduction in Fig 2.11 (2)-(3).

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> hi_retp [color="blue",label="1"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  c2a [label="Cut",shape=doublecircle]
  c2a -> ret_o2 [color="red"]
  c2a -> f_ret [color="blue",penwidth=2]

  c2b [label="Cut"]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue"]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> ret_o2 /* ret_o2l */ [color="blue"]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  ret_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

10

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> hi_retp [color="blue",label="1"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  c2b [label="Cut",shape=doublecircle]
  c2b -> hi_retp [color="red",label="1"]
  c2b -> x_o2 [color="blue",penwidth=2]

  x_o2 [label="!c"]
  x_o2 -> x1_o2 [color="blue"]
  x_o2 -> x2_o2 [color="blue",penwidth=2]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue",penwidth=2]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  hi_retp [label="!p"]
  hi_retp -> hi_ret [color="red",label="2"]
  hi_retp -> h_di [color="blue",label="1"]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

11

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> xdic [color="blue"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  d3 -> d4 [dir=none,style=dashed,constraint=false]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red"]

  d4 [label="Dup"]
  d4 -> h_di [color="blue",label="1"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  c2b [label="Cut",shape=doublecircle]
  c2b -> hi_retpb [color="red",label="1"]
  c2b -> x2_o2 [color="blue",penwidth=2]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2"]
  hi_retpa -> d4 [color="blue",label="1"]

  hi_retpb [label="!p"]
  hi_retpb -> d3 [color="red",label="2",arrowhead=odot]
  hi_retpb -> d4 [color="blue",label="1",arrowhead=odot]

  xdic [label="!c"]
  xdic -> hi_retpa [color="blue",label="1"]
  xdic -> hi_retpb [color="blue",label="1"]

  x2_o2 [label="!d"]
  x2_o2 -> x2i_o2 [color="blue",penwidth=2]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

12

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> xdic [color="blue"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  d3 -> d4 [dir=none,style=dashed,constraint=false]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red",penwidth=2]

  d4 [label="Dup",shape=doublecircle]
  d4 -> h_di [color="blue",label="1",penwidth=2]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2",penwidth=2,arrowhead=odot]
  c2b -> x2i_o2 [color="blue",penwidth=2,label="1"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2"]
  hi_retpa -> d4 [color="blue",label="1"]

  xdic [label="!c"]
  xdic -> hi_retpa [color="blue",label="1"]
  xdic -> d4 [color="blue",label="1",arrowhead=odot]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  h_di [label="!d"]
  h_di -> h [color="blue",label="2",penwidth=2]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue",penwidth=2]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

13

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> xdic [color="blue"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  d3 -> d4 [dir=none,style=dashed,constraint=false]

  d3 [label="Dup"]
  d3 -> hi_ret [color="red",penwidth=2]

  d4 [label="Dup",shape=doublecircle]
  d4 -> h [color="blue",penwidth=2]

  h_di1 [label="!d"]
  h_di1 -> d4 [color="blue",label="2"]

  h_di2 [label="!d"]
  h_di2 -> d4 [color="blue",label="2",arrowhead=odot]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2",penwidth=2,arrowhead=odot]
  c2b -> x2i_o2 [color="blue",penwidth=2,label="1"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  xdic [label="!c"]
  xdic -> hi_retpa [color="blue",label="1"]
  xdic -> h_di2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  h [label="PiL"]
  h -> h_app /* h_appr */ [color="red"]
  h -> hi_ret /* hi_retl */ [color="blue",penwidth=2]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I"]
  f_ret [label="I"]

  }

14

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> xdic [color="blue"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  d3 -> d4a [dir=none,style=dashed,constraint=false]
  d4b -> d4a [dir=none,style=dashed,constraint=false]

  d3 [label="Dup",shape=doublecircle]
  d3 -> hi_ret [color="red",penwidth=2]

  d4a [label="Dup",shape=doublecircle]
  d4a -> hi_ret [color="blue",penwidth=2]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> d4a [color="blue"]

  hb [label="PiL"]
  hb -> d4b [color="red",arrowhead=odot]
  hb -> d4a [color="blue",arrowhead=odot]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  h_di2 [label="!d"]
  h_di2 -> hb [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  c2b [label="Cut"]
  c2b -> d3 [color="red",label="2",penwidth=2,arrowhead=odot]
  c2b -> x2i_o2 [color="blue",penwidth=2,label="1"]

  hi_retpa [label="!p"]
  hi_retpa -> d3 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  xdic [label="!c"]
  xdic -> hi_retpa [color="blue",label="1"]
  xdic -> h_di2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  hi_ret [label="I",shape=doublecircle]
  f_ret [label="I"]

  }

15

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup"]
  d2 -> xdic [color="blue"]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  hi_ret1 [label="I"]
  hi_ret2 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  hb [label="PiL"]
  hb -> d4b [color="red",arrowhead=odot]
  hb -> hi_ret2 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  h_di2 [label="!d"]
  h_di2 -> hb [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  c2b [label="Cut",shape=doublecircle]
  c2b -> hi_ret2 [color="red",label="2"]
  c2b -> x2i_o2 [color="blue",penwidth=2,label="1"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  xdic [label="!c"]
  xdic -> hi_retpa [color="blue",label="1"]
  xdic -> h_di2 [color="blue",label="1"]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  f_ret [label="I"]

  }

16

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 -> d2 [dir=none,style=dashed,constraint=false]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> d2 [color="blue",label="1"]

  d2 [label="Dup",shape=doublecircle]
  d2 -> xdic [color="blue",penwidth=2]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> d2 [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  hb [label="PiL"]
  hb -> d4b [color="red",arrowhead=odot]
  hb -> x2i_o2 [color="blue",penwidth=2,label="2->1"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  h_di2 [label="!d"]
  h_di2 -> hb [color="blue",label="2",penwidth=2]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  xdic [label="!c"]
  xdic -> hi_retpa [color="blue",label="1"]
  xdic -> h_di2 [color="blue",label="1",penwidth=2]

  x2i_o2 [label="PiL"]
  x2i_o2 -> x1_o2 /* x1_o2r */ [color="red"]
  x2i_o2 -> f_ret [color="blue",penwidth=2]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]
  f_ret [label="I"]

  }

17. ... 4 duplication steps later ...

.. graphviz::

  digraph {
  Root -> d1 [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  d1 [label="Dup"]
  d1 -> f_ret [color="red",penwidth=2]

  c1bb [label="Cut"]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> xdicf [color="blue",label="1"]

  xdicf [label="!c"]
  xdicf -> d2a [color="blue"]
  xdicf -> h_di2f [color="blue",label="1"]

  h_di2f [label="!d"]
  h_di2f -> hbf [color="blue",label="2"]

  hbf [label="PiL"]
  hbf -> d2b [color="red"]
  hbf -> x2i_o2f [color="blue",label="2->1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> d2d [color="blue"]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1"]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2"]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1"]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> d2d [color="blue",arrowhead=odot]

  d1 -> d2a [dir=none,style=dashed,constraint=false]
  d1 -> d2b [dir=none,style=dashed,constraint=false]
  d1 -> d2c [dir=none,style=dashed,constraint=false]
  d1 -> d2d [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]
  d2d [label="Dup",shape=doublecircle]
  d2d -> f_ret [color="blue",penwidth=2]

  f_ret [label="I",shape=doublecircle]

  f2 [label="PiR"]
  f2 -> d1 [color="red",arrowhead=odot]
  f2 -> xdicg [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

18

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> x1_o1 [color="blue"]

  c1bb [label="Cut",shape=doublecircle]
  c1bb -> x1_o1 /* x1_o1r */ [color="red",label="0"]
  c1bb -> xdicf [color="blue",label="1",penwidth=2]

  xdicf [label="!c"]
  xdicf -> d2a [color="blue"]
  xdicf -> h_di2f [color="blue",label="1",penwidth=2]

  h_di2f [label="!d"]
  h_di2f -> hbf [color="blue",label="2",penwidth=2]

  hbf [label="PiL"]
  hbf -> d2b [color="red"]
  hbf -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1"]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2"]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1"]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> f_retg [color="blue"]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I"]

  f2 [label="PiR"]
  f2 -> f_retg [color="red"]
  f2 -> xdicg [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o1 [label="I"]
  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

19

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut",shape=doublecircle]
  c1a -> fp1 [color="red",label="0"]
  c1a -> xdicf [color="blue",label="1",penwidth=2]

  xdicf [label="!c"]
  xdicf -> d2a [color="blue"]
  xdicf -> h_di2f [color="blue",label="1",penwidth=2]

  h_di2f [label="!d"]
  h_di2f -> hbf [color="blue",label="2",penwidth=2]

  hbf [label="PiL"]
  hbf -> d2b [color="red"]
  hbf -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1"]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2"]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1"]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> f_retg [color="blue"]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I"]

  f2 [label="PiR"]
  f2 -> f_retg [color="red"]
  f2 -> xdicg [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2 [color="red",label="1"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

20

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> fp2 [color="red",label="0"]
  c1b -> h_di2f [color="blue",label="1",penwidth=2]

  d1 [label="Dup"]
  d1 -> f2 [color="red"]

  fp1 [label="!p"]
  fp1 -> d1 [color="red",label="1"]

  fp2 [label="!p"]
  fp2 -> d1 [color="red",label="1",arrowhead=odot]

  h_di2f [label="!d"]
  h_di2f -> hbf [color="blue",label="2",penwidth=2]

  hbf [label="PiL"]
  hbf -> d2b [color="red"]
  hbf -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1"]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2"]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1"]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> f_retg [color="blue"]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I"]

  f2 [label="PiR"]
  f2 -> f_retg [color="red"]
  f2 -> xdicg [color="blue"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

21

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut"]
  c1b -> d1 [color="red",label="1",arrowhead=odot]
  c1b -> hbf [color="blue",label="2",penwidth=2]

  d1 [label="Dup",shape=doublecircle]
  d1 -> f2 [color="red"]

  fp1 [label="!p"]
  fp1 -> d1 [color="red",label="1"]

  hbf [label="PiL"]
  hbf -> d2b [color="red"]
  hbf -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1"]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2"]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1"]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> f_retg [color="blue"]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I"]

  f2 [label="PiR"]
  f2 -> f_retg [color="red"]
  f2 -> xdicg [color="blue"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

22

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b [label="Cut",shape=doublecircle]
  c1b -> f2b [color="red",label="1"]
  c1b -> hbf [color="blue",label="2",penwidth=2]

  d1a -> d1b [dir=none,style=dashed,constraint=false]

  d1a [label="Dup"]
  d1a -> f_retg [color="red"]

  d1b [label="Dup"]
  d1b -> xdicg [color="blue"]

  f2a [label="PiR"]
  f2a -> d1a [color="red"]
  f2a -> d1b [color="blue"]

  f2b [label="PiR"]
  f2b -> d1a [color="red",arrowhead=odot]
  f2b -> d1b [color="blue",arrowhead=odot]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  hbf [label="PiL"]
  hbf -> d2b [color="red"]
  hbf -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1"]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2"]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1"]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> f_retg [color="blue"]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

23

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b1 [label="Cut"]
  c1b1 -> d1a [color="red",arrowhead=odot,label=1,penwidth=2]
  c1b1 -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  c1b2 [label="Cut"]
  c1b2 -> d2b [color="red",label="2"]
  c1b2 -> d1b [color="blue",arrowhead=odot,label=1]

  d1a -> d1b [dir=none,style=dashed,constraint=false]

  d1a [label="Dup"]
  d1a -> f_retg [color="red",penwidth=2]

  d1b [label="Dup",shape=doublecircle]
  d1b -> xdicg [color="blue",penwidth=2]

  f2a [label="PiR"]
  f2a -> d1a [color="red"]
  f2a -> d1b [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  xdicg [label="!c"]
  xdicg -> d2a [color="blue",arrowhead=odot]
  xdicg -> h_di2g [color="blue",label="1",penwidth=2]

  h_di2g [label="!d"]
  h_di2g -> hbg [color="blue",label="2",penwidth=2]

  hbg [label="PiL"]
  hbg -> d2b [color="red",arrowhead=odot]
  hbg -> x2i_o2g [color="blue",label="2->1",penwidth=2]

  x2i_o2g [label="PiL"]
  x2i_o2g -> d2c [color="red",arrowhead=odot]
  x2i_o2g -> f_retg [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

24

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b1 [label="Cut"]
  c1b1 -> d1a [color="red",arrowhead=odot,label=1,penwidth=2]
  c1b1 -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  c1b2 [label="Cut"]
  c1b2 -> d2b [color="red",label="2"]
  c1b2 -> xdicg2 [color="blue",label=1]


  d1a -> d1b [dir=none,style=dashed,constraint=false]
  d1a -> d1c [dir=none,style=dashed,constraint=false]
  d1a -> d1d [dir=none,style=dashed,constraint=false]
  d1a -> d1e [dir=none,style=dashed,constraint=false]

  d1a [label="Dup",shape=doublecircle]
  d1a -> f_retg [color="red",penwidth=2]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> d1e [color="blue"]

  xdicg2 [label="!c"]
  xdicg2 -> d1b [color="blue",arrowhead=odot]
  xdicg2 -> h_di2g2 [color="blue",label="1"]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2"]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1"]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> d1e [color="blue",arrowhead=odot]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1e [label="Dup",shape=doublecircle]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> d2b [color="red",arrowhead=odot]
  d1d -> d2c [color="red",arrowhead=odot]
  d1e -> f_retg [color="blue",penwidth=2]

  f2a [label="PiR"]
  f2a -> d1a [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg [label="I",shape=doublecircle]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

25

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b1 [label="Cut",shape=doublecircle]
  c1b1 -> f_retg2 [color="red",label=1,penwidth=2]
  c1b1 -> x2i_o2f [color="blue",label="2->1",penwidth=2]

  c1b2 [label="Cut"]
  c1b2 -> d2b [color="red",label="2"]
  c1b2 -> xdicg2 [color="blue",label=1]


  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  xdicg2 [label="!c"]
  xdicg2 -> d1b [color="blue",arrowhead=odot]
  xdicg2 -> h_di2g2 [color="blue",label="1"]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2"]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1"]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> f_retg2 [color="blue"]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> d2b [color="red",arrowhead=odot]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]
  f_retg2 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut"]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

26

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b2 [label="Cut"]
  c1b2 -> d2b [color="red",label="2",penwidth=2]
  c1b2 -> xdicg2 [color="blue",label=1,penwidth=2]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  xdicg2 [label="!c"]
  xdicg2 -> d1b [color="blue",arrowhead=odot]
  xdicg2 -> h_di2g2 [color="blue",label="1",penwidth=2]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2",penwidth=2]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> d2b [color="red",arrowhead=odot]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot,penwidth=2]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> h_app [color="red",penwidth=2]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  c3 [label="Cut",shape=doublecircle]
  c3 -> i [color="red",label="2"]
  c3 -> h_app [color="blue",penwidth=2]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]
  h_app [label="I"]

  }

27

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b2 [label="Cut"]
  c1b2 -> d2b [color="red",label="2",penwidth=2]
  c1b2 -> xdicg2 [color="blue",label=1,penwidth=2]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  xdicg2 [label="!c"]
  xdicg2 -> d1b [color="blue",arrowhead=odot]
  xdicg2 -> h_di2g2 [color="blue",label="1",penwidth=2]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2",penwidth=2]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> d2b [color="red",arrowhead=odot]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup"]
  d2b -> d4b [color="red",arrowhead=odot,penwidth=2]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup",shape=doublecircle]
  d4b -> i [color="red",label="2"]

  ha [label="PiL"]
  ha -> d4b [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i [label="!p"]
  i -> i_i [color="red",label="3"]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

28

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b2 [label="Cut"]
  c1b2 -> d2b [color="red",label="2",penwidth=2]
  c1b2 -> xdicg2 [color="blue",label=1,penwidth=2]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  xdicg2 [label="!c"]
  xdicg2 -> d1b [color="blue",arrowhead=odot]
  xdicg2 -> h_di2g2 [color="blue",label="1",penwidth=2]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2",penwidth=2]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> d2b [color="red",arrowhead=odot]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]
  d2b [label="Dup",shape=doublecircle]
  d2b -> i2 [color="red"]
  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> i_i [color="red"]

  ha [label="PiL"]
  ha -> i1 [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i1 [label="!p"]
  i1 -> d4b [color="red",label="3"]

  i2 [label="!p"]
  i2 -> d4b [color="red",label="3",arrowhead=odot]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

29

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  c1b2 [label="Cut",shape=doublecircle]
  c1b2 -> i2 [color="red",label="2"]
  c1b2 -> xdicg2 [color="blue",label=1,penwidth=2]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  xdicg2 [label="!c"]
  xdicg2 -> d1b [color="blue",arrowhead=odot]
  xdicg2 -> h_di2g2 [color="blue",label="1",penwidth=2]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2",penwidth=2]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> i3 [color="red"]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> i_i [color="red"]

  ha [label="PiL"]
  ha -> i1 [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i1 [label="!p"]
  i1 -> d4b [color="red",label="3"]

  d2b [label="Dup"]
  d2b -> d4b [color="red"]

  i2 [label="!p"]
  i2 -> d2b [color="red",label="3"]

  i3 [label="!p"]
  i3 -> d2b [color="red",label="3",arrowhead=odot]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

30

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  dx [label="Dup"]
  dx -> d2b [color="red"]

  c1b2a [label="Cut"]
  c1b2a -> i2a [color="red",label="2"]
  c1b2a -> d1b [color="blue",label=1,arrowhead=odot]

  c1b2b [label="Cut",shape=doublecircle]
  c1b2b -> i2b [color="red",label="2"]
  c1b2b -> h_di2g2 [color="blue",label=1,penwidth=2]

  i2a [label="!p"]
  i2a -> dx [color="red",label="3"]

  i2b [label="!p"]
  i2b -> dx [color="red",label="3",arrowhead=odot]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  h_di2g2 [label="!d"]
  h_di2g2 -> hbg2 [color="blue",label="2",penwidth=2]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> i3 [color="red"]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup"]
  d4b -> i_i [color="red"]

  ha [label="PiL"]
  ha -> i1 [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i1 [label="!p"]
  i1 -> d4b [color="red",label="3"]

  d2b [label="Dup"]
  d2b -> d4b [color="red"]

  i3 [label="!p"]
  i3 -> d2b [color="red",label="3",arrowhead=odot]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

31

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  dx [label="Dup"]
  dx -> d2b [color="red",penwidth=2]

  c1b2a [label="Cut"]
  c1b2a -> i2a [color="red",label="2"]
  c1b2a -> d1b [color="blue",label=1,arrowhead=odot]

  c1b2b [label="Cut"]
  c1b2b -> dx [color="red",label="3",arrowhead=odot,penwidth=2]
  c1b2b -> hbg2 [color="blue",label="2",penwidth=2]

  i2a [label="!p"]
  i2a -> dx [color="red",label="3"]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> i3 [color="red"]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4b [label="Dup",shape=doublecircle]
  d4b -> i_i [color="red"]

  ha [label="PiL"]
  ha -> i1 [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i1 [label="!p"]
  i1 -> d4b [color="red",label="3"]

  d2b [label="Dup"]
  d2b -> d4b [color="red",penwidth=2]

  i3 [label="!p"]
  i3 -> d2b [color="red",label="3",arrowhead=odot]

  i_i [label="PiR"]
  i_i -> il [color="blue"]
  i_i -> i_ret [color="red"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

32

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  dx [label="Dup"]
  dx -> d2b [color="red",penwidth=2]

  c1b2a [label="Cut"]
  c1b2a -> i2a [color="red",label="2"]
  c1b2a -> d1b [color="blue",label=1,arrowhead=odot]

  c1b2b [label="Cut"]
  c1b2b -> dx [color="red",label="3",arrowhead=odot,penwidth=2]
  c1b2b -> hbg2 [color="blue",label="2",penwidth=2]

  i2a [label="!p"]
  i2a -> dx [color="red",label="3"]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> i3 [color="red"]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2b [dir=none,style=dashed,constraint=false]
  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4a -> d4b [dir=none,style=dashed,constraint=false]

  d4a [label="Dup"]
  d4a -> il [color="blue"]

  d4b [label="Dup"]
  d4b -> i_ret [color="red"]

  i_i1 [label="PiR"]
  i_i1 -> d4a [color="blue"]
  i_i1 -> d4b [color="red"]

  i_i2 [label="PiR"]
  i_i2 -> d4a [color="blue",arrowhead=odot]
  i_i2 -> d4b [color="red",arrowhead=odot]

  ha [label="PiL"]
  ha -> i1 [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i1 [label="!p"]
  i1 -> i_i1 [color="red",label="3"]

  d2b [label="Dup",shape=doublecircle]
  d2b -> i_i2 [color="red"]

  i3 [label="!p"]
  i3 -> d2b [color="red",label="3",arrowhead=odot]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

33

.. graphviz::

  digraph {
  Root -> f_retf [color="red",penwidth=2,label="0->1"]

  c1a [label="Cut"]
  c1a -> fp1 [color="red",label="0"]
  c1a -> d2a [color="blue",label="1"]

  dx [label="Dup",shape=doublecircle]
  dx -> i_i2 [color="red"]

  c1b2a [label="Cut"]
  c1b2a -> i2a [color="red",label="2"]
  c1b2a -> d1b [color="blue",label=1,arrowhead=odot]

  c1b2b [label="Cut"]
  c1b2b -> dx [color="red",label="3",arrowhead=odot,penwidth=2]
  c1b2b -> hbg2 [color="blue",label="2",penwidth=2]

  i2a [label="!p"]
  i2a -> dx [color="red",label="3"]

  d1b -> d1c [dir=none,style=dashed,constraint=false]
  d1b -> d1d [dir=none,style=dashed,constraint=false]

  xdicg1 [label="!c"]
  xdicg1 -> d1b [color="blue"]
  xdicg1 -> h_di2g1 [color="blue",label="1"]

  h_di2g1 [label="!d"]
  h_di2g1 -> hbg1 [color="blue",label="2"]

  hbg1 [label="PiL"]
  hbg1 -> d1c [color="red"]
  hbg1 -> x2i_o2g1 [color="blue",label="2->1"]

  x2i_o2g1 [label="PiL"]
  x2i_o2g1 -> d1d [color="red"]
  x2i_o2g1 -> f_retg1 [color="blue"]

  hbg2 [label="PiL"]
  hbg2 -> d1c [color="red",arrowhead=odot]
  hbg2 -> x2i_o2g2 [color="blue",label="2->1",penwidth=2]

  x2i_o2g2 [label="PiL"]
  x2i_o2g2 -> d1d [color="red",arrowhead=odot]
  x2i_o2g2 -> x2i_o2f [color="blue",penwidth=2]

  d1b [label="Dup"]
  d1c [label="Dup"]
  d1d [label="Dup"]
  d1b -> d2a [color="blue",arrowhead=odot]
  d1c -> i3 [color="red"]
  d1d -> d2c [color="red",arrowhead=odot]

  f2a [label="PiR"]
  f2a -> f_retg1 [color="red"]
  f2a -> xdicg1 [color="blue"]

  fp1 [label="!p"]
  fp1 -> f2a [color="red",label="1"]

  x2i_o2f [label="PiL"]
  x2i_o2f -> d2c [color="red"]
  x2i_o2f -> f_retf [color="blue",penwidth=2]

  d2a -> d2c [dir=none,style=dashed,constraint=false]

  d2a [label="Dup"]
  d2a -> hi_retpa [color="blue",label="1"]

  d2c [label="Dup"]
  d2c -> x1_o2 [color="red"]

  f_retf [label="I"]
  f_retg1 [label="I"]

  hi_ret1 [label="I"]

  d4a -> d4b [dir=none,style=dashed,constraint=false]

  d4a [label="Dup"]
  d4a -> il [color="blue"]

  d4b [label="Dup"]
  d4b -> i_ret [color="red"]

  i_i1 [label="PiR"]
  i_i1 -> d4a [color="blue"]
  i_i1 -> d4b [color="red"]

  ha [label="PiL"]
  ha -> i1 [color="red"]
  ha -> hi_ret1 [color="blue"]

  h_di1 [label="!d"]
  h_di1 -> ha [color="blue",label="2"]

  c2a [label="Cut"]
  c2a -> hi_retpa [color="red",label="1"]
  c2a -> x1_o2 [color="blue"]

  hi_retpa [label="!p"]
  hi_retpa -> hi_ret1 [color="red",label="2"]
  hi_retpa -> h_di1 [color="blue",label="1"]

  i1 [label="!p"]
  i1 -> i_i1 [color="red",label="3"]

  d2c -> d2d [dir=none,style=dashed,constraint=false]
  d2d -> d2e [dir=none,style=dashed,constraint=false]

  d2d [label="Dup"]
  d2d -> d4a [color="blue",arrowhead=odot]

  d2e [label="Dup"]
  d2e -> d4b [color="red",arrowhead=odot]

  i_i2 [label="PiR"]
  i_i2 -> d2d [color="blue"]
  i_i2 -> d2e [color="red"]

  i_i3 [label="PiR"]
  i_i3 -> d2d [color="blue",arrowhead=odot]
  i_i3 -> d2e [color="red",arrowhead=odot]

  i3 [label="!p"]
  i3 -> i_i3 [color="red",label="3"]

  il [label="!d"]
  il -> i_ret /* ild */ [color="blue"]

  x1_o2 [label="I"]
  i_ret [label="I"]

  }

So, an overview of the reduction:
1-2 Beta
5-6 Duplicate lambda
6-7 Beta
8-9 Beta
13-14 Duplicate app (correct)
16-17 Duplicate app x2 (too many)
- Duplicate lambda
- Beta
- Duplicate lambda
- Duplicate lambda
- Duplicate lambda

Compared to Asperti:
- Beta
- Beta
- Duplicate
- Beta
- Various duplications
- Duplicate
- Beta
- Beta
- Duplicate
- Beta
- Duplicate
- Beta