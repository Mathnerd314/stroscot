{-
Several serial algorithms based on depth-first search compute strongly connected components in linear time. They all run in Θ(V+E) asymptotic time for an adjacency list of V vertices and E edges. With an adjacency matrix representation all entries must be scanned so it is Ο(V^2). The space representation is also very similar, they use stacks for the search and for the in-progress component, in the worst case requiring space for all vertices. However Kosaraju is slower in practice because it does two passes.

* Kosaraju's algorithm uses two passes. First a depth-first search in the original graph is used to order the vertices. The second BFS or DFS is on the transpose graph and finds SCCs one at a time.
* Tarjan's algorithm uses a single pass of depth-first search and a stack of vertices that have been explored by the search but not yet assigned to a component. It calculates "low numbers" of each vertex - the index number of the highest ancestor reachable in one step from a descendant of the vertex.
* The path-based algorithm uses a single pass of depth-first search and two stacks. The first stack tracks vertices not yet assigned to components, while the second stack tracks current path in the depth-first search tree. It only uses the numbering found by the depth-first search, with no "low number" construction, hence is a little simpler than Tarjan.

The path-based algorithm is supposedly an improvement over Tarjan, but Tarjan is "the definitive serial algorithm".

There are also parallel algorithms:

* The forward-backward algorithm picks a random vertex and computes the predecessor and descendant sets. These can be computed with a parallel BFS. The intersection is an SCC, and any remaining SCCs must lie in entirely in the predecessor set, entirely in the descendant set, or in the remainder of the graph, so these three sets can be recursed on in parallel. In a graph of bounded degree this has serial cost O(n log n).


Trimming vertices of in or out degree zero 



The depth-first search can be replaced with a breadth-first or A* search.
-}

dfs v =
  preOrder[v] = C++
  order.push(v)
  for (w of edges(v)) {
    if(preOrder[w] == undefined)
      dfs w

dfs_all =
  C = mut 0 // counter of the number of vertices reached so far, used to compute the preorder numbers of the vertices.
  preOrder = replicate (length nodes) undefined
  order = []
  for (let i = 0; i < nodes.length; i++) {
    if (preOrder[i] == undefined) dfs i
  }


scc (nodes: [[Node]]): Node[][] {
  const index: Record<string, number> = {}
  for (let i = 0; i < nodes.length; i++) {
    index[nodes[i].id] = i
  }
  edges = \(v: number) -> nodes[v].links.map(l => index[l.target])
  const done: boolean[] = []
  const S: number[] = [] // vertices that have not yet been assigned to a strongly connected component, in the order in which the depth-first search reaches the vertices.
  const P: number[] = [] // vertices that have not yet been determined to belong to different strongly connected components from each other.
  const components: number[][] = []

  dfs_all

  function process(v: number) {
    // 1. Set the preorder number of v to C, and increment C.
    preOrder[v] = C++
    // 2. Push v onto S and also onto P.
    S.push(v)
    P.push(v)
    // 3. For each edge from v to a neighboring vertex w:
    for (const w of edges(v)) {
      // If the preorder number of w has not yet been assigned, recursively search w;
      if (preOrder[w] === undefined) process(w)
      // Otherwise, if w has not yet been assigned to a strongly connected component:
      // Repeatedly pop vertices from P until the top element of P has a preorder number
      // less than or equal to the preorder number of w.
      if (!done[w]) {
        while (preOrder[P[P.length - 1]] > preOrder[w]) {
          P.pop()
        }
      }
    }
    // 4. If v is the top element of P:
    if (v === P[P.length - 1]) {
      // Pop vertices from S until v has been popped, and assign the popped vertices to a new component.
      const component: number[] = []
      do {
        const x = S.pop()!
        done[x] = true
        component.push(x)
      } while (component[component.length - 1] !== v)
      components.push(component)
      // Pop v from P.
      P.pop()
    }
  }
  return components.map(c => c.map(x => nodes[x]))
}
