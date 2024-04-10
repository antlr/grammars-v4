/* Convert a rooted tree to a hierarchy of clusters for patchwork.
 * ARGV[0] is desired root
 */
BEG_G {
  node_t rt;
  node_t n;
  graph_t cg;
  graph_t sg;
  int depth;
  int mark[node_t];
  graph_t stk[int];

  if (! $.directed) {
    printf(2,"Input graph is not directed\n");
    exit (1);
  }
  rt = isNode($,ARGV[0]);
  if (rt == NULL) {
    printf(2,"Root node \"%s\" not found\n", ARGV[0]);
    exit (1);
  }
  $tvroot = rt;
  $tvtype = TV_prepostfwd;
  cg = graph(rt.name,"U");
}

N {
  if (mark[$]) {
    depth--;
  }
  else {
    mark[$] = 1;
    if (depth > 0) {
      if (fstout($)) {
        sg = subg(stk[depth-1], "cluster_" + $.name); 
        if (($.style == "filled") && ($.fillcolor != ""))
          sg.bgcolor = $.fillcolor;
      }
      else {
        sg = NULL;
        n = node(stk[depth-1], $.name);
        n.style = "filled";
        n.fillcolor = $.fillcolor;
      }
    }
    else sg = cg;
    stk[depth] = sg;
    depth++;
  }
}
END_G {
  write(cg);
}

