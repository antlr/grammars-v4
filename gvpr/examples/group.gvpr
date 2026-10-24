/* Collapse all nodes with group = X into a single node */
BEG_G {
  node_t metaN; 
  graph_t g = graph ("metagraph", "S");
  $tvtype = TV_ne;
  $O = g;
}

  /* create only one node with given name/value */
N[group == "X"] {
  if (!metaN) {
    metaN = node (g, $.name);
  }
}

  /* duplicate all others */
N[group != "X"] { node (g, $.name); }

  /* Create an edge only if at least one of the nodes
   * is not a collapsed node */
E {
  node_t t;
  node_t h;
  if ($.tail.group == "X") { 
    if ($.head.group == "X") return;
    t = metaN;
    h = node (g, $.head.name);
  }
  else if ($.head.group == "X") {
    t = node (g, $.tail.name);
    h = metaN;
  }
  else {
    t = node (g, $.tail.name);
    h = node (g, $.head.name);
  }
  edge (t, h, "");
}

  /* set g to be output graph */
