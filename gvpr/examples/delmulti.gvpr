/* create a copy of the input graph with no multiedges */
BEG_G { 
  graph_t g = graph ("merge", "S");
} 
E {
  int wt;
  node_t h = node(g,$.head.name);
  node_t t = node(g,$.tail.name);
  edge_t e = isEdge(t,h,"");
  wt = $.weight;
  if (wt <= 0) wt = 1;
  if (e) {
    e.weight = e.weight + wt;
  }
  else if (h != t) {
    e = edge(t,h,"");
    e.weight = wt;
  }
}
END_G { 
  fwriteG(g,1);
}
