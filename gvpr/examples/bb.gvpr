/* computes the bounding box of a graph based on its nodes 
   taking into account clusters and node sizes.
 */
BEGIN {
  double x, y, w2, h2;
  double llx, lly, urx, ury;
  double llx0, lly0, urx0, ury0;

  graph_t clustBB (graph_t G) {
    graph_t sg;
    for (sg = fstsubg(G); sg; sg = nxtsubg(sg)) {
	  sg = clustBB(sg);
    }
	if (G.name == "cluster*") {
      sscanf (G.bb, "%lf,%lf,%lf,%lf", &llx0, &lly0, &urx0, &ury0);
      if (llx0 < llx) llx = llx0;
      if (lly0 < lly) lly = lly0;
      if (urx0 > urx) urx = urx0;
      if (ury0 > ury) ury = ury0;
    }
    return G;
  }
}
BEG_G {
  llx = 1000000; lly = 1000000; urx = -1000000; ury = -1000000;
}
N {
  sscanf ($.pos, "%lf,%lf", &x, &y);
  w2 = (36.0*(double)$.width);
  h2 = (36.0*(double)$.height);
  if ((x - w2) < llx) llx = x - w2;
  if ((x + w2) > urx) urx = x + w2;
  if ((y - h2) < lly) lly = y - h2;
  if ((y + h2) > ury) ury = y + h2;
}
END_G {
  clustBB ($);
  $.bb = sprintf ("%lf,%lf,%lf,%lf", llx, lly, urx, ury);
}
