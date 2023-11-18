/* Takes a graph laid out by twopi and adds rings.
 * Assumes ARGV[] = "root" "=" <rootname>, as output by twopi -v.
 * Usage:
 *   twopi -v foo.dot > out 2> log
 *   gvpr -f addrings.g -a"`grep root log`" out | neato -n2 ...
 */
BEG_G {
  graph_t og;
  edge_t e;
  node_t ctr = node($, ARGV[0]);
  double rs = 1.0; /* min. slack between the squares of two consecutive radii */
  int cx, cy;
  int x, y;
  node_t n;
  int i, n_r;
  int d;
  int rads[int];
  char* ctr_s = ctr.pos;
  sscanf (ctr_s, "%d,%d", &cx, &cy);
  if (hasAttr($, "ranksep")) {
    sscanf ($.ranksep, "%f", &rs);
    if (rs == 0.0) rs = 1.0;
  }
  rs *= 72;
  rs = 1.5*rs*rs;
}
N [$ != ctr] {
  sscanf ($.pos, "%d,%d", &x, &y);
  d = (x-cx)*(x-cx) + (y-cy)*(y-cy);
  for (rads[i]) {
    if ((rads[i]-rs <= d) && (d <= rads[i]+rs)) return;
  }
  n_r++;
  rads[n_r] = d;
}
END_G {
  og = copy (NULL, $);
  og.outputorder = "nodesfirst";
  setDflt (og, "N", "label", "\\N");
  for (rads[i]) {
    n = node(og, "ring_"+((string)i));
    n.shape = "circle";
    n.pos = ctr_s;
    n.style = "";
    n.label = "";
    d = rads[i];
    n.width = sprintf("%f", sqrt(d)/36.0);
  }
  for (n=fstnode($);n;n = nxtnode(n))
    clone (og, n);
  for (n=fstnode($);n;n = nxtnode(n))
    for (e=fstedge(n);e;e = nxtedge(e,n))
      clone (og, e);
  write(og);
  
}
