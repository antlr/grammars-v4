/* Given graph processed by dijkstra, and node,
 * color shortest path
 * Assumes path has been computed by dijkstra
 */
BEG_G {
  node_t n = isNode($,ARGV[0]);
  node_t nxt;
  edge_t e;
  double d, totd = 0;

  if (n == NULL) {
    printf(2, "no node named \"%s\"\n", ARGV[0]);
    exit(1);
  }
  while (n.prev != "") {
	nxt = isNode($,n.prev);
    /* printf(2, "nxt \"%s\"\n", nxt.name); */
	e = isEdge (n, nxt, "");
    if (e == NULL) {
      printf(2, "no edge between %s and %s\n", n.name, nxt.name);
    }
    e.color = "blue";
    /* printf(2, "len %s\n", e.len); */
    /* sscanf (e.len, "%f", &d); */
	/* totd += d; */
    n = nxt;
  }
}
