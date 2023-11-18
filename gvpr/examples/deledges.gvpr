/* delete all edges */
BEGIN {
  int map[edge_t];
  edge_t e;
}
E {map[$]=1}
END_G {
  for (map[e]) delete ($,e);
}
