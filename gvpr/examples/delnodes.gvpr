/* Delete nodes whose names are given in ARGV */
BEG_G {
  int names[char*];
  int nodes[node_t];
  node_t n;
  int i;

  for (i = 0; i < ARGC; i++)
    names[ARGV[i]] = 1;
}
N[names[name]]{nodes[$] = 1}
END_G {
  for (nodes[n])
    delete ($,n);
}
