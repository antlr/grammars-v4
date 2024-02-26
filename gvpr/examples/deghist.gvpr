/* print histogram of node degrees */
BEGIN {
  int degrees[];
  int maxd = 0;
  int i, d;
  char* maxn;
}
N{ 
  degrees[degree]++;
  if (degree > maxd) {
    maxn = $.name;
    maxd = degree;
  }
}
END {
  printf ("max node %s (%d)\n", maxn, maxd);
  for (i = 1; i <= maxd; i++) {
    d = degrees[i];
    if (d > 0) printf ("[%d] %d\n", i, d);
  }
}
