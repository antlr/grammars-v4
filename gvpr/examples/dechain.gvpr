/* Remove peninsulas - chains hanging off the main graph */
BEGIN {
  edge_t  e;
  node_t  v, n;
}
N [degree == 1] {
  n = $;
  while (n.degree == 1) {
    e = fstedge (n);
    if (e.head == n) v = e.tail;
    else v = e.head;
    delete($G,n);
    n = v;
  }  
}
