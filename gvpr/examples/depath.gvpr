/* Replace paths a -> b -> ... -> c with a -> c */
BEGIN {
  edge_t  e;
  node_t  n, prv, nxt;
}

N [(indegree == 1) && (outdegree == 1)] {
  e = fstin ($);
  prv = e.tail;
  e = fstout ($);
  nxt = e.head;
  delete ($G,$);

  while ((prv.indegree == 1) && (prv.outdegree == 0)) {
    e = fstin (prv);
    n = e.tail;
    delete ($G,prv);
    prv = n;
  }

  while ((nxt.indegree == 0) && (nxt.outdegree == 1)) {
    e = fstout (nxt);
    n = e.head;
    delete ($G,nxt);
    nxt = n;
  }
 
  if (!isEdge (prv,nxt,""))
    edge (prv,nxt,"");
  
}
