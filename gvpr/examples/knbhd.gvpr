/* knbhd - Return the k-neighborhood of a node, i.e., allnodes
 * whose path length from the given node is <= k.
 * ARGV[] = k node_name
 */
BEG_G {
  node_t  ctr;
  int     maxlen;

  graph_t comp = subg($, "kcomp");
  int     sid = 0, eid = 0;
  int     curlen;
  node_t  curnode;
  int     nlen[node_t];
  node_t  stk[int];
  node_t  other;
  edge_t  e;

  if (ARGC != 2) {
    printf (2, "Two arguments required\n");
    exit(1);
  }

  if (!sscanf(ARGV[0],"%d",&maxlen)) {
    printf (2, "Improper length parameter \"%s\"\n", ARGV[0]);
    exit(1);
  }
  maxlen++; /* length of 0 means unset */

  ctr = isNode ($, ARGV[1]);
  if (!ctr) {
    printf (2, "node %s not found\n", ARGV[1]);
    exit(1);
  }

  subnode (comp,ctr);
  nlen[ctr] = 1;
  curnode = ctr;
  while (curnode) {
    curlen = nlen[curnode];
    if (curlen == maxlen) break;
    
    for (e = fstedge(curnode); e; e = nxtedge(e,curnode)) {
      other = e.head;
      if (other == curnode) other = e.tail;
      if (nlen[other]) continue; /* already seen */
      subnode(comp,other);
      nlen[other] = curlen+1;
      stk[eid++] = other;
    }

    if (sid < eid) curnode = stk[sid++];
    else curnode = NULL;
  }
  
  induce(comp);
  write(comp);
}
