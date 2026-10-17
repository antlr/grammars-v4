/* Report the distance from src = ARGV[0] to dst = ARGV[1]
 */
BEG_G {
  int dist[node_t];
  node_t n, curn;
  node_t src = node($G, ARGV[0]);
  node_t dst = node($G, ARGV[1]);
  $tvroot = src;
  $tvtype = TV_bfs;
}

N {
  curn = $;
  if ($ == dst) {
    printf ("dist from %s to %s is %d\n", src.name, dst.name, dist[dst]);
    exit(0);
  }
}

E {
  if ($.head == curn) n = $.tail; 
  else n = $.head;
  if (dist[n] == 0) dist[n] = dist[curn]+1;
}
