BEGIN {
  int visited[node_t];

  int visit(node_t n, edge_t e) {
    if (visited[n] == 0) {
      visited[n] = 1;

      for (e = fstin(n); e; e = nxtin(e)) {
        visit(e.tail, NULL);
      }

      printf("%s\n", n.name);
    }

    return 0;
  }
}

N {
  visit($, NULL);
}