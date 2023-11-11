/* Collapse all edges with same group attribute into a single edge */
BEG_G {
  int seen[string];
  $O = $;            // Use the input graph as output.
}
E {
  if (collapse == "") return;          // If no collapse, ignore.
  if (seen[collapse]) delete ($G, $);  // If already seen an edge with this collapse value, 
                                       // delete the edge.
  else seen[collapse] = 1;             // Else mark collapse value as seen and keep edge.
}
