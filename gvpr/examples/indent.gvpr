/* Print the depth-first traversal of nodes
 * as an indented list
 */
BEGIN {
  int i, indent;
  int seen[string];
  void prInd () {
    for (i = 0; i < indent; i++) printf ("  ");
  } 
}
BEG_G {

   $tvtype = TV_prepostfwd;
   $tvroot = node($,ARGV[0]);
}
N {
  if (seen[$.name]) indent--;
  else {
    prInd();
      print ($.name);
    seen[$.name] = 1;
    indent++;
  }
}
