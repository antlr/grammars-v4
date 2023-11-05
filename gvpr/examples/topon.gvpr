/* Generate copy of topology of input graph
 * Replace names with numbers
 */
BEGIN {
int id = 0;
char* names[char*];

char* mapn (char* inname)
{
   char* s = names[inname];
   if (s == "") {
     s = id++;
     names[inname] = s;
   }
   return s;
}

}

BEG_G {
  graph_t g = graph ($.name, "U");
}
N { node (g, mapn($.name)); }
E { edge (node (g, mapn($.tail.name)), node (g, mapn($.head.name)), ""); }
END_G {
  write (g);
}
