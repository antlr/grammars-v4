/* finds root node of graph.
 * scales the x and y position of all other nodes using, first,
 * ARGV[0], or $G.scale.
 * 
 * The expected syntax is "x,y" where at least one of x or y must
 * be given. If only one is given, the other is taken as 1.
 */
BEGIN {
  double scalex, scaley;
  int r, done;

  int setScale (char* s) 
  {
    if ((sscanf (s, ",%f",&scaley))) {
        scalex = 1;
        return 1;
    }
    else {
      r = sscanf (s, "%f,%f",&scalex,&scaley);
      if (r) {
        if (r == 1) scaley = 1;
        return 1;
      }
    }
    return 0;
  }

}

BEG_G {
  node_t ctr = node($,aget($,"root"));
  double cx, cy, x, y, delx;
  
  /* get scale argument */
  done = 0;
  if (ARGC == 1)
    done = setScale (ARGV[0]);
  if (!done && isAttr($,"G","scale"))
    done = setScale ($.scale);
  if (!done)
    scalex = scaley = 1.0;

  if ((scalex == 1.0) && (scaley == 1.0))
    exit (0);

  $.bb = "";
  sscanf (ctr.pos, "%f,%f", &cx, &cy);
}
N [$ != ctr] {
    sscanf ($.pos, "%f,%f", &x, &y);
    delx = scalex*(x - cx) + cx;
    dely = scaley*(y - cy) + cy;
    $.pos = sprintf ("%f,%f", delx, dely);
}
