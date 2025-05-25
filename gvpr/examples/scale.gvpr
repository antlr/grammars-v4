/* finds node n with root attribute 
 * finds distance minr of closest node
 * the layout is then scaled out from n so that 
 * a node is put on the smallest circle of radius x*minr
 * containing n
 */
BEG_G {
  node_t ctr;
  int cx, cy;
  int x, y;
  double delx, dely;
  int newx, newy;
  node_t n;
  edge_t e;
  int i, sc, d, mind = -1;
  double fact, newr, ang, minr;
  
  ctr = node($,aget($,"root"));
  sscanf (ctr.pos, "%d,%d", &cx, &cy);
  for (e = fstedge(ctr); e; e = nxtedge(e, ctr)) {
    if (e.head == ctr) n = e.tail;
    else n = e.head;
    sscanf (n.pos, "%d,%d", &x, &y);
    d = (x-cx)*(x-cx) + (y-cy)*(y-cy);
    if ((mind == -1) || (d < mind)) mind = d;
  } 
  minr = (int)sqrt((double)mind);
}

N [$ != ctr] {
  
    sscanf ($.pos, "%d,%d", &x, &y);
    dely = y - cy;
    delx = x - cx;
    d = delx*delx + dely*dely;
    sc = (int)sqrt((double)(d/mind));
    if (sc > 1) {
      fact = 2.0;
      for (i=1; i<sc-1;i++) fact *= 2.0;
      newr = minr*(2.0 - (1.0/fact));
      ang = atan2 (dely, delx);
      newx = newr*cos(ang) + cx; 
      newy = newr*sin(ang) + cy; 
      $.pos = sprintf ("%d,%d", newx, newy);
    }
}
