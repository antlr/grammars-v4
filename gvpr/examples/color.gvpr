/* color edges based on normalized length alpha.
 * If we have lim[i-1] <= alpha <= lim[i], the color
 * is linearly interpolated between color[i-1] and color[i].
 * Edges already having a color attribute are left unchanged.
 */
BEGIN {
  double lens [edge_t];
  double maxlen, len, minlen=1.7976931348623157e+308;
  double alpha, x0, y0, x1, y1;
  double h0, s0, v0;
  int i;
 
  int ncolors;     /* number of colors: ncolors >= 2 */
  /* start of color i.
   * lim[0]=0 < lim[1] < ... < lim[ncolors-1]=1
   */
  double lim[int]; 
  /* HSV values for color i */
  double h[int];
  double s[int];
  double v[int];

  /* simple default */
  ncolors = 2;
  lim[0] = 0;
  lim[1] = 1;
  h[0] = 0;
  h[1] = 0.67;
  s[0] = 1;
  s[1] = 1;
  v[0] = 1;
  v[1] = 1;
}
E{
    sscanf ($.tail.pos, "%f,%f", &x0, &y0);
    sscanf ($.head.pos, "%f,%f", &x1, &y1);

    len = sqrt ((x0-x1)*(x0-x1) + (y0-y1)*(y0-y1));

    lens[$] = len;
    if (len > maxlen) maxlen = len;
    if (len < minlen) minlen = len;
}
BEG_G {
  if (!isAttr($,"E","color"))
    setDflt($,"E","color","");
}
E{
  if ($.color != "") return; 
  alpha = (lens[$]-minlen)/(maxlen-minlen);
  for (i = 1; i < ncolors; i++) {
    if (alpha < lim[i]) break;
  }
  alpha = (alpha - lim[i-1])/(lim[i] - lim[i-1]);
  h0 = (1-alpha)*h[i-1] + alpha*h[i];  
  s0 = (1-alpha)*s[i-1] + alpha*s[i];  
  v0 = (1-alpha)*v[i-1] + alpha*v[i];  
  $.color = sprintf ("%.02f %.02f %.02f", h0, s0, v0);
}
