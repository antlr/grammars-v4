/* Assuming nodes have been positioned by dot, this adds a rank
 * attribute by placing all nodes with the same y value on a specific
 * integer rank.
 * If the graph has rankdir=LR, x and y are flipped.
 */
BEG_G {
  double x,y;
  int lv[double];
  int r, rk[double];
  int flip;
  if (isAttr($,"G","rankdir") && $.rankdir=="LR") flip = 1;
}
N {
  sscanf($.pos,"%f,%f",&x,&y);
  if (flip) lv[x] = 1;
  else lv[y] = 1;
}
BEG_G {
  r = 0;
  if (flip)
    forr (lv[x]) {
      rk[x] = r++;
      /* printf (2, "rk[%f] = %d\n", y, rk[y]); */
    }
  else
    forr (lv[y]) {
      rk[y] = r++;
      /* printf (2, "rk[%f] = %d\n", y, rk[y]); */
    }
}
N {
  sscanf($.pos,"%f,%f",&x,&y);
  /* printf(2, "node %s y %f rk %d\n", $.name, y, rk[y]); */
  if (flip) $.rank = sprintf("%d", rk[x]); 
  else $.rank = sprintf("%d", rk[y]); 
}
