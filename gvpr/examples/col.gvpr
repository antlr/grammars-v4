# test
/* color nodes using output of dijkstra */
BEG_G {
    double h, hn, hf, d, sat, md = maxdist; 
    if (hue_near =="") hue_near = 0.8;
    if (hue_far =="") hue_far = 1.2;
    hn = hue_near;
    hf = hue_far;
}
# test
N {
    d = dist;
    sat = (md - d + 1.0) /( md + 1.0);
    h = hn + ((hf - hn) * d)/md;
    while (h < 0.0) h = h + 1.0;
    while (h > 1.0) h = h - 1.0;
    color = sprintf("%lf %lf %lf",hue,sat,1.0);
      /* make sure the shape is filled */
    if (!match(style,"filled")>=0) {
      if (style != "") style=sprintf("%s,filled",style);
      else style="filled";
    }
}

