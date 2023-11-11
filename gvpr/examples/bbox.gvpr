/* computes the bounding box of a graph based on node positions */
BEGIN {
  double xmin, ymin, xmax, ymax;
  xmin = ymin = 1.7976931348623157e+308;
  xmax = ymax = -ymin;
  double x, y;
}
N {
  if (sscanf ($.pos, "%f,%f", &x, &y) == 2) {
    if (x < xmin) xmin = x;
    if (y < ymin) ymin = y;
    if (x > xmax) xmax = x;
    if (y > ymax) ymax = y;
  }
}

END {
  printf ("(%f,%f) (%f,%f)\n", xmin,  ymin, xmax, ymax);
  if (ARGC) printf ("area = %f aspect = %f\n", ((xmax-xmin)*(ymax-ymin))/1000000., (xmax-xmin)/(ymax-ymin));
}
