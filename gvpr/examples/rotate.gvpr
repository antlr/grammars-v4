/* Given node name and angle, rotate a layout using the given
 * node as origin.
 */

BEGIN {
  double x,y;
  double x0,y0;
  double x1,y1;
  double angle, cosa, sina;
  int cnt, sz;

  void rotate (double a, double b) {
    a -= x0;
    b -= y0;
    x1 = a*cosa - b*sina;
    y1 = a*sina + b*cosa;
  }
  char* rotateE (char* p) {
    char* newpos = "";
    cnt = sscanf (p, "e,%lf,%lf%n", &x, &y, &sz);
    if (cnt == 2) {
      rotate (x,y);
      newpos = newpos + sprintf ("e%lf,%lf ", x1, y1);
      p = substr(p, sz);
    }
    cnt = sscanf (p, "s,%lf,%lf%n", &x, &y, &sz);
    if (cnt == 2) {
      rotate (x,y);
      newpos = newpos + sprintf ("s%lf,%lf ", x1, y1);
      p = substr(p, sz);
    }

    while (sscanf (p, "%lf,%lf%n", &x, &y, &sz) == 2) {
      rotate (x,y);
      newpos = newpos + sprintf ("%lf,%lf ", x1, y1);
      p = substr(p, sz);
    }

    return newpos;
  }
}

BEG_G {
  node_t ctr = node ($, ARGV[0]);

  sscanf (ARGV[1], "%f", &angle);
  cosa = cos(angle);
  sina = sin(angle);

  sscanf (ctr.pos, "%f,%f", &x0, &y0);
  $.bb ="";
}
N {
  sscanf ($.pos, "%f,%f", &x, &y);
  rotate (x,y);
  $.pos = sprintf ("%f,%f", x1, y1);
}
E {
  $.pos = rotateE($.pos);
}
