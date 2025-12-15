// include isnan macro and export a non-macro version we can use
#include <math.h>

int double_isnan(double d) {
    return isnan(d);
}