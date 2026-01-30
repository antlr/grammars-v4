/* Make sure we can eliminate redundant copies where source is NaN
 * (which requires us to compare NaN values appropriately and recognize when
 * they're the same even though NaN doesn't compare equal to itself).
 * We should be able to eliminate all control-flow instructions from target
 */

int double_isnan(double d); // defined in tests/chapter_13/helper_libs/nan.c

double na;

int target(int flag) {
    na = 0.0 / 0.0;
    double d = 0.0 / 0.0;
    if (flag) {
        na = d; // same value it already is; can delete this
    }
    return 0;
}

int main(void) {
    target(1);
    if (!double_isnan(na)) {
        return 1; // fail
    }

    return 0;
}