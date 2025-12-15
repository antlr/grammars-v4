double fifty_fiveE5 = 55e5;
double fifty_fourE4 = 54e4;
double tiny = .00004;
double four = 4.;
double point_one = 0.1;

/* Test comparisons on doubles - evaluate true and false case for each comparison operator */
int main(void) {

    /* false comparisons */
    if (fifty_fiveE5 < fifty_fourE4) {
        return 1;
    }

    if (four > 4.0) {
        return 2;
    }

    if (tiny <= 0.0) {
        return 3;
    }

    if (fifty_fourE4 >= fifty_fiveE5) {
        return 4;
    }

    if (tiny == 0.0) {
        return 5;
    }

    if (point_one != point_one) {
        return 6;
    }

    /* true comparisons */

    if (!(tiny > 00.000005))  {
        return 7;
    }

    if (!(-.00004 < four)) {
        return 8;
    }

    if (!(tiny <= tiny)) {
        return 9;
    }

    if (!(fifty_fiveE5 >= fifty_fiveE5)) {
        return 10;
    }

    if (!(0.1 == point_one)) {
        return 11;
    }

    if (!(tiny != .00003)) {
        return 12;
    }

    /* try comparing two constants */
    if (0.00003 < 0.000000000003) {
        return 13;
    }

    return 0;

}