/* If we can determine the value of a switch's controlling expression at
 * compile time, we can eliminate the whole switch statement except the path
 * that will actually be taken. In this case, this lets us reduce the whole
 * function to a single return statement
 */

int callee(void){
    return 0;
}

int target(void) {
    int switch_var = 10;
    int retval = -1;
    switch(switch_var) {
        case 1:
            callee();
            return 1;
        case 2:
            retval = -2;
            break;
        case 10: // case we'll actually take
            retval = 0;
            break;
        default:
            retval = 1000;
            break;
    }
    return retval;
}

int main(void) {
    return target();
}