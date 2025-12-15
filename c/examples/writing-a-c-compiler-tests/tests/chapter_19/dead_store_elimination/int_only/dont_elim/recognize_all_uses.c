/* Make sure we recognize all the different ways a variable
 * can be used/generated (in Unary, Binary, JumpIfZero, etc.) */

int test_jz(int flag, int arg) {
    if (flag) {
        arg = 0;  // this store is not dead b/c arg is used later;
                  // put it in an if statement so we don't propagate 0 into
                  // return statement
    }
    return arg ? 1 : 2;
}

int test_jnz(int flag, int arg) {
    if (flag) {
        arg = 0;
    }
    return arg || 0;
}

int test_binary(int flag, int arg1, int arg2) {
    if (flag == 0) {
        arg1 = 4;  // this store is not dead b/c arg is used later;
                   // put it in an if statement so we don't propagate 4 into
                   // return statement
    } else if (flag == 1) {
        arg2 = 3;  // also not a dead store
    }
    return arg1 * arg2;  // generates arg1 and arg2
}

int test_unary(int flag, int arg) {
    if (flag) {
        arg = 5;  // this store is not dead b/c arg is used later;
                  // put it in an if statement so we don't propagate 5 into
                  // return statement
    }
    return -arg;  // generates arg
}

int f(int arg) {
    return arg + 1;
}

int test_funcall(int flag, int arg) {
    if (flag) {
        arg = 7;  // this store is not dead b/c arg is used later;
                  // put it in an if statement so we don't propagate 7 into
                  // return statement
    }
    return f(arg);
}

int main(void) {
    if (test_jz(1, 1) != 2) {  // 0 ? 1 : 2
        return 1; // fail
    }
    if (test_jz(0, 1) != 1) {  // 1 ? 1 : 2
        return 2; // fail
    }
    if (test_jnz(1, 1) != 0) {  // 0 || 0
        return 3; // fail
    }
    if (test_jnz(0, 1) != 1) {  // 1 || 1
        return 4; // fail
    }
    if (test_binary(0, 8, 9) != 36) {  // 4 * 9
        return 5; // fail
    }
    if (test_binary(1, 8, 9) != 24) {  // 8 * 3
        return 6; // fail
    }
    if (test_binary(2, 8, 9) != 72) {  // 8 * 9
        return 7; // fail
    }
    if (test_unary(0, 8) != -8) {
        return 8;  // fail
    }
    if (test_unary(1, 8) != -5) {
        return 9;  // fail
    }
    if (test_funcall(1, 5) != 8) {  // f(7) => 7 + 1
        return 10; // fail
    }
    if (test_funcall(0, 9) != 10) {  // f(9) ==> 9 + 1
        return 11; // fail
    }
    return 0;
}