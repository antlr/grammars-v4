/* Make sure a called function can correctly access variables on the stack */
int f(int reg1, int reg2, int reg3, int reg4, int reg5, int reg6,
    int stack1, int stack2, int stack3) {
    int x = 10;
    // make sure every variable has the right value
    if (reg1 == 1 && reg2 == 2 && reg3 == 3 && reg4 == 4 && reg5 == 5
        && reg6 == 6 && stack1 == -1 && stack2 == -2 && stack3 == -3
        && x == 10) {
        // make sure we can update the value of one argument
        stack2 = 100;
        return stack2;
    }
    return 0;
}