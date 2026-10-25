// Apply ++ and -- to subscript expressions, which are lvalues

// indices (static to prevent copy prop)
int i = 2;
int j = 1;
int k = 0;

int main(void) {
    int arr[3][2][2] = {
        {{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}, {{9, 10}, {11, 12}}};

    if (arr[i][j][k]++ != 11) {
        return 1;  // fail
    }
    if (arr[i][j][k] != 12) {
        return 2;  // fail
    }

    // also apply ++/-- to indices
    if (++arr[--i][j--][++k] /* arr[1][1][1] */ != 9) {
        return 3;  // fail
    }

    // check side effect of updating j
    if (arr[i][j][k] /* arr[1][0][1]*/ != 6) {
        return 4;  // fail
    }
    if (--arr[i][j][k] != 5) {
        return 5;  // fail
    }
    return 0;  // success
}