/* Make sure we can access an array declared in another translation unit */
extern long arr[4];
int double_each_element(void);

int main(void) {
    // check value of each array element
    for (int i = 0; i < 4; i = i + 1) {
        if (arr[i] != i + 1) {
            return i + 1;
        }
    }

    // update each element
    double_each_element();

    // check new values
    for (int i = 0; i < 4; i = i + 1) {
        if (arr[i] != (i + 1) * 2) {
            return i + 5;
        }
    }

    return 0;
}