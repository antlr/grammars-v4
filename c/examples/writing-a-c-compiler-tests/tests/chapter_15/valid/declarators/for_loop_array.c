/* Test that we can declare arrays in for loop initializers */
int main(void) {
    int counter = 0;

    // declare an array in for loop header, then check its values
    for (int i[3] = {1, 2, 3}; counter < 3; counter = counter + 1){
        if (i[counter] != counter + 1) {
            return 1;
        }
    }

    return 0;
}