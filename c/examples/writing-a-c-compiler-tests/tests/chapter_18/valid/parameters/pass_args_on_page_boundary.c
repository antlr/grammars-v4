/* Test that we don't read past the bounds of a structure when passing it as a
 * parameter: pass a structure parameter that ends at the end
 * of a page, where the next page isn't mapped. If we read past the end of the
 * structure we'll trigger a memory access violation and crash the program.
 * */

// structure type is two eightbytes
struct nine_bytes {
    char arr[11];
};

// irregularly-sized struct that's right on a page boundary,
// defined in data_on_page_boundary_<PLATFORM>.s
extern struct nine_bytes on_page_boundary;

int f(struct nine_bytes in_reg, int a, int b, int c, int d, int e,
      struct nine_bytes on_stack) {
    // validate structs
    for (int i = 0; i < 9; i = i + 1) {
        char in_reg_c = in_reg.arr[i];
        char on_stack_c = on_stack.arr[i];
        if (i == 2) {
            // on_page_boundary[2] == 4
            if (in_reg_c != 4 || on_stack_c != 4) {
                return 1;
            }
        } else if (i == 3) {
            // on_page_boundary[3] == 5
            if (in_reg_c != 5 || on_stack_c != 5) {
                return 2;
            }
        } else if (i == 8) {
            // on_page_boundary[8] == 6
            if (in_reg_c != 6 || on_stack_c != 6) {
                return 3;
            }
        } else {
            // all other array elements are 0
            if (in_reg_c || on_stack_c) {
                return 4;
            }
        }
    }

    // validate other args
    if (a != 101 || b != 102 || c != 103 || d != 104 || e != 105) {
        return 5;
    }

    return 0;  // success
}

int main(void) {
    on_page_boundary.arr[2] = 4;
    on_page_boundary.arr[3] = 5;
    on_page_boundary.arr[8] = 6;
    // pass this struct in register and on stack
    return f(on_page_boundary, 101, 102, 103, 104, 105,
             on_page_boundary);  // 0 is success
}