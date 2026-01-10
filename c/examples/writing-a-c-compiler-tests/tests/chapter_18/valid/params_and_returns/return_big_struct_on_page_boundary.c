/* Test that we don't read past the bounds of a structure when passing it as a
 * return value: return a structure that ends at the end
 * of a page, where the next page isn't mapped. If we read past the end of the
 * structure we'll trigger a memory access violation and crash the program.
 * This test is similar to return_struct_on_page_boundary except the struct is
 * large enough to be passed in memory instead of registers
 * */

struct eighteen_bytes {
    char arr[18];
};

// irregularly-sized struct that's right on a page boundary,
// defined in big_data_on_page_boundary_<PLATFORM>.s
extern struct eighteen_bytes on_page_boundary;

struct eighteen_bytes return_struct(void) {
    on_page_boundary.arr[17] = 12;
    on_page_boundary.arr[9] = -1;
    on_page_boundary.arr[8] = -2;
    on_page_boundary.arr[7] = -3;
    return on_page_boundary;
}

int main(void) {
    // call function that returns on_page_boundary
    struct eighteen_bytes x = return_struct();

    // validate it
    for (int i = 0; i < 18; i = i + 1) {
        char val = x.arr[i];
        if (i == 7) {
            if (val != -3) {
                return 1;
            }
        } else if (i == 8) {
            if (val != -2) {
                return 2;
            }
        } else if (i == 9) {
            if (val != -1) {
                return 3;
            }
        } else if (i == 17) {
            if (val != 12) {
                return 4;
            }
        } else if (x.arr[i]) {  // all other elements are 0
            return 5;
        }
    }

    return 0;  // success
}