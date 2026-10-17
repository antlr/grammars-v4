/* Test that we don't read past the bounds of a structure when passing it as a
 * return value: return a structure that ends at the end
 * of a page, where the next page isn't mapped. If we read past the end of the
 * structure we'll trigger a memory access violation and crash the program.
 * */

struct ten_bytes {
    char arr[10];
};

// irregularly-sized struct that's right on a page boundary,
// defined in data_on_page_boundary_<PLATFORM>.s
extern struct ten_bytes on_page_boundary;

struct ten_bytes return_struct(void) {
    on_page_boundary.arr[9] = -1;
    on_page_boundary.arr[8] = -2;
    on_page_boundary.arr[7] = -3;
    return on_page_boundary;
}

int main(void) {
    // call function that returns on_page_boundary
    struct ten_bytes x = return_struct();

    // validate it
    for (int i = 0; i < 7; i = i + 1) {
        if (x.arr[i]) {
            return 1;
        }
    }

    if (x.arr[7] != -3) {
        return 2;
    }
    if (x.arr[8] != -2) {
        return 2;
    }
    if (x.arr[9] != -1) {
        return 3;
    }
    return 0;  // success
}