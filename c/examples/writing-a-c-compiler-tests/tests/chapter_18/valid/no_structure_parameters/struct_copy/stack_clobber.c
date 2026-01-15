/* Test that copying an aggregate value with Copy, Load, Store,
 * CopyFromOffset or CopyToOffset doesn't clobber the stack.
 * To do this, we store some bytes on the stack, copy the struct,
 * then validate those bytes haven't changed. This test assumes structures
 * are allocated on the stack in the same order they're declared/initialized,
 * so we know which objects are right next to the one we copy to. If this assumption
 * doesn't hold, clobbers may overwrite stack padding instead of data that we
 * validate, and go undetected).
 */

void exit(int status);

struct chars {
    char char_array[3];
};

static struct chars y = {{0, 1, 2}};
static struct chars *ptr;  // in main we'll make this point to y

// validate a three-char array, which should contain
// an increasing sequence of values starting with 'start'
// If validation fails, exit with status code 'code'
void validate_array(char *char_array, int start, int code) {

    for (int i = 0; i < 3; i = i + 1) {
        if (char_array[i] != start + i) {
            exit(code);
        }
    }
    return;
}

// use different values for stack bytes and y in each test:
// this makes it less likely that tests will happen to succeed
// when they should fail
// because correct values are left over in uninitialized memory
// from previous invocations
void increment_y(void) {
    y.char_array[0] = y.char_array[0] + 3;
    y.char_array[1] = y.char_array[1] + 3;
    y.char_array[2] = y.char_array[2] + 3;
}

// Test case 1: copy struct via Copy instruction
int test_copy(void) {
    // write some values to stack
    struct chars a = {"abc"};
    struct chars b = {"xyz"};
    struct chars c = {"def"};
    // copy struct to b
    b = y;
    // validate a, b, and c - make sure a and c weren't clobbered
    validate_array(a.char_array, 'a', 1);
    validate_array(b.char_array, 0, 2);
    validate_array(c.char_array, 'd', 3);
    return 0;
}

// Test case 2: copy struct via Load instruction
// b = *ptr will translate to the following TACKY:
//   Load(ptr, tmp)
//   b = tmp
// Be careful not to store any temporary values
// on the stack other than tmp, to we can be sure that
// clobbers will overwrite the bytes we validate rather than
// some other temporary value

// helpers to validate other stuff on stack without generating any other temporary variables
static struct chars to_validate;
void validate_static(int start, int code) {
    validate_array(to_validate.char_array, start, code);
}

int test_load(void) {
    static struct chars b; // keep b in static storage, not on the stack
    // write some values to stack
    struct chars a = {"ghi"};
    // load value from ptr into temporary 'struct char', then copy to b
    b = *ptr; // we set ptr in main
    // validate a and b
    to_validate = a;
    validate_static('g', 4);
    to_validate = b;
    validate_static(3, 5);
    return 0;
}

// Test case 3: copy struct via Store instruction
int test_store(void) {
    // write some values to stack
    struct chars struct_array[3] = {{"jkl"}, {"xyz"}, {"mno"}};
    struct chars *ptr = &struct_array[1];

    // store y through pointer to array element
    *ptr = y;

    // validate each array element, make sure elements 0 and 2 weren't changed
    validate_array(struct_array[0].char_array, 'j', 6);
    validate_array(struct_array[1].char_array, 6, 7);
    validate_array(struct_array[2].char_array, 'm', 8);
    return 0;
}

// define a struct that contains nested struct char
struct chars_container {
    char c;
    struct chars chars;
    char arr[3];
};

// Test case 4: copy struct via CopyFromOffset instruction
// b = big_struct.member becomes the following TACKY:
//   tmp = CopyFromOffset(big_struct, member offset)
//   b = tmp
// Be careful not to store any temporary values
// on the stack other than tmp, to we can be sure that
// clobbers will overwrite the bytes we validate rather than
// some other temporary value
int test_copy_from_offset(void) {
    // write some values to stack
    struct chars a = {"pqr"};

    static struct chars b = {"xyz"};
    static struct chars_container container = {100, {{9, 10, 11}}, "123"};

    // copy to temporary struct via CopyFromOffset, then to b
    b = container.chars;

    // validate a and b
    to_validate = a;
    validate_static('p', 9);
    to_validate = b;
    validate_static(9, 10);
    return 0;
}

// Test case 5: copy struct via CopyToOffset instruction
int test_copy_to_offset(void) {

    struct chars_container container = {
        'x', {{0, 0, 0}}, "stu"
    };

    // copy to nested struct chars via CopyToOffset
    container.chars = y;

    // validate all elements of container
    if (container.c != 'x') {
        exit(11);
    }

    validate_array(container.chars.char_array, 12, 12);

    validate_array(container.arr, 's', 13);

    return 0;
}

int main(void) {
    ptr = &y;
    test_copy();
    increment_y();
    test_load();
    increment_y();
    test_store();
    increment_y();
    test_copy_from_offset();
    increment_y();
    test_copy_to_offset();
    return 0;
}