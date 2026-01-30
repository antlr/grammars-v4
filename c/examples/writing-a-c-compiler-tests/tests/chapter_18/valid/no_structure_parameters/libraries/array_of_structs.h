/* Test that we can pass a pointer to an array of structures as a parameter */

struct inner {
    long l;
    char arr[2];
};  // size: 8 bytes, alignment: 4 bytes

struct outer {
    char a;          // byte 0
    struct inner b;  // bytes 4-11

};  // size: 12 byte, alignment: 4 bytes

int validate_struct_array(struct outer *struct_array);