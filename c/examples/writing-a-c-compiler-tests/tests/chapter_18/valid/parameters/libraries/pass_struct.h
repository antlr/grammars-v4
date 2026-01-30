/* Basic test of passing an argument of structure type: similar to chapter_18/valid/parameters/simple.c
 * but split into two translation units
 * */
struct pair {
    int x;
    int y;
};

int validate_struct_param(struct pair p);