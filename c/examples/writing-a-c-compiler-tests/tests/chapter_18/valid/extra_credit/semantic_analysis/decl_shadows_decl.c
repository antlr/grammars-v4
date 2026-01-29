/* A struct type declaration can shadow a union type declaration
 * with the same tag, or vice versa. See
 * chapter_18/invalid_types/extra_credit/struct_and_union_ptrs.c
 * for a similar test case where we verify that the typechecker
 * can distinguish between pointers to these types
 */

int main(void) {
    struct tag; // declare (don't define) a struct type
    struct tag *struct_ptr = 0;
    {
        union tag; // declare (don't define) a union type, shadowing outer declaration
        union tag *union_ptr = 0;

        // both pointers are null
        if (struct_ptr || union_ptr) {
            return 1;// fail
        }
    }
    return 0;
}