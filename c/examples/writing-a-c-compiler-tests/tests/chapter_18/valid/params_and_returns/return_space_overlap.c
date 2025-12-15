/* If a return value is passed on the stack in space allocated by the caller,
 * test that this space does not overlap with any other objects the callee
 * can access, as the ABI requires.
 * This could happen if you implement more aggressive optimizations than
 * the book covers - e.g. if you rewrote the TACKY code
 *   tmp = f()
 *   globvar = tmp
 * as
 *   globvar = f()
 * On the other hand, it's fine to rewrite
 *   tmp = f()
 *   localvar = tmp
 * as
 *   localvar = f()
 * as long as f() can't otherwise access localvar (e.g. through a pointer)
 * */
struct s {
    long l1;
    long l2;
    long l3;
};

/* These are defined in
 * tests/chapter_18/valid/params_and_returns/return_space_address_overlap_<PLATFORM>.s
 */

extern struct s globvar;  // initialized to 0

// Validate that memory RDI points to does not overlap with globvar,
// then return { 400, 500, 600 }
struct s overlap_with_globvar(void);
// Validate that memory RDI points to does not overlap with ptr,
// then return { ptr->l1 * 2, ptr->l2 * 2, ptr->l3 * 2 }
struct s overlap_with_pointer(struct s *ptr);

int main(void) {
    // make sure we don't pass the address of globvar as the return
    // address in RDI
    globvar = overlap_with_globvar();
    if (globvar.l1 != 400l || globvar.l2 != 500l || globvar.l3 != 600l) {
        return 2;
    }

    // make sure we don't pass the address of my_struct as the return
    // address in RDI, since we also pass it as the first argument
    struct s my_struct = {10l, 9l, 8l};
    my_struct = overlap_with_pointer(&my_struct);
    if (my_struct.l1 != 20l || my_struct.l2 != 18l || my_struct.l3 != 16l) {
        return 4;
    }

    return 0;  // success
}