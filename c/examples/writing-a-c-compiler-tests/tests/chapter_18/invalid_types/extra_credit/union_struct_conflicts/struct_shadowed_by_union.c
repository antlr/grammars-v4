/* When one type declaration shadows another with the same tag (including a
 * union type shadowing struct type or vice versa) you can't specify the outer
 * tag
 * */
int main(void) {
    struct tag {int a;};
    {
        union tag {long l;}; // shadows previous definition of tag
        // illegal to specify 'struct tag' here b/c it conflicts with
        // 'union tag' declared above
        struct tag *x;
    }
    return 0;
}