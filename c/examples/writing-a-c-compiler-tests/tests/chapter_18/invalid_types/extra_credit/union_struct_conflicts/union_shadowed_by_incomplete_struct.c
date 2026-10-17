
/* When one type declaration shadows another with the same tag (including a
 * union type shadowing struct type or vice versa) you can't specify the outer
 * tag
 * */

int main(void) {
    union tag {int a;};
    {
        struct tag;
        union tag *x; // illegal b/c "union tag" isn't visible
    }
    return 0;
}