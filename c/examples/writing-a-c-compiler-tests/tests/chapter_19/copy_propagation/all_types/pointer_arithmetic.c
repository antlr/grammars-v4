/* Test that we propagate copies into AddPtr instructions */
int target(void) {
    int nested[3][23] = {{0, 1}, {2}};

    // we'll initially generate something like this:
    // index0 = SignExtend(1)
    // tmp2 = AddPtr(ptr=tmp1, index=index0, scale=92)
    // index1 = SignExtend(0)
    // tmp3 = AddPtr(ptr=tmp2, index=index1, scale=4)
    // return tmp3
    // But after constant folding and copy propagation, both AddPtr
    // instruction should have constant indices, so we won't need
    // any imul instructions in the final assembly (like we normally would to
    // implement AddPtr with a non-standard scale)
    return nested[1][0];
}

int main(void) {
    return target();
}