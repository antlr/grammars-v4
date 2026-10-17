/* Test long expressions in &&, ||, ! and controlling expressions */

int not(long l) {
    return !l;
}

int if_cond(long l) {
    if (l) {
        return 1;
    }
    return 0;
}

int and(long l1, int l2) {
    return l1 && l2;
}

int or(int l1, long l2) {
    return l1 || l2;
}

int main(void) {
    // this would be equal to zero if we only considered lower 32 bits
    long l = 1152921504606846976l; // 2^60
    long zero = 0l;
    if (not(l)) {
        return 1;
    }
    if (!not(zero)) {
        return 2;
    }
    if(!if_cond(l)) {
        return 3;
    }
    if(if_cond(zero)) {
        return 4;
    }

    if (and(zero, 1)) {
        return 5;
    }

    if (!or(1, l)) {
        return 6;
    }

    return 0;

}