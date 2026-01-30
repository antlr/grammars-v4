int main(void) {
    int x = 10;
    // two versions of empty switch statements;
    // in both , we execute the controlling expression even though there's
    // nothing to execute in the body
    switch(x = x + 1) {

    }
    switch(x = x + 1)
    ;
    return x;
}