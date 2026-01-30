/* Because long is a keyword, you can't use it as a function name */
int long(void) {
    return 4;
}

int main(void){
    return long();
}