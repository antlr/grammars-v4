/* String literals can only initialize char arrays,
 * not arrays of other types */
int main(void) {
    long ints[4] = "abc";
    return ints[1];
}