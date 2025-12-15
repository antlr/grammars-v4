/* The @ symbol doesn't appear in any C tokens,
   except inside string or character literals. */
int main(void) {
    return 0@1;
}