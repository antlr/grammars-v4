/* You can only call a function, not a constant.
   Our implementation will reject this during parsing.
   Because the C grammar permits this declaration,
   some compilers may reject it during type checking.
*/

int main(void) {
    return 1();
}