// subscript expression requires an integer index, not void

int main(void) {
  char arr[3];
  return arr[(void)1];
}