/* This is adjusted to
 *   int f(int (*arr)[3])
 */
int f(int arr[2][3]);

/* This is adjusted to
 *   int f(int (*arr)[4])
 * so it conflicts with the previous declaration
 */
int f(int arr[2][4]);