void foo(int [5]);
void fooc(int x[const 5]);
void foos(int x[static 5]);
void foov(int x[volatile 5]);
void foor(int x[restrict 5]);
void fooc(int [const 5]);
void foos(int [static 5]);
void foov(int [volatile 5]);
void foor(int [restrict 5]);
void fooc(int (* const x));
void foos(int *x);
void foov(int * volatile x);
void foor(int * restrict x);
void fooc(int x[volatile 5])
{
  x[3] = 42;
#ifdef INVALID
  x = 0;
#endif
}
void foovm(int x[const *]);
void foovm(int * const x);
#ifdef INVALID
void wrongc(int x[3][const 4]);
void wrongvm(int x[static *]);
void foovm(int x[const *])
{
  x[2] = 1;
}
#endif
int main()
{
  return 0;
}
