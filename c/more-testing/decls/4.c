int a;
int * b;
extern int c;
extern int *d;

void foo() {
	a = 1;
	*b = 2;
	c = 3;
	*d = 4;
}