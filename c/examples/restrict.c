// __restrict_keyword.c
// compile with: /LD
// In the following function, declare a and b as disjoint arrays
// but do not have same assurance for c and d.
void sum2(int n, int * __restrict a, int * __restrict b,
	  int * c, int * d) {
	int i;
	for (i = 0; i < n; i++) {
		a[i] = b[i] + c[i];
		c[i] = b[i] + d[i];
	}
}

// By marking union members as __restrict, tell compiler that
// only z.x or z.y will be accessed in any given scope.
union z {
	int * __restrict x;
	double * __restrict y;
};