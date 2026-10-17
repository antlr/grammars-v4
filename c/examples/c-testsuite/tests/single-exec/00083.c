#define CALL(FUN, ...) FUN(__VA_ARGS__)

int
one(int a)
{
	if (a != 1)
		return 1;
	
	return 0;
}

int
two(int a, int b)
{
	if (a != 1)
		return 1;
	if (b != 2)
		return 1;
	
	return 0;
}

int
three(int a, int b, int c)
{
	if (a != 1)
		return 1;
	if (b != 2)
		return 1;
	if (c != 3)
		return 1;
	
	return 0;
}

int
main()
{
	if (CALL(one, 1))
		return 2;
	if (CALL(two, 1, 2))
		return 3;
	if (CALL(three, 1, 2, 3))
		return 4;
	
	return 0;
}
