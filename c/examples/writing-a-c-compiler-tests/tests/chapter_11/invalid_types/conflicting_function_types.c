/* It's illegal to declare a function multiple times with different parameter types */

int foo(int a);

int main(void) {
    return 0;
}

int foo(long a);