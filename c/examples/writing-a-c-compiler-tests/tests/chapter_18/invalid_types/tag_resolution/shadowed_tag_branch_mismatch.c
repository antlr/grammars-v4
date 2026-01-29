int main(void) {
    struct s {
        int i;
    };
    struct s struct1 = {1};

    {
        struct s {
            int i;
        };
        struct s struct2 = {2};

        // invalid conditional expression: struct1 and struct2 have different
        // types
        (void)(1 ? struct1 : struct2);
    }
}