union u {
    int a;
};

int main(void) {
    union u foo = {1};

    // introduce a different union u type
    union u {
        int b;
    };

    return foo.b; // foo belongs to outer union u type, which doesn't have member 'b'
}