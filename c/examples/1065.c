// NB: GCC preprocessor changes __cdecl to __attribute__((__cdecl__))
// under the covers, even though there are no #include files anywhere.
// VSC and CLANG do not alter.

typedef int UINTN;
void * __cdecl  LibPcdGetPtr (UINTN TokenNumber);
void __cdecl LibPcdGetPtr (UINTN TokenNumber);
