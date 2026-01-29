typedef int UINTN;
typedef struct {} MyStruct;

//function returns pointer
void *
__cdecl
f1(
   UINTN             param1
  );


int *
__cdecl
f2 (
   int             param1
  );


MyStruct *
f3 (
   int             param1,
   char            param2
  );
