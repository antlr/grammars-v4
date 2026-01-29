//parameter contains pointer
typedef int MyType;
typedef int MyType1;

int
__cdecl
f1 (
   const MyType        *param1,
   int                  param2
  );


MyType1
__cdecl
f2 (
   MyType        *param1,
   int       *     param2
  );


void
__cdecl
f3 (
   void        *param1
  );