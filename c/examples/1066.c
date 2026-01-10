typedef int UINTN;
typedef long GUID;

typedef
   void
   (__cdecl *PCD_CALLBACK)(
   const GUID *CallBackGuid,
   UINTN CallBackToken,
   void *TokenData,
   UINTN TokenDataSize
  );