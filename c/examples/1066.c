typedef int UINTN;
typedef GUID long;

typedef
   void
   (__cdecl *PCD_CALLBACK)(
   const GUID *CallBackGuid,
   UINTN CallBackToken,
   void *TokenData,
   UINTN TokenDataSize
  );