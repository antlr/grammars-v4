struct incomplete;

struct s {
  // member type is invalid: illegal to specify array of incomplete type,
  // even as a pointer's referenced type
  struct incomplete (*array_pointer)[3];
};