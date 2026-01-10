struct s;

extern struct s x;

int main(void) {
  // can't use expression w/ incomplete struct type as expression statement
  for (x;;)
    ;
  return 0;
}