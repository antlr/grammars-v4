struct s;

int main(void) {
  return sizeof(struct s); // can't take size of incomplete type
}