main( ) {
  auto c;
read:
  c= getchar();
  putchar(c);
  if(c != '*n') goto read;
}               /* loop if not a newline */
