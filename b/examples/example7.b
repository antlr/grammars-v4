main( ) {
  auto c;
  while (1) {
    while ( (c=getchar()) != ' ')
      if (putchar(c) == '*n') exit();
    putchar( '*n' );
    while ( (c=getchar()) == ' '); /* skip blanks */
    if (putchar(c)=='*n') exit(); /* done when newline */
    }
}
