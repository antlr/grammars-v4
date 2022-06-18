class A{
  bool isPlusOrMinus(Expression expression) {
    if (expression.operator == '+') return true;
    if (expression.operator == '-') return true;
    return false;
  }
}