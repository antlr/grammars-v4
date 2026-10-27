object someRule extends RewriteRule {
  def divide() = {
    new java.text.DecimalFormat().format(1 / 1000.0)
  }
}