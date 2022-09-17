resource "aws_route53_zone" "my_zone" {
  name  = "abc"

  provider = aws.my-provider
}
