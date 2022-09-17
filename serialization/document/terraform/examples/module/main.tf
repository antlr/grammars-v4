variable "in" {
  type = string
}

output "out" {
  value = var.in
}

output "all_s3_arns" {
  value = aws_s3_bucket.qlikview.*.arn[0]
}
