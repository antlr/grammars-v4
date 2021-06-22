resource "aws_s3_bucket" "a" {
  bucket = "test_bucket"
}

resource "aws_s3_bucket_policy" "b" {
  bucket = aws_s3_bucket.a.id
  policy = jsonencode({
    "Version" : "2012-10-17",
    "Id"      : "my_policy",
    "Statement" : [
      {
        "Sid"       : "IPAllow",
        "Effect"    : "Deny",
        "Principal" : "*",
        "Action"    : "s3:*",
        "Resource"  : "arn:aws:s3:::test_bucket/*",
        "Condition" : {
          "IpAddress" : { "aws:SourceIp" : "1.2.3.4/32" }
        }
      }
    ]
  })
}

resource "aws_s3_bucket" "d" {
  bucket = "test_bucket"
}
