data "aws_iam_policy_document" "datadog_aws_integration_assume_role" {
  statement {
    actions = ["sts:AssumeRole"]

    principals {
      type = "AWS"
      identifiers = ["arn:aws:iam::123456:root"]
    }

    condition {
      test = "StringEquals"
      variable = "sts:ExternalId"  //variable is a keyword

      values = [
        "123456"
      ]
    }
  }
}
