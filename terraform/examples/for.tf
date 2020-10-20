locals {
  allowed_consumers  = ["A", "B"]
  vpce_consumer_arns = [for id in local.allowed_consumers : "arn:aws:iam::${id}:root"]
}
