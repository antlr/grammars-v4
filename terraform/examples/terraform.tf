terraform {
  required_version = ">= 0.12.20"

  backend "s3" {
    bucket     = "terraform-state"
    encrypt    = true
    kms_key_id = "arn:aws:kms:eu-central-1:12345:key/455ffca3-aebb-181b-b0de-fe435017808"
    key        = "terraform"
    region     = "eu-central-1"

    dynamodb_table = "terraform-locks"
  }
}
