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

module "m" {
  source = "./module/"
}

resource "aws_instance" "server" {
  instance_type = "t2.micro"

  ami = data.aws_ami.ami.id

  security_groups = [aws_security_group.server.name]

  tags = {
    Name = "my server"
  }
}

data "aws_ami" "ami" {
  most_recent      = true
  name_regex       = "my-ami"
  owners           = ["self"]
}

locals {
  a = "B"
}

provider "aws" {
  region = "eu-central-1"
}

variable "in" {
  type = string
}

output "out" {
  value = aws_instance.server.instance_type
}
