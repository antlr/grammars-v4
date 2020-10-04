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
