data "aws_ami" "main" {
  name = "abc"
}

resource "aws_instance" "main" {
  ami = data.aws_ami.main.id
}

locals {
  a = "a"
}
