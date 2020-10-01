data "aws_ami" "main" {
  name = "abc"
}

resource "aws_instance" "main" {
  ami = xdata.aws_ami.main.id
}

locals {
  a = "a"
}
