/* This is
   a multiline
   comment */

variable "var" {
  // single line comment
  # single line comment
  type = string
}

variable "names" {
  type = list
}

resource "aws_ec2_instance" "instance" {
  description = join(",", var.names)
}
