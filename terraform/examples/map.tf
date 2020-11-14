locals {
  a    = {b = "C"
          d = 1}
  tags = merge(local.a, {v = "test"})
}
