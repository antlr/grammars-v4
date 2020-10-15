data "aws_ami" "main" {
  name = "abc"
}

resource "aws_instance" "main" {
  ami = data.aws_ami.main.id
}

locals {
  vpce_consumer_arns = ["1", "2", "3"]
  star_reference     = aws_vpc_endpoint_service_allowed_principal.vpce_producer_allowed_consumers.*.principal_arn
}

resource "aws_vpc_endpoint_service_allowed_principal" "vpce_producer_allowed_consumers" {
  count = length(local.vpce_consumer_arns)

  vpc_endpoint_service_id = "this is an id"
  principal_arn           = local.vpce_consumer_arns[count.index]
}

module "a" {
  source = "./module"
}

locals {
  b = module.a.out
}

resource "aws_acm_certificate" "ecs_certificate" {
  domain_name               = "abc.de"
  subject_alternative_names = ["*.abc.de"]
  validation_method         = "DNS"

  tags = {}

  options {
    certificate_transparency_logging_preference = "ENABLED"
  }

  lifecycle {
    //due to AWS service limits we are not allowed to issue more than x certifactes
    prevent_destroy = true
  }
}

resource "aws_route53_record" "cert_validation_record" {
  name     = aws_acm_certificate.ecs_certificate.domain_validation_options.0.resource_record_name
  type     = aws_acm_certificate.ecs_certificate.domain_validation_options.0.resource_record_type
  zone_id  = "4711"
  records  = [aws_acm_certificate.ecs_certificate.domain_validation_options.0.resource_record_value]
  ttl      = 60
}
