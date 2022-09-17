provider "google" {
  region      = var.region
  credentials = file(var.credentials_file_path)
}

provider "random" {
}

resource "random_id" "project_name" {
  byte_length = 8
}

resource "google_project" "endpoints_project" {
  name            = "Endpoints Project"
  project_id      = "tf-ep-${random_id.project_name.hex}"
  org_id          = var.org_id
  billing_account = var.billing_account_id
}

resource "google_project_service" "endpoints_project" {
  project = google_project.endpoints_project.project_id
  service = "compute.googleapis.com"
}

resource "google_project_service" "endpoints_project_sm" {
  project = google_project.endpoints_project.project_id
  service = "servicemanagement.googleapis.com"
}

resource "google_endpoints_service" "endpoints_service" {
  service_name = "echo-api.endpoints.${google_project.endpoints_project.project_id}.cloud.goog"
  project      = google_project.endpoints_project.project_id

  openapi_config = <<EOF
swagger: "2.0"
info:
  description: "A simple Google Cloud Endpoints API example."
  title: "Endpoints Example"
  version: "1.0.0"
host: "echo-api.endpoints.${google_project.endpoints_project.project_id}.cloud.goog"
basePath: "/"
consumes:
- "application/json"
produces:
- "application/json"
schemes:
- "https"
paths:
  "/echo":
    post:
      description: "Echo back a given message."
      operationId: "echo"
      produces:
      - "application/json"
      responses:
        200:
          description: "Echo"
          schema:
            $ref: "#/definitions/echoMessage"
      parameters:
      - description: "Message to echo"
        in: body
        name: message
        required: true
        schema:
          $ref: "#/definitions/echoMessage"
      security:
      - api_key: []
definitions:
  echoMessage:
    properties:
      message:
        type: "string"
EOF


  depends_on = [google_project_service.endpoints_project_sm]
}

resource "google_compute_network" "network" {
  name                    = "ep-network"
  auto_create_subnetworks = "true"
  project                 = google_project.endpoints_project.project_id
  depends_on              = [google_project_service.endpoints_project]
}

# Allow the hosted network to be hit over ICMP, SSH, and HTTP.
resource "google_compute_firewall" "network" {
  name    = "allow-ssh-and-icmp"
  network = google_compute_network.network.self_link
  project = google_compute_network.network.project

  allow {
    protocol = "icmp"
  }

  allow {
    protocol = "tcp"
    ports    = ["22", "80"]
  }
}

resource "google_compute_instance" "project_1_vm" {
  name         = "tf-ep-vm"
  project      = google_project.endpoints_project.project_id
  machine_type = "f1-micro"
  zone         = var.region_zone

  boot_disk {
    initialize_params {
      image = "projects/debian-cloud/global/images/family/debian-8"
    }
  }

  metadata = {
    endpoints-service-name      = google_endpoints_service.endpoints_service.service_name
    endpoints-service-config-id = google_endpoints_service.endpoints_service.config_id
    startup-script              = file("scripts/install-vm.sh")
  }

  network_interface {
    network = google_compute_firewall.network.network

    access_config {
      // Ephemeral IP
    }
  }

  service_account {
    scopes = ["https://www.googleapis.com/auth/cloud-platform"]
  }

  depends_on = [
    google_project_service.endpoints_project_sm,
    google_project_service.endpoints_project,
  ]
}

output "ip" {
  value = google_compute_instance.project_1_vm.network_interface[0].access_config[0].nat_ip
}
