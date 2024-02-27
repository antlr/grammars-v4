{
  "StartAt": "MapState",
  "States": {
    "MapState": {
      "Type": "Map",
      "MaxConcurrency": 1,
      "ItemReader": {
        "ReaderConfig": {
          "InputType": "CSV",
          "CSVHeaderLocation": "GIVEN",
          "CSVHeaders": [
            "H1",
            "H2",
            "H3"
          ]
        },
        "Resource": "arn:aws:states:::s3:getObject",
        "Parameters": {
          "Bucket.$": "$.Bucket",
          "Key.$": "$.Key"
        }
      },
      "ItemProcessor": {
        "ProcessorConfig": {
          "Mode": "DISTRIBUTED",
          "ExecutionType": "STANDARD"
        },
        "StartAt": "PassItem",
        "States": {
          "PassItem": {
            "Type": "Pass",
            "End": true
          }
        }
      },
      "End": true
    }
  }
}
