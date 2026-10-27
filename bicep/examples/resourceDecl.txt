// resourceDecl -> decorator* "resource" IDENTIFIER(name) interpString(type) "existing"? "=" (ifCondition | object | forExpression) NL

resource stg 'Microsoft.Storage/storageAccounts@2019-06-01' = {
  name: 'examplestorage'
  location: 'eastus'
}

resource stg 'Microsoft.Storage/storageAccounts@2019-06-01' existing = {
  name: 'examplestorage'
  location: 'eastus'
}

resource stg 'Microsoft.Storage/storageAccounts@2019-06-01' = {
  name: 'examplestorage'
  location: 'eastus'
  sku: {
    name: 'Standard_LRS'
    tier: 'Standard'
  }
  kind: 'StorageV2'
  properties: {
    accessTier: 'Hot'
  }
}
