// parameterDecl ->
//   decorator* "parameter" IDENTIFIER(name) typeExpression parameterDefaultValue? NL |
//   decorator* "parameter" IDENTIFIER(name) "resource" interpString(type) parameterDefaultValue? NL |

param siteName string = 'site${uniqueString(resourceGroup().id)}'

param hostingPlanName string = '${siteName}-plan'

@secure()
param demoPassword string

@description('Must be at least Standard_A3 to support 2 NICs.')
param virtualMachineSize string = 'Standard_DS1_v2'

@description('Must be at least Standard_A3 to support 2 NICs.')
param virtualMachineSize resource '...' = 'Standard_DS1_v2'

param vNetSettings object = {
  name: 'VNet1'
  location: 'eastus'
  addressPrefixes: [
    {
      name: 'firstPrefix'
      addressPrefix: '10.0.0.0/22'
    }
  ]
  subnets: [
    {
      name: 'firstSubnet'
      addressPrefix: '10.0.0.0/24'
    }
    {
      name: 'secondSubnet'
      addressPrefix: '10.0.1.0/24'
    }
  ]
}
