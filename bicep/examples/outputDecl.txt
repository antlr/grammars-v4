// outputDecl ->
//   decorator* "output" IDENTIFIER(name) IDENTIFIER(type) "=" expression NL
//   decorator* "output" IDENTIFIER(name) "resource" interpString(type) "=" expression NL

output hostname string = publicIP.properties.dnsSettings.fqdn

output stringOutput string = deployment().name
output integerOutput int = length(environment().authentication.audiences)
output booleanOutput bool = contains(deployment().name, 'demo')
output arrayOutput array = environment().authentication.audiences
output objectOutput object = subscription()

output stringOutput string = user['user-name']

output endpoint string = deployStorage ? myStorageAccount.properties.primaryEndpoints.blob : ''

output deployedNSGs array = [for (name, i) in orgNames: {
  orgName: name
  nsgName: nsg[i].name
  resourceId: nsg[i].id
}]
