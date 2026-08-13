let $services := 
    //classDeclaration[typeList[1]/typeType =!> string() = 'TextDocumentService']
for $service in $services
let $implementedDocumentOperations 
    := $service//memberDeclaration[./modifier =!> string() = 'public']
                /methodDeclaration[./typeTypeOrVoid//typedentifier =!> string() = 'CompletableFuture']
let $customOperations := 
    //classDeclaration[typeList[1]/typeType =!> string() = 'TextDocumentService']
(: //classDeclarationmemberDeclaration/preceding-sibling::modifier :)
return ()

(: typeTypeOrVoid/typeType/annotation  :)
