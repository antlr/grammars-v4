let $records := //namedRecordDecl
let $items := //itemTypeDecl
let $modules := (
	for $p in //importDecl/STRING
	let $parsed := antlr:parse($p, "antlrquery", "xquery")
	return $parsed
	)
let $importedRecords := $modules//namedRecordDecl[./annotations/qname = 'public']
let $importedItems := $modules//itemRecordDecl[./annotations/qname = 'public']
let $importedFunctions := $modules//functionDecl[./annotations/qname = 'public']
let $allNames := $records/qname
	           | $items/qname
	           | $importedRecords/qname
	           | $importedItems/qname
let $duplicates := (
		for $name in $allNames
		group by $strName := string($name)
		where count($name) > 1
		return array{$name}
	) => diagnose(())
for $record in $records
return $record
