
declare type java:type as enum('null', 'type');

declare function java:analyze($node as node()?) as java:type external;
declare function java:is-subtype-of($type as java:type) as boolean external;
declare function java:enum-members($node as java:type?) as boolean external;
declare function java:any-enum() as java:type external;

for $s in //switchStmt
let $switchedtype := switchedExpr=!>java:analyze()
let $is-enum := java:is-subtype-of(java:any-enum())
return
	if ($is-enum) {
		let $members := $switchedtype =!> java:enum-members()
        return ()
	}
