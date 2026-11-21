(: ============================================================ :)
(: CREATE -- build new nodes and maps :)
(: ============================================================ :)

(: computed element constructor :)
element #book {
    element #title   { "XQuery 4.0 Unleashed" },
    element #author  { "Priscilla Walmsley" },
    element #price   { 49.99 },
    attribute #id    { "book-001" }
} ;

(: direct element constructor :)
<book id="book-002">
    <title>Learning XQuery</title>
    <author>Jason Hunter</author>
    <price>39.95</price>
</book> ;

(: create a new map record :)
map { "id": "emp-001", "name": "Alice", "dept": "Engineering", "salary": 95000 } ;

(: create an array of records :)
[
    map { "id": 1, "product": "Widget", "qty": 100 },
    map { "id": 2, "product": "Gadget", "qty": 50  },
    map { "id": 3, "product": "Doohickey", "qty": 200 }
] ;

(: create a sequence of elements :)
for $i in 1 to 5
return element #item { attribute #seq { $i }, "item " || $i } ;

(: ============================================================ :)
(: READ -- query and select :)
(: ============================================================ :)

(: select all books :)
/catalog/book ;

(: select by predicate :)
/catalog/book[@id = "book-001"] ;

(: select with path :)
/catalog/book/title ;

(: select with FLWOR :)
for $b in /catalog/book
where $b/price > 30
order by $b/price descending
return $b/title ;

(: read from a map :)
let $emp := map { "name": "Bob", "dept": "Sales" }
return $emp?name ;

(: read all map values :)
let $m := map { "a": 1, "b": 2, "c": 3 }
return for value $v in $m return $v ;

(: read array members :)
let $arr := [10, 20, 30, 40, 50]
return for member $v at $i in $arr return concat($i, ": ", $v) ;

(: grouped read :)
for $b in /catalog/book
let $cat := $b/@category
group by $cat
return element #category { attribute #name { $cat }, $b } ;

(: ============================================================ :)
(: UPDATE -- functional transformation (XQuery is immutable; :)
(:           "update" means build a modified copy)            :)
(: ============================================================ :)

(: update a single field by rebuilding the element :)
for $b in /catalog/book[@id = "book-001"]
return element #book {
    $b/@*,
    $b/title,
    $b/author,
    element #price { $b/price * 0.9 },    (: 10% discount :)
    element #updated { current-date() }
} ;

(: update a map value :)
let $emp := map { "id": "emp-001", "name": "Alice", "salary": 95000 }
return $emp +:= map { "salary": 100000, "promoted": true() } ;

(: bulk update: apply a transformation to all matching nodes :)
for $b in /catalog/book
where xs:decimal($b/price) > 40
return element #book {
    $b/@*,
    $b/*[not(self::price)],
    element #price { xs:decimal($b/price) * 0.85 }
} ;

(: ============================================================ :)
(: DELETE -- filter out unwanted items :)
(: ============================================================ :)

(: "delete" by selecting everything except the target :)
/catalog/book[not(@id = "book-001")] ;

(: delete by status flag :)
for $b in /catalog/book
where $b/@status != "discontinued"
return $b ;

(: delete map entry by rebuilding without a key :)
let $m   := map { "a": 1, "b": 2, "c": 3 }
let $del := "b"
return map:merge(
    for key $k value $v in $m
    where $k != $del
    return map { $k: $v }
) ;

(: delete array members matching a condition :)
let $arr := [1, 2, 3, 4, 5, 6]
return array {
    for member $v in $arr
    where $v mod 2 = 0     (: keep only even :)
    return $v
} ;

(: delete duplicate elements (distinct values) :)
distinct-values(/catalog/book/author) ;
