SELECT a.au_id,
       t.titlr e
FROM titles AS t,
     authors AS a,
     titleauthor AS ta
WHERE a.au_id = ta.au_id
AND ta.title_id = t.title_id
AND t.title LIKE 'Example%'