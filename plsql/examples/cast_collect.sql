SELECT 
	CAST
	(
		COLLECT(phone_numbers) AS phone_book_t
	) 
FROM customers
