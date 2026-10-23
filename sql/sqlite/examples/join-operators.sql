SELECT * FROM Song JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song NATURAL JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song LEFT JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song LEFT OUTER JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song RIGHT JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song RIGHT OUTER JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song FULL JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song FULL OUTER JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song INNER JOIN Album ON Song.albumId = Album.id;

SELECT * FROM Song CROSS JOIN Album ON Song.albumId = Album.id;