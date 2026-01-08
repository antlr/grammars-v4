SELECT FILTER([{'k':'v'}], x -> x:k::STRING = 'v');

SELECT FILTER([{'k':1}], x -> x:k = 1);
