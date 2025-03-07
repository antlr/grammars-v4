-- Using *.ALL to Retrieve All Table Columns, Expanding the Structured Types
SELECT *.ALL
FROM student_record;

-- Using *.ALL to Retrieve the Expanded Structured Type for a Single UDT Column
SELECT student_record.*.ALL;

-- Selecting the Information For Only One Student From a UDT
SELECT s.college.ALL
FROM student_record s
WHERE s.student.First_name() = 'Steven'
AND s.student.Last_name() = 'Smith';
