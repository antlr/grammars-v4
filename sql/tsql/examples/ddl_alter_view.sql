ALTER VIEW HumanResources.EmployeeHireDate  
AS
SELECT p.FirstName, p.LastName, e.HireDate
FROM HumanResources.Employee AS e JOIN Person.Person AS p
                                       ON e.BusinessEntityID = p.BusinessEntityID
WHERE HireDate < CONVERT(DATETIME,'20020101',101) ;
GO
