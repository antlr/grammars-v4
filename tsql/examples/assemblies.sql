ALTER ASSEMBLY MyClass
ADD FILE FROM 'C:\MyClassProject\Class1.cs';
ALTER ASSEMBLY ComplexNumber WITH PERMISSION_SET = EXTERNAL_ACCESS;
ALTER ASSEMBLY ComplexNumber
FROM 'C:\Program Files\Microsoft SQL Server\130\Tools\Samples\1033\Engine\Programmability\CLR\UserDefinedDataType\CS\ComplexNumber\obj\Debug\ComplexNumber.dll' ;
