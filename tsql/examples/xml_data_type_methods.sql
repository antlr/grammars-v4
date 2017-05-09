-- Using the value() method against an xml type variable
DECLARE @myDoc xml
DECLARE @ProdID int
SET @myDoc = '<Root>
<ProductDescription ProductID="1" ProductName="Road Bike">
<Features>
  <Warranty>1 year parts and labor</Warranty>
  <Maintenance>3 year parts and labor extended maintenance is available</Maintenance>
</Features>
</ProductDescription>
</Root>'

SET @ProdID =  @myDoc.value('(/Root/ProductDescription/@ProductID)[1]', 'int' )
SELECT @ProdID
GO;
