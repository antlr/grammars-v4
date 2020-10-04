CREATE PROCEDURE Example_Sproc
	@SomeValue BIT,
	@SomeOtherValue SMALLINT,
	@Days INT
AS
BEGIN
	SET NOCOUNT ON;

	INSERT INTO Example (
		SomeValue,
		SomeOtherValue,
		DAYS
	)
	OUTPUT inserted.Id
	VALUES (
		@SomeValue,
		@SomeOtherValue,
		@DAYS
	);
 
END
