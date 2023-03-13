CREATE PROC dbo.ExecuteProc (
  @param1 VARCHAR(30),
  @param2 VARCHAR(30),
  @tab varchar(30),
  @value varchar(30)
) 
AS
BEGIN
  DECLARE @cond1 varchar(30),
          @cond2 varchar(30)
  
  -- support double quoted strings
  SELECT @cond1 = " AND col3 > 50 ",
         @cond1 = " AND col4 < 20 "

  -- alternating string and local id
  EXECUTE ("INSERT INTO " + @tab + " ('col1', 'col2', " + @value + ")
              SELECT col1, col2 FROM " + @tab2 )

  -- but following the Sybase definition alternating is not mandatory
	EXEC ( "INSERT	INTO	" + @param1 + " (col1, col2"+ @param2 + ")
			      SELECT	col1, col2 " + @insert + " 
			      FROM	" + @tab + "
            WHERE	col1 = '" + @value + "' " + @cond_tab + @cond_prod + "
            AND 	col5 = '" + @value + "'
            AND		col6 > 0 ")

	EXECUTE ("DELETE FROM " + @tab + " 
            WHERE	col1 = '" + @value + "' " + @cond_tab + @cond_prod + " AND TRUE ")

END