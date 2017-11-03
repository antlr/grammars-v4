CREATE FUNCTION [Report].[Abc]
	(
		 @Stichtag_Version_DM_SID					INT
		,@Vergleichsstichtag1_Version_DM_SID		INT
		,@Vergleichsstichtag2_Version_DM_SID		INT
		,@Vergleichsstichtag3_Version_DM_SID		INT
	)
	RETURNS TABLE AS RETURN
	(
	WITH Cde AS
	(
		SELECT 1
	)
		SELECT 2
	)
