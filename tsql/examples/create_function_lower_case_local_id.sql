CREATE FUNCTION [main].[TestParentheses]
  (
    @Why INTEGER
  )
  RETURNS TABLE
  AS
  RETURN
  (
  SELECT @Why + 1 AS UltimateAnswer
  )