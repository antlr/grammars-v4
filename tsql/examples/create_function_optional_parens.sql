CREATE FUNCTION [main].[TestParentheses]
  (
    @WHY INTEGER
  )
  RETURNS TABLE
  AS
  RETURN
  (
  SELECT @WHY + 1 AS UltimateAnswer
  )