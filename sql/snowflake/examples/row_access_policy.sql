--SQL test for ROW ACCESS policy related
CREATE OR REPLACE ROW ACCESS POLICY rap_simple
    AS (code_organisation VARCHAR) RETURNS BOOLEAN ->
    CURRENT_ROLE() = 'ADMIN';

-- body wrapped in CASE (already worked)
CREATE ROW ACCESS POLICY IF NOT EXISTS rap_case
    AS (code_organisation VARCHAR) RETURNS BOOLEAN ->
    CASE
        WHEN CURRENT_ROLE() = 'ADMIN' THEN TRUE
        ELSE FALSE
    END;

-- EXISTS(subquery) as the bare policy body (the case this PR fixes)
CREATE ROW ACCESS POLICY rap_exists
    AS (code_organisation VARCHAR) RETURNS BOOLEAN ->
    EXISTS (
        SELECT 1
        FROM mapping_table AS m
        WHERE m.user_code = LEFT(CURRENT_USER(), 6)
          AND m.code_organisation = code_organisation
    );

-- EXISTS combined with other boolean operators
CREATE ROW ACCESS POLICY rap_exists_combined
    AS (val VARCHAR) RETURNS BOOLEAN ->
    CURRENT_ROLE() = 'ADMIN'
    OR EXISTS (SELECT 1 FROM allowlist WHERE allowlist.val = val);

-- NOT EXISTS as a boolean expression
CREATE ROW ACCESS POLICY rap_not_exists
    AS (val VARCHAR) RETURNS BOOLEAN ->
    NOT EXISTS (SELECT 1 FROM blocklist WHERE blocklist.val = val);
