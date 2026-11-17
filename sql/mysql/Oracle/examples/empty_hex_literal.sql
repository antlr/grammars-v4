-- Empty hexadecimal literal x'' (zero-length binary string), accepted by MySQL.
-- Lower case is used because this grammar is not case-folded in the test harness.
CREATE TABLE t_default (v VARBINARY(1) DEFAULT x'');
ALTER TABLE t_default ADD COLUMN normalized_ref VARBINARY(16) GENERATED ALWAYS AS (IF(v = 'default', x'', v)) VIRTUAL;
CREATE TABLE t_control (v VARBINARY(1) DEFAULT x'00');
