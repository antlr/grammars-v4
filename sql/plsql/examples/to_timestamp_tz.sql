select
	raw_value,
	to_timestamp_tz(raw_value),
	to_timestamp_tz(raw_value, 'YYYY-MM-DD'),
	to_timestamp_tz(raw_value, 'YYYY-MM-DD', 'NLS_DATE_LANGUAGE = RUSSIAN'),
	to_timestamp_tz(raw_value default '1970-01-01' on conversion error, 'YYYY-MM-DD'),
	to_timestamp_tz(raw_value default '1970-01-01' on conversion error, 'YYYY-MM-DD', 'NLS_DATE_LANGUAGE = RUSSIAN'),
	to_timestamp_tz(raw_value default null on conversion error),
	to_timestamp_tz(raw_value default null on conversion error, 'YYYY-MM-DD'),
	to_timestamp_tz(raw_value default null on conversion error, 'YYYY-MM-DD', 'NLS_DATE_LANGUAGE = RUSSIAN')
from raw_values;