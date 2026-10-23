INSERT INTO table_name (field_name_1, field_name_2) VALUES ('value1', 'value2') ON CONFLICT (field_name_1) DO NOTHING ON CONFLICT (field_name_2) DO NOTHING;
