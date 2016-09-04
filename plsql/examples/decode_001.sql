SELECT
decode(
	a.attribute_name, n.col,
	a.attribute_num_value * n.scale + n.shift,
	a.attribute_num_value) as "values"
	from employees