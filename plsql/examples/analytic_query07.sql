	with
	clus_tab as (
	select id,
	a.attribute_name aname,
	a.conditional_operator op,
	nvl(a.attribute_str_value,
	round(decode(a.attribute_name, n.col,
	a.attribute_num_value * n.scale + n.shift,
	a.attribute_num_value),4)) val,
	a.attribute_support support,
	a.attribute_confidence confidence
	from table(dbms_data_mining.get_model_details_km('km_sh_clus_sample')) t,
	table(t.rule.antecedent) a,
	km_sh_sample_norm n
	where a.attribute_name = n.col (+) and a.attribute_confidence > 0.55
	),
	clust as (
	select id,
	cast(collect(cattr(aname, op, to_char(val), support, confidence)) as cattrs) cl_attrs
	from clus_tab
	group by id
	),
	custclus as (
	select t.cust_id, s.cluster_id, s.probability
	from (select
	cust_id
	, cluster_set(km_sh_clus_sample, null, 0.2 using *) pset
	from km_sh_sample_apply_prepared
	where cust_id = 101362) t,
	table(t.pset) s
	)
	select a.probability prob, a.cluster_id cl_id,
	b.attr, b.op, b.val, b.supp, b.conf
	from custclus a,
	(select t.id, c.*
	from clust t,
	table(t.cl_attrs) c) b
	where a.cluster_id = b.id
	order by prob desc, cl_id asc, conf desc, attr asc, val asc
