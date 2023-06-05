--
-- This is created by pgsql/src/tools/findoidjoins/make_oidjoins_check
--
SELECT	ctid, aggfnoid
FROM	pg_catalog.pg_aggregate fk
WHERE	aggfnoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggfnoid);
SELECT	ctid, aggtransfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggtransfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggtransfn);
SELECT	ctid, aggfinalfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggfinalfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggfinalfn);
SELECT	ctid, aggcombinefn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggcombinefn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggcombinefn);
SELECT	ctid, aggserialfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggserialfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggserialfn);
SELECT	ctid, aggdeserialfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggdeserialfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggdeserialfn);
SELECT	ctid, aggmtransfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggmtransfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggmtransfn);
SELECT	ctid, aggminvtransfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggminvtransfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggminvtransfn);
SELECT	ctid, aggmfinalfn
FROM	pg_catalog.pg_aggregate fk
WHERE	aggmfinalfn != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.aggmfinalfn);
SELECT	ctid, aggsortop
FROM	pg_catalog.pg_aggregate fk
WHERE	aggsortop != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.aggsortop);
SELECT	ctid, aggtranstype
FROM	pg_catalog.pg_aggregate fk
WHERE	aggtranstype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.aggtranstype);
SELECT	ctid, aggmtranstype
FROM	pg_catalog.pg_aggregate fk
WHERE	aggmtranstype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.aggmtranstype);
SELECT	ctid, amhandler
FROM	pg_catalog.pg_am fk
WHERE	amhandler != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.amhandler);
SELECT	ctid, amopfamily
FROM	pg_catalog.pg_amop fk
WHERE	amopfamily != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opfamily pk WHERE pk.oid = fk.amopfamily);
SELECT	ctid, amoplefttype
FROM	pg_catalog.pg_amop fk
WHERE	amoplefttype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.amoplefttype);
SELECT	ctid, amoprighttype
FROM	pg_catalog.pg_amop fk
WHERE	amoprighttype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.amoprighttype);
SELECT	ctid, amopopr
FROM	pg_catalog.pg_amop fk
WHERE	amopopr != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.amopopr);
SELECT	ctid, amopmethod
FROM	pg_catalog.pg_amop fk
WHERE	amopmethod != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_am pk WHERE pk.oid = fk.amopmethod);
SELECT	ctid, amopsortfamily
FROM	pg_catalog.pg_amop fk
WHERE	amopsortfamily != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opfamily pk WHERE pk.oid = fk.amopsortfamily);
SELECT	ctid, amprocfamily
FROM	pg_catalog.pg_amproc fk
WHERE	amprocfamily != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opfamily pk WHERE pk.oid = fk.amprocfamily);
SELECT	ctid, amproclefttype
FROM	pg_catalog.pg_amproc fk
WHERE	amproclefttype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.amproclefttype);
SELECT	ctid, amprocrighttype
FROM	pg_catalog.pg_amproc fk
WHERE	amprocrighttype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.amprocrighttype);
SELECT	ctid, amproc
FROM	pg_catalog.pg_amproc fk
WHERE	amproc != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.amproc);
SELECT	ctid, adrelid
FROM	pg_catalog.pg_attrdef fk
WHERE	adrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.adrelid);
SELECT	ctid, attrelid
FROM	pg_catalog.pg_attribute fk
WHERE	attrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.attrelid);
SELECT	ctid, atttypid
FROM	pg_catalog.pg_attribute fk
WHERE	atttypid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.atttypid);
SELECT	ctid, attcollation
FROM	pg_catalog.pg_attribute fk
WHERE	attcollation != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.attcollation);
SELECT	ctid, roleid
FROM	pg_catalog.pg_auth_members fk
WHERE	roleid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.roleid);
SELECT	ctid, member
FROM	pg_catalog.pg_auth_members fk
WHERE	member != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.member);
SELECT	ctid, grantor
FROM	pg_catalog.pg_auth_members fk
WHERE	grantor != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.grantor);
SELECT	ctid, castsource
FROM	pg_catalog.pg_cast fk
WHERE	castsource != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.castsource);
SELECT	ctid, casttarget
FROM	pg_catalog.pg_cast fk
WHERE	casttarget != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.casttarget);
SELECT	ctid, castfunc
FROM	pg_catalog.pg_cast fk
WHERE	castfunc != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.castfunc);
SELECT	ctid, relnamespace
FROM	pg_catalog.pg_class fk
WHERE	relnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.relnamespace);
SELECT	ctid, reltype
FROM	pg_catalog.pg_class fk
WHERE	reltype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.reltype);
SELECT	ctid, reloftype
FROM	pg_catalog.pg_class fk
WHERE	reloftype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.reloftype);
SELECT	ctid, relowner
FROM	pg_catalog.pg_class fk
WHERE	relowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.relowner);
SELECT	ctid, relam
FROM	pg_catalog.pg_class fk
WHERE	relam != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_am pk WHERE pk.oid = fk.relam);
SELECT	ctid, reltablespace
FROM	pg_catalog.pg_class fk
WHERE	reltablespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_tablespace pk WHERE pk.oid = fk.reltablespace);
SELECT	ctid, reltoastrelid
FROM	pg_catalog.pg_class fk
WHERE	reltoastrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.reltoastrelid);
SELECT	ctid, collnamespace
FROM	pg_catalog.pg_collation fk
WHERE	collnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.collnamespace);
SELECT	ctid, collowner
FROM	pg_catalog.pg_collation fk
WHERE	collowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.collowner);
SELECT	ctid, connamespace
FROM	pg_catalog.pg_constraint fk
WHERE	connamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.connamespace);
SELECT	ctid, conrelid
FROM	pg_catalog.pg_constraint fk
WHERE	conrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.conrelid);
SELECT	ctid, contypid
FROM	pg_catalog.pg_constraint fk
WHERE	contypid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.contypid);
SELECT	ctid, conindid
FROM	pg_catalog.pg_constraint fk
WHERE	conindid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.conindid);
SELECT	ctid, conparentid
FROM	pg_catalog.pg_constraint fk
WHERE	conparentid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_constraint pk WHERE pk.oid = fk.conparentid);
SELECT	ctid, confrelid
FROM	pg_catalog.pg_constraint fk
WHERE	confrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.confrelid);
SELECT	ctid, connamespace
FROM	pg_catalog.pg_conversion fk
WHERE	connamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.connamespace);
SELECT	ctid, conowner
FROM	pg_catalog.pg_conversion fk
WHERE	conowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.conowner);
SELECT	ctid, conproc
FROM	pg_catalog.pg_conversion fk
WHERE	conproc != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.conproc);
SELECT	ctid, datdba
FROM	pg_catalog.pg_database fk
WHERE	datdba != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.datdba);
SELECT	ctid, dattablespace
FROM	pg_catalog.pg_database fk
WHERE	dattablespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_tablespace pk WHERE pk.oid = fk.dattablespace);
SELECT	ctid, setdatabase
FROM	pg_catalog.pg_db_role_setting fk
WHERE	setdatabase != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_database pk WHERE pk.oid = fk.setdatabase);
SELECT	ctid, classid
FROM	pg_catalog.pg_depend fk
WHERE	classid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.classid);
SELECT	ctid, refclassid
FROM	pg_catalog.pg_depend fk
WHERE	refclassid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.refclassid);
SELECT	ctid, classoid
FROM	pg_catalog.pg_description fk
WHERE	classoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.classoid);
SELECT	ctid, enumtypid
FROM	pg_catalog.pg_enum fk
WHERE	enumtypid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.enumtypid);
SELECT	ctid, extowner
FROM	pg_catalog.pg_extension fk
WHERE	extowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.extowner);
SELECT	ctid, extnamespace
FROM	pg_catalog.pg_extension fk
WHERE	extnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.extnamespace);
SELECT	ctid, fdwowner
FROM	pg_catalog.pg_foreign_data_wrapper fk
WHERE	fdwowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.fdwowner);
SELECT	ctid, srvowner
FROM	pg_catalog.pg_foreign_server fk
WHERE	srvowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.srvowner);
SELECT	ctid, srvfdw
FROM	pg_catalog.pg_foreign_server fk
WHERE	srvfdw != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_foreign_data_wrapper pk WHERE pk.oid = fk.srvfdw);
SELECT	ctid, indexrelid
FROM	pg_catalog.pg_index fk
WHERE	indexrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.indexrelid);
SELECT	ctid, indrelid
FROM	pg_catalog.pg_index fk
WHERE	indrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.indrelid);
SELECT	ctid, inhrelid
FROM	pg_catalog.pg_inherits fk
WHERE	inhrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.inhrelid);
SELECT	ctid, inhparent
FROM	pg_catalog.pg_inherits fk
WHERE	inhparent != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.inhparent);
SELECT	ctid, classoid
FROM	pg_catalog.pg_init_privs fk
WHERE	classoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.classoid);
SELECT	ctid, lanowner
FROM	pg_catalog.pg_language fk
WHERE	lanowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.lanowner);
SELECT	ctid, lanplcallfoid
FROM	pg_catalog.pg_language fk
WHERE	lanplcallfoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.lanplcallfoid);
SELECT	ctid, laninline
FROM	pg_catalog.pg_language fk
WHERE	laninline != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.laninline);
SELECT	ctid, lanvalidator
FROM	pg_catalog.pg_language fk
WHERE	lanvalidator != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.lanvalidator);
SELECT	ctid, loid
FROM	pg_catalog.pg_largeobject fk
WHERE	loid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_largeobject_metadata pk WHERE pk.oid = fk.loid);
SELECT	ctid, lomowner
FROM	pg_catalog.pg_largeobject_metadata fk
WHERE	lomowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.lomowner);
SELECT	ctid, nspowner
FROM	pg_catalog.pg_namespace fk
WHERE	nspowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.nspowner);
SELECT	ctid, opcmethod
FROM	pg_catalog.pg_opclass fk
WHERE	opcmethod != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_am pk WHERE pk.oid = fk.opcmethod);
SELECT	ctid, opcnamespace
FROM	pg_catalog.pg_opclass fk
WHERE	opcnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.opcnamespace);
SELECT	ctid, opcowner
FROM	pg_catalog.pg_opclass fk
WHERE	opcowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.opcowner);
SELECT	ctid, opcfamily
FROM	pg_catalog.pg_opclass fk
WHERE	opcfamily != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opfamily pk WHERE pk.oid = fk.opcfamily);
SELECT	ctid, opcintype
FROM	pg_catalog.pg_opclass fk
WHERE	opcintype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.opcintype);
SELECT	ctid, opckeytype
FROM	pg_catalog.pg_opclass fk
WHERE	opckeytype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.opckeytype);
SELECT	ctid, oprnamespace
FROM	pg_catalog.pg_operator fk
WHERE	oprnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.oprnamespace);
SELECT	ctid, oprowner
FROM	pg_catalog.pg_operator fk
WHERE	oprowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.oprowner);
SELECT	ctid, oprleft
FROM	pg_catalog.pg_operator fk
WHERE	oprleft != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.oprleft);
SELECT	ctid, oprright
FROM	pg_catalog.pg_operator fk
WHERE	oprright != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.oprright);
SELECT	ctid, oprresult
FROM	pg_catalog.pg_operator fk
WHERE	oprresult != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.oprresult);
SELECT	ctid, oprcom
FROM	pg_catalog.pg_operator fk
WHERE	oprcom != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.oprcom);
SELECT	ctid, oprnegate
FROM	pg_catalog.pg_operator fk
WHERE	oprnegate != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.oprnegate);
SELECT	ctid, oprcode
FROM	pg_catalog.pg_operator fk
WHERE	oprcode != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.oprcode);
SELECT	ctid, oprrest
FROM	pg_catalog.pg_operator fk
WHERE	oprrest != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.oprrest);
SELECT	ctid, oprjoin
FROM	pg_catalog.pg_operator fk
WHERE	oprjoin != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.oprjoin);
SELECT	ctid, opfmethod
FROM	pg_catalog.pg_opfamily fk
WHERE	opfmethod != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_am pk WHERE pk.oid = fk.opfmethod);
SELECT	ctid, opfnamespace
FROM	pg_catalog.pg_opfamily fk
WHERE	opfnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.opfnamespace);
SELECT	ctid, opfowner
FROM	pg_catalog.pg_opfamily fk
WHERE	opfowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.opfowner);
SELECT	ctid, partrelid
FROM	pg_catalog.pg_partitioned_table fk
WHERE	partrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.partrelid);
SELECT	ctid, partdefid
FROM	pg_catalog.pg_partitioned_table fk
WHERE	partdefid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.partdefid);
SELECT	ctid, polrelid
FROM	pg_catalog.pg_policy fk
WHERE	polrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.polrelid);
SELECT	ctid, pronamespace
FROM	pg_catalog.pg_proc fk
WHERE	pronamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.pronamespace);
SELECT	ctid, proowner
FROM	pg_catalog.pg_proc fk
WHERE	proowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.proowner);
SELECT	ctid, prolang
FROM	pg_catalog.pg_proc fk
WHERE	prolang != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_language pk WHERE pk.oid = fk.prolang);
SELECT	ctid, provariadic
FROM	pg_catalog.pg_proc fk
WHERE	provariadic != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.provariadic);
SELECT	ctid, prosupport
FROM	pg_catalog.pg_proc fk
WHERE	prosupport != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.prosupport);
SELECT	ctid, prorettype
FROM	pg_catalog.pg_proc fk
WHERE	prorettype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.prorettype);
SELECT	ctid, rngtypid
FROM	pg_catalog.pg_range fk
WHERE	rngtypid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.rngtypid);
SELECT	ctid, rngsubtype
FROM	pg_catalog.pg_range fk
WHERE	rngsubtype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.rngsubtype);
SELECT	ctid, rngcollation
FROM	pg_catalog.pg_range fk
WHERE	rngcollation != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.rngcollation);
SELECT	ctid, rngsubopc
FROM	pg_catalog.pg_range fk
WHERE	rngsubopc != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opclass pk WHERE pk.oid = fk.rngsubopc);
SELECT	ctid, rngcanonical
FROM	pg_catalog.pg_range fk
WHERE	rngcanonical != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.rngcanonical);
SELECT	ctid, rngsubdiff
FROM	pg_catalog.pg_range fk
WHERE	rngsubdiff != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.rngsubdiff);
SELECT	ctid, ev_class
FROM	pg_catalog.pg_rewrite fk
WHERE	ev_class != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.ev_class);
SELECT	ctid, seqrelid
FROM	pg_catalog.pg_sequence fk
WHERE	seqrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.seqrelid);
SELECT	ctid, seqtypid
FROM	pg_catalog.pg_sequence fk
WHERE	seqtypid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.seqtypid);
SELECT	ctid, refclassid
FROM	pg_catalog.pg_shdepend fk
WHERE	refclassid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.refclassid);
SELECT	ctid, classoid
FROM	pg_catalog.pg_shdescription fk
WHERE	classoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.classoid);
SELECT	ctid, starelid
FROM	pg_catalog.pg_statistic fk
WHERE	starelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.starelid);
SELECT	ctid, staop1
FROM	pg_catalog.pg_statistic fk
WHERE	staop1 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.staop1);
SELECT	ctid, staop2
FROM	pg_catalog.pg_statistic fk
WHERE	staop2 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.staop2);
SELECT	ctid, staop3
FROM	pg_catalog.pg_statistic fk
WHERE	staop3 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.staop3);
SELECT	ctid, staop4
FROM	pg_catalog.pg_statistic fk
WHERE	staop4 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.staop4);
SELECT	ctid, staop5
FROM	pg_catalog.pg_statistic fk
WHERE	staop5 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.staop5);
SELECT	ctid, stacoll1
FROM	pg_catalog.pg_statistic fk
WHERE	stacoll1 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.stacoll1);
SELECT	ctid, stacoll2
FROM	pg_catalog.pg_statistic fk
WHERE	stacoll2 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.stacoll2);
SELECT	ctid, stacoll3
FROM	pg_catalog.pg_statistic fk
WHERE	stacoll3 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.stacoll3);
SELECT	ctid, stacoll4
FROM	pg_catalog.pg_statistic fk
WHERE	stacoll4 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.stacoll4);
SELECT	ctid, stacoll5
FROM	pg_catalog.pg_statistic fk
WHERE	stacoll5 != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.stacoll5);
SELECT	ctid, stxrelid
FROM	pg_catalog.pg_statistic_ext fk
WHERE	stxrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.stxrelid);
SELECT	ctid, stxnamespace
FROM	pg_catalog.pg_statistic_ext fk
WHERE	stxnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.stxnamespace);
SELECT	ctid, stxowner
FROM	pg_catalog.pg_statistic_ext fk
WHERE	stxowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.stxowner);
SELECT	ctid, stxoid
FROM	pg_catalog.pg_statistic_ext_data fk
WHERE	stxoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_statistic_ext pk WHERE pk.oid = fk.stxoid);
SELECT	ctid, spcowner
FROM	pg_catalog.pg_tablespace fk
WHERE	spcowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.spcowner);
SELECT	ctid, trftype
FROM	pg_catalog.pg_transform fk
WHERE	trftype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.trftype);
SELECT	ctid, trflang
FROM	pg_catalog.pg_transform fk
WHERE	trflang != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_language pk WHERE pk.oid = fk.trflang);
SELECT	ctid, trffromsql
FROM	pg_catalog.pg_transform fk
WHERE	trffromsql != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.trffromsql);
SELECT	ctid, trftosql
FROM	pg_catalog.pg_transform fk
WHERE	trftosql != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.trftosql);
SELECT	ctid, tgrelid
FROM	pg_catalog.pg_trigger fk
WHERE	tgrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.tgrelid);
SELECT	ctid, tgparentid
FROM	pg_catalog.pg_trigger fk
WHERE	tgparentid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_trigger pk WHERE pk.oid = fk.tgparentid);
SELECT	ctid, tgfoid
FROM	pg_catalog.pg_trigger fk
WHERE	tgfoid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.tgfoid);
SELECT	ctid, tgconstrrelid
FROM	pg_catalog.pg_trigger fk
WHERE	tgconstrrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.tgconstrrelid);
SELECT	ctid, tgconstrindid
FROM	pg_catalog.pg_trigger fk
WHERE	tgconstrindid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.tgconstrindid);
SELECT	ctid, tgconstraint
FROM	pg_catalog.pg_trigger fk
WHERE	tgconstraint != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_constraint pk WHERE pk.oid = fk.tgconstraint);
SELECT	ctid, cfgnamespace
FROM	pg_catalog.pg_ts_config fk
WHERE	cfgnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.cfgnamespace);
SELECT	ctid, cfgowner
FROM	pg_catalog.pg_ts_config fk
WHERE	cfgowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.cfgowner);
SELECT	ctid, cfgparser
FROM	pg_catalog.pg_ts_config fk
WHERE	cfgparser != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_ts_parser pk WHERE pk.oid = fk.cfgparser);
SELECT	ctid, mapcfg
FROM	pg_catalog.pg_ts_config_map fk
WHERE	mapcfg != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_ts_config pk WHERE pk.oid = fk.mapcfg);
SELECT	ctid, mapdict
FROM	pg_catalog.pg_ts_config_map fk
WHERE	mapdict != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_ts_dict pk WHERE pk.oid = fk.mapdict);
SELECT	ctid, dictnamespace
FROM	pg_catalog.pg_ts_dict fk
WHERE	dictnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.dictnamespace);
SELECT	ctid, dictowner
FROM	pg_catalog.pg_ts_dict fk
WHERE	dictowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.dictowner);
SELECT	ctid, dicttemplate
FROM	pg_catalog.pg_ts_dict fk
WHERE	dicttemplate != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_ts_template pk WHERE pk.oid = fk.dicttemplate);
SELECT	ctid, prsnamespace
FROM	pg_catalog.pg_ts_parser fk
WHERE	prsnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.prsnamespace);
SELECT	ctid, prsstart
FROM	pg_catalog.pg_ts_parser fk
WHERE	prsstart != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.prsstart);
SELECT	ctid, prstoken
FROM	pg_catalog.pg_ts_parser fk
WHERE	prstoken != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.prstoken);
SELECT	ctid, prsend
FROM	pg_catalog.pg_ts_parser fk
WHERE	prsend != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.prsend);
SELECT	ctid, prsheadline
FROM	pg_catalog.pg_ts_parser fk
WHERE	prsheadline != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.prsheadline);
SELECT	ctid, prslextype
FROM	pg_catalog.pg_ts_parser fk
WHERE	prslextype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.prslextype);
SELECT	ctid, tmplnamespace
FROM	pg_catalog.pg_ts_template fk
WHERE	tmplnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.tmplnamespace);
SELECT	ctid, tmplinit
FROM	pg_catalog.pg_ts_template fk
WHERE	tmplinit != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.tmplinit);
SELECT	ctid, tmpllexize
FROM	pg_catalog.pg_ts_template fk
WHERE	tmpllexize != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.tmpllexize);
SELECT	ctid, typnamespace
FROM	pg_catalog.pg_type fk
WHERE	typnamespace != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_namespace pk WHERE pk.oid = fk.typnamespace);
SELECT	ctid, typowner
FROM	pg_catalog.pg_type fk
WHERE	typowner != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_authid pk WHERE pk.oid = fk.typowner);
SELECT	ctid, typrelid
FROM	pg_catalog.pg_type fk
WHERE	typrelid != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_class pk WHERE pk.oid = fk.typrelid);
SELECT	ctid, typelem
FROM	pg_catalog.pg_type fk
WHERE	typelem != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.typelem);
SELECT	ctid, typarray
FROM	pg_catalog.pg_type fk
WHERE	typarray != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.typarray);
SELECT	ctid, typinput
FROM	pg_catalog.pg_type fk
WHERE	typinput != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typinput);
SELECT	ctid, typoutput
FROM	pg_catalog.pg_type fk
WHERE	typoutput != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typoutput);
SELECT	ctid, typreceive
FROM	pg_catalog.pg_type fk
WHERE	typreceive != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typreceive);
SELECT	ctid, typsend
FROM	pg_catalog.pg_type fk
WHERE	typsend != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typsend);
SELECT	ctid, typmodin
FROM	pg_catalog.pg_type fk
WHERE	typmodin != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typmodin);
SELECT	ctid, typmodout
FROM	pg_catalog.pg_type fk
WHERE	typmodout != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typmodout);
SELECT	ctid, typanalyze
FROM	pg_catalog.pg_type fk
WHERE	typanalyze != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_proc pk WHERE pk.oid = fk.typanalyze);
SELECT	ctid, typbasetype
FROM	pg_catalog.pg_type fk
WHERE	typbasetype != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.typbasetype);
SELECT	ctid, typcollation
FROM	pg_catalog.pg_type fk
WHERE	typcollation != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.typcollation);
SELECT	ctid, conpfeqop
FROM	(SELECT ctid, unnest(conpfeqop) AS conpfeqop FROM pg_catalog.pg_constraint) fk
WHERE	conpfeqop != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.conpfeqop);
SELECT	ctid, conppeqop
FROM	(SELECT ctid, unnest(conppeqop) AS conppeqop FROM pg_catalog.pg_constraint) fk
WHERE	conppeqop != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.conppeqop);
SELECT	ctid, conffeqop
FROM	(SELECT ctid, unnest(conffeqop) AS conffeqop FROM pg_catalog.pg_constraint) fk
WHERE	conffeqop != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.conffeqop);
SELECT	ctid, conexclop
FROM	(SELECT ctid, unnest(conexclop) AS conexclop FROM pg_catalog.pg_constraint) fk
WHERE	conexclop != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_operator pk WHERE pk.oid = fk.conexclop);
SELECT	ctid, indcollation
FROM	(SELECT ctid, unnest(indcollation) AS indcollation FROM pg_catalog.pg_index) fk
WHERE	indcollation != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.indcollation);
SELECT	ctid, indclass
FROM	(SELECT ctid, unnest(indclass) AS indclass FROM pg_catalog.pg_index) fk
WHERE	indclass != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opclass pk WHERE pk.oid = fk.indclass);
SELECT	ctid, partclass
FROM	(SELECT ctid, unnest(partclass) AS partclass FROM pg_catalog.pg_partitioned_table) fk
WHERE	partclass != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_opclass pk WHERE pk.oid = fk.partclass);
SELECT	ctid, partcollation
FROM	(SELECT ctid, unnest(partcollation) AS partcollation FROM pg_catalog.pg_partitioned_table) fk
WHERE	partcollation != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_collation pk WHERE pk.oid = fk.partcollation);
SELECT	ctid, proargtypes
FROM	(SELECT ctid, unnest(proargtypes) AS proargtypes FROM pg_catalog.pg_proc) fk
WHERE	proargtypes != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.proargtypes);
SELECT	ctid, proallargtypes
FROM	(SELECT ctid, unnest(proallargtypes) AS proallargtypes FROM pg_catalog.pg_proc) fk
WHERE	proallargtypes != 0 AND
	NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type pk WHERE pk.oid = fk.proallargtypes);
