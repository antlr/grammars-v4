# Makefile.local has system-dependent definitions of
# IXML, XSLT, and the location of some stylesheets,
# in order to keep this Makefile more system-neutral.
include Makefile.local

PO2013=../Project-Oberon-2013-materials

# ................................................................
# Making grammar Oberon.0.{ixml,xml}
# ................................................................

## Make Oberon.0, Wirth's grammar with no changes.  First in XML, by
## just parsing his EBNF as ixml.
../Grammars/Oberon.0.ixml.xml : $(PO2013)/OberonSyntax.Text.txt \
                           ./ixml-from-EBNF-W.ixml
	$(IXML) $< ixml-from-EBNF-W.ixml $@

../Grammars/Oberon.0.ixml.pp.xml : $(PO2013)/OberonSyntax.Text.txt \
                              ./ixml-from-EBNF-W.ixml
	$(IXML) $< ixml-from-EBNF-W.ixml $@ --pretty-print

## Then in ixml, by serializing the XML form.
../Grammars/Oberon.0.ixml: ../XML/Oberon.0.ixml.xml
	$(XSLT) $< $(GINGERSNAP)/serialize-as-ixml.xsl $@

## We can make that a pattern:
../Grammars/%.ixml: ../XML/%.ixml.xml
	$(XSLT) $< $(GINGERSNAP)/serialize-as-ixml.xsl $@


# ................................................................
# Making grammar Oberon.1.{ixml,xml}
# ................................................................

## Make Oberon.1, a mechanical adaptation of Wirth's grammar.
## Put the start symbol first, add whitespace after literals
## and after low-level tokens, hide the internal structure of
## low-level tokens.

### Make temporary versions in this directory
tmp.Oberon.1.xml: ../XML/Oberon.0.ixml.xml
	$(XSLT) $< $(GINGERSNAP)/inject-whitespace.xsl $@ \
		start='module' \
		tokens='ident+integer+real+CharConstant+string' \
		hide='letter+character+digit+hexDigit' \
		make-token-rules='true' \
		literal-names=`pwd`"/name-the-literals.xml"

tmp.Oberon.1.ixml: tmp.Oberon.1.xml
	$(XSLT) $< $(GINGERSNAP)/serialize-as-ixml.xsl $@

### Demand manual intervention to make the copies in the
### Grammars and XML directories
../Grammars/Oberon.1.xml: tmp.Oberon.1.xml
	@echo "Manual intervention needed to make Oberon.1.xml: "
	@echo "(1) edit tmp.Oberon.1.xml, save, convert to ixml"
	@echo "or"
	@echo "(2) edit tmp.Oberon.1.ixml, save, convert to XML"
../Grammars/Oberon.1.ixml: tmp.Oberon.1.ixml
	@echo "Manual intervention needed to make Oberon.1.ixml: "
	@echo "(1) edit tmp.Oberon.1.xml, save, convert to ixml"
	@echo "or"
	@echo "(2) edit tmp.Oberon.1.ixml, save, convert to XML"

### Make it relatively easy to turn either form into the other,
### without clobbering something that may have been the result
### of manual intervention.
../Grammars/Oberon.1.dup.xml: ../Grammars/Oberon.1.ixml
	$(IXML) $< $(SPECGRAMMAR) $@
	@echo You should compare Grammars/Oberon.{1,1.dup}.xml
../Grammars/Oberon.1.dup.ixml: ../Grammars/Oberon.1.xml
	$(XSLT) $< $(GINGERSNAP)/serialize-as-ixml.xsl $@
	@echo You should compare Grammars/Oberon.{1,1.dup}.ixml

# ................................................................
# Parsing with Oberon.1 grammar
# ................................................................
../XML/%.Mod.1.xml: $(PO2013)/%.Mod.txt ../Grammars/Oberon.1.ixml
	time $(IXML) $< ../Grammars/Oberon.1.ixml $@ --gll
../XML/%.Mod.1.pp.xml: $(PO2013)/%.Mod.txt ../Grammars/Oberon.1.ixml
	time $(IXML) $< ../Grammars/Oberon.1.ixml $@ -pp --gll

all_1_trees:  ../XML/ORB.Mod.1.xml \
	../XML/ORG.Mod.1.xml \
	../XML/ORP.Mod.1.xml \
	../XML/ORS.Mod.1.xml \
	../XML/ORTool.Mod.1.xml

all_1_pretty:  ../XML/ORB.Mod.1.pp.xml \
	../XML/ORG.Mod.1.pp.xml \
	../XML/ORP.Mod.1.pp.xml \
	../XML/ORS.Mod.1.pp.xml \
	../XML/ORTool.Mod.1.pp.xml

# ................................................................
# Making grammar Oberon.{ixml,xml}
# ................................................................

## Oberon.ixml is a non-mechanical adaptation and requires
## manual intervention.  No rules here.

## But if one form is modified, let's make it easy to get
## the other form.
../Grammars/Oberon.dup.xml: ../Grammars/Oberon.ixml
	$(IXML) $< $(SPECGRAMMAR) $@
	@echo You should compare Grammars/Oberon.{,dup}.xml
../Grammars/Oberon.dup.ixml: ../Grammars/Oberon.xml
	$(XSLT) $< $(GINGERSNAP)/serialize-as-ixml.xsl $@
	@echo You should compare Grammars/Oberon.{,dup}.ixml

# ................................................................
# Parsing with Oberon.ixml grammar
# ................................................................
../XML/%.Mod.xml: $(PO2013)/%.Mod.txt ../Grammars/Oberon.ixml
	time $(IXML) $< ../Grammars/Oberon.ixml $@ --gll
../XML/%.Mod.pp.xml: $(PO2013)/%.Mod.txt ../Grammars/Oberon.ixml
	time $(IXML) $< ../Grammars/Oberon.ixml $@ -pp --gll

all_trees:  ../XML/ORB.Mod.xml \
	../XML/ORG.Mod.xml \
	../XML/ORP.Mod.xml \
	../XML/ORS.Mod.xml \
	../XML/ORTool.Mod.xml

all_pretty:  ../XML/ORB.Mod.pp.xml \
	../XML/ORG.Mod.pp.xml \
	../XML/ORP.Mod.pp.xml \
	../XML/ORS.Mod.pp.xml \
	../XML/ORTool.Mod.pp.xml


# ................................................................
# Misc
# ................................................................

# Sanity checks.  As a test, let's parse Wirth's EBNF with his own
# grammar for EBNF, once unindented and once indented.
tmp.Oberon.0.ebnf.xml : $(PO2013)/OberonSyntax.Text.txt ./EBNF-W.ixml
	$(IXML) $< EBNF-W.ixml $@

tmp.Oberon.0.ebnf.pp.xml : $(PO2013)/OberonSyntax.Text.txt ./EBNF-W.ixml
	$(IXML) $< EBNF-W.ixml $@ --pretty-print

