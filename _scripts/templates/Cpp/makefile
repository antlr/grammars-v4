# Generated from trgen <version>
default:
	bash build.sh
run:
	@trwdog ./build/<if(os_win)>Release/<endif><exec_name> $(RUNARGS)
clean:
	bash clean.sh
FORCE: ;
test: FORCE
	bash test.sh
