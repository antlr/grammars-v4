# Generated from trgen <version>
default:
	bash build.sh
clean:
	bash clean.sh
run:
	@trwdog ./<if(os_win)>Test.exe<else>Test<endif> $(RUNARGS)
FORCE: ;
test: FORCE
	bash test.sh
