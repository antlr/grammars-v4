# Template generated code from trgen <version>
default: setup
	rm -rf build; mkdir build; cd build; cmake .. <cmake_target>; $(MAKE)
setup:
	if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
run:
	cd build; trwdog ./<exec_name> $(RUNARGS)
clean:
	rm -rf build <temp_dir>/antlr4_runtime
test:
	bash test.sh
