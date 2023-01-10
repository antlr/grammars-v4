# Template generated code from trgen <version>
default: setup
	rm -rf build; mkdir build; cd build; cmake .. <cmake_target>; <if(os_win)>cmake --build . --config Release<else>$(MAKE)<endif>
setup:
	if [ -f transformGrammar.py ]; then python3 transformGrammar.py ; fi
run:
	@trwdog ./build/<if(os_win)>Release/<endif><exec_name> $(RUNARGS)
clean:
	rm -rf build <temp_dir>/antlr4_runtime
test:
	bash test.sh
