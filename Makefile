all:
	escript priv/build

test:
	erl -pa "ebin" -noshell -eval "\
	elm_string:test(),\
	elm_library_git:test(),\
	elm_library_svn:test(),\
	elm_library_hg:test()" -s init stop

clean:
	@echo "removing:"
	@rm -fv ebin/*.beam
