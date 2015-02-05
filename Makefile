.PHONY: dev test

dev:
	erl -pa ebin/ -pa deps/*/ebin/ -config rel/files/sys.config \
	-args_file rel/files/vm.args \
	-eval "[application:start(App) || App <- [compiler, syntax_tools, \
		eenum, of_protocol, goldrush, lager, mnesia, of_driver, \
		ofs_handler, ls]]"

test:
	ct_run -dir tests -logdir logs -pa ebin -pa deps/*/ebin
