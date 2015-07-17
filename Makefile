.PHONY: dev test

dev:
	erl -pa ebin/ -pa deps/*/ebin/ -config rel/files/sys.config \
	-args_file rel/files/vm.args \
	-eval "application:ensure_all_started(ls)"

test:
	ct_run -dir tests -logdir logs -pa ebin -pa deps/*/ebin

plot:
	python scripts/exo_lager_to_gnuplot.py
	gnuplot scripts/plot.plg
