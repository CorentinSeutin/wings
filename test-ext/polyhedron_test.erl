-module(polyhedron_test).

-compile(export_all).

-include_lib("wings/src/wings.hrl").

go()  -> start().

start() ->
    Res = run_test_compare_min_max(),
    ok.

run_test_compare_min_max() ->
    true.

