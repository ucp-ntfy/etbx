-module(etbx_tests).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

foo() ->
    foo.

maybe_apply_test_() ->
    [?_assertEqual(foo,       etbx:maybe_apply(?MODULE, foo, [])),
     ?_assertEqual(undefined, etbx:maybe_apply(?MODULE, bar, []))].

