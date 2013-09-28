-module(etbx_tests).
-include("etbx.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-record(frob, {foo, bar="bar", baz}).

foo() ->
    foo.

maybe_apply_test_() ->
    [?_assertEqual(foo,       etbx:maybe_apply(?MODULE, foo, [])),
     ?_assertEqual(undefined, etbx:maybe_apply(?MODULE, bar, []))].

is_nil_test_() ->
    [?_assertEqual(true,  etbx:is_nil("")),
     ?_assertEqual(true,  etbx:is_nil([])),
     ?_assertEqual(true,  etbx:is_nil(<<>>)),
     ?_assertEqual(true,  etbx:is_nil(undefined)),
     ?_assertEqual(true,  etbx:is_nil()),
     ?_assertEqual(true,  etbx:is_nil(<<"">>)),
     ?_assertEqual(false, etbx:is_nil(0)),
     ?_assertEqual(false, etbx:is_nil([undefined])),
     ?_assertEqual(false, etbx:is_nil([[]]))].

index_of_test_() ->
    [?_assertEqual(undefined, etbx:index_of(foo, [])),
     ?_assertEqual(0,         etbx:index_of(foo, [foo, bar])),
     ?_assertEqual(1,         etbx:index_of(foo, [bar, foo])),
     ?_assertEqual(1,         etbx:index_of(foo, [baz, foo, bar])),
     ?_assertEqual(2,         etbx:index_of(foo, [baz, bar, foo])),
     ?_assertEqual(undefined, etbx:index_of(foo, [baz, bar]))].

to_rec_test_() ->
    [?_assertEqual(#frob{foo="foo", bar="bar", baz="baz"}, 
                   etbx:to_rec(?RECSPEC(frob), [{baz, "baz"}, {foo, "foo"}])),
     ?_assertEqual(#frob{},
                   etbx:to_rec(?RECSPEC(frob), [])),
     ?_assertEqual(#frob{},
                   etbx:to_rec(?RECSPEC(frob), [{bad, "bad"}]))].
