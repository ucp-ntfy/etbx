-module(etbx_tests).
-include("etbx.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-record(frob, {foo, bar, baz}).

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

to_rec_test_() ->
    [?_assertEqual(#frob{foo="foo", bar="bar", baz="baz"}, 
                   etbx:to_rec(?RECSPEC(frob), 
                               [{baz, "baz"}, {foo, "foo"}, {bar, "bar"}])),
     ?_assertEqual(#frob{foo="foo", bar="bar", baz="baz"}, 
                   ?TO_REC(frob, [{baz, "baz"}, {foo, "foo"}, {bar, "bar"}]))].
