-module(stache_tests).

-include_lib("eunit/include/eunit.hrl").

% API
-export([spec_test_/0]).

%--- API ----------------------------------------------------------------------

spec_test_() ->
    {ok, Contents} = file:read_file("../test/mustache_spec/specs/interpolation.json"),
    JSON = jiffy:decode(Contents, [return_maps]),
    io:format(user, "~p~n", [JSON]),
    [generate(T) || T <- maps:get(<<"tests">>, JSON)].

%--- Internal -----------------------------------------------------------------

generate(Test) ->
    {maps:get(<<"desc">>, Test), fun() -> run(Test) end}.

run(#{<<"template">> := T, <<"data">> := D, <<"expected">> := E} = Test) ->
    io:format("Expected: ~p~nTemplate: ~p~nData:     ~p~n", [E, T, D]),
    ?assertEqual(E, stache:render(T, D)).
