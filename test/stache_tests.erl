-module(stache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SPEC_PATH, "../test/mustache_spec/specs/").

%--- Tests --------------------------------------------------------------------

spec_test_() ->
    Files = ["interpolation"],
    {inparallel, [
        generate(Test) ||
            File <- Files,
            Test <- maps:get(<<"tests">>, decode(read_file(File)))
    ]}.

%--- Internal -----------------------------------------------------------------

generate(Test) ->
    {maps:get(<<"desc">>, Test), fun() -> run(Test) end}.

run(#{<<"template">> := T, <<"data">> := D, <<"expected">> := E}) ->
    io:format("Expected: ~p~nTemplate: ~p~nData:     ~p~n", [E, T, D]),
    ?assertEqual(E, iolist_to_binary(stache:render(T, D))).

read_file(File) ->
    {ok, Contents} = file:read_file(?SPEC_PATH ++ File ++ ".json"),
    Contents.

decode(Contents) -> jiffy:decode(Contents, [return_maps]).
