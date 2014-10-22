% @doc Mustache template renderer.
-module(stache).

% API
-export([render/2]).

%--- API ----------------------------------------------------------------------

% @doc Render a template using the supplied variables.
-spec render(Template::binary(), _) -> iolist().
render(Template, Vars) ->
    render(chunks(Template), Vars, [], []).

%--- Internal -----------------------------------------------------------------

chunks(Template) -> re:split(Template, <<"(\{\{(?:\{|/|&|#)?|\}\}\}?)">>).

render([], _Vars, Result, _Context) ->
    lists:reverse(Result);
render([<<>>|Template], Vars, Result, Context) ->
    render(Template, Vars, Result, Context);
render([<<"{{#">>, Var, <<"}}">>|Template], Vars, Result, Context) ->
    case maps:get(Var, Vars) of
        Value when is_boolean(Value), Value ->
            render(Template, Vars, Result, push(Var, Context));
        Value when is_boolean(Value) ->
            render(exit_block(Var, Template), Vars, Result, Context);
        Value when is_map(Value) ->
            render(Template, Vars, Result, push(Var, Context));
        Value when is_list(Value) ->
            repeat_over_list
    end;
render([<<"{{/">>, Var, <<"}}">>|Template], Vars, Result, Context) ->
    render(Template, Vars, Result, pop(Var, Context));
render([<<"{{&">>, Var, <<"}}">>|Template], Vars, Result, Context) ->
    Value = get(Context, Var, Vars),
    render(Template, Vars, [Value|Result], Context);
render([<<"{{">>, Var, <<"}}">>|Template], Vars, Result, Context) ->
    Value = get(Context, Var, Vars, [escape]),
    render(Template, Vars, [Value|Result], Context);
render([<<"{{{">>, Var, <<"}}}">>|Template], Vars, Result, Context) ->
    Value = get(Context, Var, Vars),
    render(Template, Vars, [Value|Result], Context);
render([Chunk|Template], Vars, Result, Context) ->
    render(Template, Vars, [Chunk|Result], Context).

get(Context, Var, Data) -> get(Context, Var, Data, []).

get(Context, Var, Data, Opts) ->
    case {find(Context ++ sections(Var), Data), Opts} of
        {{ok, Value}, _} when is_integer(Value) ->
            integer_to_binary(Value);
        {{ok, Value}, _} when is_float(Value) ->
            iolist_to_binary(io_lib:format("~p", [Value]));
        {{ok, Value}, [escape]} when is_binary(Value) ->
            escape(Value);
        {{ok, Value}, _} when is_binary(Value) ->
            Value;
        {{ok, Value}, _} when is_boolean(Value) ->
            Value;
        {error, _} ->
            <<>>
    end.

find([Var], Data)      -> maps:find(strip(Var), Data);
find([Var|Rest], Data) ->
    case maps:find(strip(Var), Data) of
        {ok, Value} -> find(Rest, Value);
        error -> error
    end.

exit_block(Var, [<<"{{/">>, Var, <<"}}">>|Template]) ->
    Template;
exit_block(Var, [_Chunk|Template]) ->
    exit_block(Var, Template).

sections(Var) -> re:split(Var, <<"\\.">>).

escape(Bin) -> << <<(escape_char(C))/binary>> || <<C>> <= Bin>>.

strip(Bin) ->
    re:replace(Bin, <<"(^\\s+)|(\\s+$)">>, <<>>, [global, {return, binary}]).

escape_char($&) -> <<"&amp;">>;
escape_char($") -> <<"&quot;">>;
escape_char($<) -> <<"&lt;">>;
escape_char($>) -> <<"&gt;">>;
escape_char(C)  -> <<C>>.


push(Var, Context) -> Context ++ [Var].

pop(Var, [Var])           -> [];
pop(Var, [Other|Context]) -> [Other|pop(Var, Context)].
