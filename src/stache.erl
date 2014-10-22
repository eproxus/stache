% @doc Mustache template renderer.
-module(stache).

% API
-export([render/2]).

%--- API ----------------------------------------------------------------------

% @doc Render a template using the supplied variables.
-spec render(Template::binary(), _) -> iolist().
render(Template, Vars) ->
    render(chunks(Template), Vars, <<>>).

%--- Internal -----------------------------------------------------------------

chunks(Template) -> re:split(Template, <<"(\{\{\{?|\}\}\}?)">>).

render([], _Vars, Result) ->
    Result;
render([<<"{{">>, <<$&, Var/binary>>, <<"}}">>|Template], Vars, Result) ->
    Value = get(Var, Vars),
    render(Template, Vars, [Result, Value]);
render([<<"{{">>, Var, <<"}}">>|Template], Vars, Result) ->
    Value = get(Var, Vars, [escape]),
    render(Template, Vars, [Result, Value]);
render([<<"{{{">>, Var, <<"}}}">>|Template], Vars, Result) ->
    Value = get(Var, Vars),
    render(Template, Vars, [Result, Value]);
render([Chunk|Template], Vars, Result) ->
    render(Template, Vars, [Result, Chunk]).

get(Var, Data) -> get(Var, Data, []).

get(Var, Data, Opts) ->
    case {find(sections(Var), Data), Opts} of
        {{ok, Value}, _} when is_integer(Value) ->
            integer_to_binary(Value);
        {{ok, Value}, _} when is_float(Value) ->
            iolist_to_binary(io_lib:format("~p", [Value]));
        {{ok, Value}, [escape]} when is_binary(Value) ->
            escape(Value);
        {{ok, Value}, _} when is_binary(Value) ->
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

sections(Var) -> re:split(Var, <<"\\.">>).

escape(Bin) -> << <<(escape_char(C))/binary>> || <<C>> <= Bin>>.

strip(Bin) ->
    re:replace(Bin, <<"(^\\s+)|(\\s+$)">>, <<>>, [global, {return, binary}]).

escape_char($&) -> <<"&amp;">>;
escape_char($") -> <<"&quot;">>;
escape_char($<) -> <<"&lt;">>;
escape_char($>) -> <<"&gt;">>;
escape_char(C)  -> <<C>>.
