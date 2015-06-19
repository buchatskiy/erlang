%% Feel free to use, reuse and abuse the code in this file.

%% @doc Hello world handler.
-module(json_server_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
        {Method, Req2} = cowboy_req:method(Req),
	HasBody = cowboy_req:has_body(Req2),
	{ok, Req3} = maybe_echo(Method, HasBody, Req2),
	{ok, Req3, State}.

maybe_echo(<<"POST">>, true, Req) ->
	{ok, PostVals, Req2} = cowboy_req:body_qs(Req),
	try
            [{BinJson, true}] = PostVals,
            {Json} = jsone:decode(BinJson),
            Action = proplists:get_value(<<"action">>, Json),
            RespBody = case parse_command(Action, Json) of
                {ok, OkRes} -> jsone:encode([<<"ok">>, OkRes]);
                {error, ErrorRes} -> jsone:encode([<<"error">>, ErrorRes]);
                UndefRes -> term_to_binary(UndefRes)
            end,
            cowboy_req:reply(200, [
                              {<<"content-type">>, <<"text/plain; charset=utf-8">>}
                              ], RespBody, Req2)
        catch
            _:_ -> cowboy_req:reply(500, [], <<"Wrong JSON format">>, Req2)
        end;
maybe_echo(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
maybe_echo(_, _, Req) ->
	%% Method not allowed.
	cowboy_req:reply(405, Req).

parse_command(<<"insert">>, Json) ->
    Key = proplists:get_value(<<"key">>, Json),
    Value = proplists:get_value(<<"value">>, Json),
    cache_server:insert(Key, Value);

parse_command(<<"lookup">>, Json) ->
    Key = proplists:get_value(<<"key">>, Json),
    cache_server:lookup(Key);

parse_command(<<"lookup_by_date">>, Json) ->
    DateFrom = binary_to_datetime(proplists:get_value(<<"date_from">>, Json)),
    DateTo = binary_to_datetime(proplists:get_value(<<"date_to">>, Json)),
    cache_server:lookup_by_date(DateFrom, DateTo);

parse_command(_, _) ->
    {error, <<"wrong_comand">>}.

binary_to_datetime(BinDate) ->
    [Date, Time] = bs03:split(BinDate, <<" ">>),
    [Year, Month, Day] = bs03:split(Date, <<"/">>),
    [Hour, Minute, Sec] = bs03:split(Time, <<":">>),
    {{binary_to_integer(Year),
      binary_to_integer(Month),
      binary_to_integer(Day)},
     {binary_to_integer(Hour),
      binary_to_integer(Minute),
      binary_to_integer(Sec)}}.

terminate(_Reason, _Req, _State) ->
	ok.
