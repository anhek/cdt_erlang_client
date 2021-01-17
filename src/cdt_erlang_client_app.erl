-module(cdt_erlang_client_app).

-behaviour(application).

-export([start/2, stop/1]).

%%%===================================================================

-spec start(StartType :: term(), StartArgs :: any()) -> {ok, pid()} | {ok, pid(), State :: any()} | {error, any()}.
start(_StartType, _StartArgs) ->
    cdt_erlang_client_sup:start_link().

%%--------------------------------------------------------------------

-spec stop(State :: term()) -> any().
stop(_State) ->
    ok.
