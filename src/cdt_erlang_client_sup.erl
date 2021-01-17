-module(cdt_erlang_client_sup).

-behaviour(supervisor).

-export([start_tab_srv/1]).
-export([start_link/0]).
-export([init/1]).

%%%===================================================================

-spec start_tab_srv(TabInfo :: map()) -> cdt_erlang_client:tab().
start_tab_srv(TabInfo) ->
    CreatorPid = self(),
    Id = maps:get(<<"id">>, TabInfo),
    case supervisor:start_child(?MODULE, #{
        id => {cdt_erlang_client_tab_srv, CreatorPid, Id},
        start => {cdt_erlang_client_tab_srv, start_link, [TabInfo, CreatorPid]},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => [cdt_erlang_client_tab_srv]
    }) of
        {ok, Pid} -> {Id, Pid};
        {error, {already_started, Pid}} -> {Id, Pid}
    end.

%%--------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init(term()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, {
        #{
            strategy => one_for_one,
            intensity => 5,
            period => 30
        },
        []
    }}.
