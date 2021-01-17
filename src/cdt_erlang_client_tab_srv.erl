-module(cdt_erlang_client_tab_srv).

-behaviour(gen_server).

-export([listen_ws/1, unlisten_ws/1]).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    id :: binary(),
    url :: binary(),
    wsDUrl :: binary(),

    creator_pid :: pid(),
    creator_mon_ref :: reference(),

    conn_pid :: pid(),
    stream_ref,

    listen_ws = false :: boolean()
}).

-include("cdt_erlang_client_types.hrl").
-include("cdt_erlang_client_cdt_types.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(TabConfig :: map(), Creator :: pid()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}.
start_link(TabConfig, CreatorPid) ->
    gen_server:start_link(?MODULE, [TabConfig, CreatorPid], []).

%%--------------------------------------------------------------------

-spec listen_ws(TabPid :: pid()) -> ok.
listen_ws(TabPid) ->
    gen_server:call(TabPid, listen_ws).

%%--------------------------------------------------------------------

-spec unlisten_ws(TabPid :: pid()) -> ok.
unlisten_ws(TabPid) ->
    gen_server:call(TabPid, unlisten_ws).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore.
init([#{
    <<"id">> := Id,
    <<"url">> := Url,
    <<"webSocketDebuggerUrl">> := WsDUrl
}, CreatorPid]) ->
    ?LOG_INFO("Starting new tab ~p ~p", [self(), Url]),
    {ok, {_, _, Host, Port, Path, _}} = http_uri:parse(WsDUrl),
    {ok, ConnPid} = gun:open(binary_to_list(Host), Port),
    {ok, _Proto} = gun:await_up(ConnPid),
    StreamRef = gun:ws_upgrade(ConnPid, binary_to_list(Path)),
    StreamRef = receive
        {gun_upgrade, ConnPid, StreamRef2, [<<"websocket">>], _Headers} ->
            StreamRef2;
        {gun_response, ConnPid, _, _, Status, Headers} ->
            exit({ws_upgrade_failed, Status, Headers});
        {gun_error, ConnPid, StreamRef, Reason} ->
            exit({ws_upgrade_failed, Reason})
    after 3000 ->
        exit(timeout)
    end,
    CreatorMonRef = erlang:monitor(process, CreatorPid),
    {ok, #state{
        id = Id,
        url = Url,
        wsDUrl = WsDUrl,
        creator_pid = CreatorPid,
        creator_mon_ref = CreatorMonRef,
        conn_pid = ConnPid,
        stream_ref = StreamRef
    }}.

%%--------------------------------------------------------------------

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {noreply, NewState :: #state{}}.
handle_call(listen_ws, _From, State = #state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    ok = gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(#{<<"id">> => 1, <<"method">> => ?CDT_NETWORK_ENABLE})}),
    {reply, ok, State#state{listen_ws = true}};
handle_call(unlisten_ws,  _From, State = #state{conn_pid = ConnPid, stream_ref = StreamRef}) ->
    ok = gun:ws_send(ConnPid, StreamRef, {text, jsx:encode(#{<<"id">> => 2, <<"method">> => ?CDT_NETWORK_DISABLE})}),
    {reply, ok, State#state{listen_ws = false}};
handle_call(Request, _From, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_cast(Request :: term(), State :: #state{}) -> {noreply, NewState :: #state{}}.
handle_cast(Request, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Request]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}.
handle_info({gun_ws, _, _, {text, CDTData}}, State = #state{listen_ws = true, creator_pid = CreatorPid}) ->
    CDTDataDecoded = jsx:decode(CDTData),
    case CDTDataDecoded of
        #{
            <<"method">> := ?CDT_WEBSOCKET_FRAME_RECEIVED,
            <<"params">> := #{
                <<"response">> := #{
                    <<"payloadData">> := Payload
                }
            }
        } ->
            erlang:send(CreatorPid, {?CDTEC_WS_FRAME_RECEIVED, Payload}),
            ok;
        _ ->
            ok
    end,
    {noreply, State};
handle_info({gun_ws, _, _, _}, State = #state{listen_ws = false}) ->
    {noreply, State};
handle_info({gun_down, _, _, _, _}, State) ->
    ?LOG_INFO("Tab ~p ws closed", [self()]),
    {stop, normal, State};
handle_info({'DOWN', CreatorMonRef, process, CreatorPid, _Reason}, State = #state{
    creator_mon_ref = CreatorMonRef, creator_pid = CreatorPid
}) ->
    ?LOG_INFO("Tab ~p creator down", [self()]),
    {stop, normal, State};
handle_info(Info, State = #state{}) ->
    ?LOG_WARNING("Unhandled ~p", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------

-spec terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()), State :: #state{}) -> term().
terminate(Reason, _State = #state{}) ->
    ?LOG_INFO("Tab ~p terminated with reason ~p", [self(), Reason]),
    ok.

%%--------------------------------------------------------------------

-spec code_change(OldVsn :: term() | {down, term()}, State :: #state{}, Extra :: term()) -> {ok, NewState :: #state{}}.
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
