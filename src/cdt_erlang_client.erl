-module(cdt_erlang_client).

%% API
-export([
    new_tab/1,
    get_tab/1,
    listen_ws/1,
    unlisten_ws/1
]).

-export_type([tab/0]).
-type tab() :: {Id :: binary(), pid()}.

-include("cdt_erlang_client_types.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec get_tab(Regexp :: iodata()) -> [tab()].
get_tab(Regexp) ->
    ChromeHost = application:get_env(cdt_erlang_client, chrome_host, "localhost"),
    ChromePort = application:get_env(cdt_erlang_client, chrome_port, 9222),
    ChromeUrl = "http://"++ChromeHost++":"++integer_to_list(ChromePort)++"/json/list",
    {ok, {_, _, ChromeTabs}} = httpc:request(get, {ChromeUrl, []}, [], [{body_format, binary}]),
    ChromeTabsDecoded = jsx:decode(ChromeTabs),
    {ok, MP} = re:compile(Regexp),
    MatchFun = fun(Url, MP2) ->
        case re:run(http_uri:decode(Url), MP2) of
            {match, _} -> true;
            nomatch -> false
        end
    end,
    ChromeTabsFiltered = lists:filter(fun(#{<<"type">> := Type, <<"url">> := Url}) ->
        Type == <<"page">> andalso MatchFun(Url, MP)
    end, ChromeTabsDecoded),
    lists:map(fun(TabInfo) ->
        cdt_erlang_client_sup:start_tab_srv(TabInfo)
    end, ChromeTabsFiltered).

%%--------------------------------------------------------------------

-spec new_tab(TabUrl :: string()) -> tab().
new_tab(TabUrl) ->
    ChromeHost = application:get_env(cdt_erlang_client, chrome_host, "localhost"),
    ChromePort = application:get_env(cdt_erlang_client, chrome_port, 9222),
    ChromeUrl = "http://"++ChromeHost++":"++integer_to_list(ChromePort)++"/json/new?"++http_uri:encode(TabUrl),
    {ok, {_, _, TabInfo}} = httpc:request(get, {ChromeUrl, []}, [], [{body_format, binary}]),
    TabInfoDecoded = jsx:decode(TabInfo),
    cdt_erlang_client_sup:start_tab_srv(TabInfoDecoded).

%%--------------------------------------------------------------------

-spec listen_ws(tab()) -> ok.
listen_ws({_, Pid}) ->
    cdt_erlang_client_tab_srv:listen_ws(Pid).

%%--------------------------------------------------------------------

-spec unlisten_ws(tab()) -> ok.
unlisten_ws({_, Pid}) ->
    cdt_erlang_client_tab_srv:unlisten_ws(Pid).
