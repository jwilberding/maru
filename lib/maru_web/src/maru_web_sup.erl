%%%----------------------------------------------------------------
%%% @author  Tristan Sloughter <tristan.sloughter@gmail.com>
%%% @doc
%%% @end
%%% @copyright 2011 Tristan Sloughter
%%%----------------------------------------------------------------
-module(maru_web_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | any().
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================


%% @private
-spec init(list()) -> {ok, {SupFlags::any(), [ChildSpec::any()]}} |
                       ignore | {error, Reason::any()}.
init([]) ->
    WebChild = {webmachine_mochiweb,
                {webmachine_mochiweb, start, [config()]},
                permanent, 5000, worker, dynamic},

    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags , [WebChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

config() ->
    {ok, IP} = application:get_env(webmachine_ip),

    Port = case application:get_env(webmachine_port) of
	       {ok, P} ->
		   P;
	       _ ->
		   Home = os:getenv("HOME"),
		   case file:read_file(filename:join(Home, "environment.json")) of
		       {ok, Binary} ->
			   {PropList} = jiffy:decode(Binary),
			   proplists:get_value(<<"PORT_WWW">>, PropList, 8080);
		       _ ->
			   8080
		   end
	   end,

    {ok, App} = application:get_application(),
    LogDir = filename:join(code:priv_dir(App), "logs"),

    {ok, {priv, HostApp}} = application:get_env(host_dir),
    HostDir = code:priv_dir(HostApp),
    %{ok, {priv, HostApp}} = application:get_env(dispatch_file),
    {ok, Dispatch} = file:consult(filename:join(HostDir, "dispatch")),

    [{ip, IP},
     {port, Port},
     {log_dir, LogDir},
     {backlog, 128},
     {dispatch, Dispatch}].
