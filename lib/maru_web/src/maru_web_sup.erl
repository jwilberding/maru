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
    {ok, Port} = application:get_env(webmachine_port),
    {ok, App} = application:get_application(),
    LogDir = filename:join(code:priv_dir(App), "logs"),

    {ok, {priv, HostApp}} = application:get_env(host_dir),
    HostDir = code:priv_dir(HostApp),
    %{ok, {priv, HostApp}} = application:get_env(dispatch_file),
    {ok, Dispatch} = file:consult(filename:join(HostDir, "dispatch")),

    %% Write out erlydtl compiled templates to priv dir for serving
    create_static_from_templates(HostDir),

    [{ip, IP},
     {port, Port},
     {log_dir, LogDir},
     {backlog, 128},
     {dispatch, Dispatch}].

create_static_from_templates(HostDir) ->
    filelib:fold_files(HostDir, "html", true,
                       fun(X, _Acc) ->
                               erlydtl:compile(X, page),
                               {ok, File} = page:render(),
                               file:write_file(X, File),
                               ok
                       end, ok).
