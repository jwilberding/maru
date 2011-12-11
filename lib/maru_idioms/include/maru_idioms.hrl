-define(NOTIFY_AIRBRAKE(Type, Reason, Message),
        airbrake:notify(Type, Reason, Message, ?MODULE, ?LINE, erlang:get_stacktrace())).

-define(FUN(Body), fun() ->
                           Body
                   end).

-define(FUN1(Body), fun(X) ->
                           Body
                    end).

-define(DEFAULT(X, Y), case X of
                           undefined ->
                               Y;
                           not_found ->
                               Y;
                           _ ->
                               X
                       end).
