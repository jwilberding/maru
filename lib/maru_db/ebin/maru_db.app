%% This is the application resource file (.app file) for the maru_db,
%% application.
{application, maru_db,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [maru_db_app,
              maru_db_sup]},
   {registered,[maru_db_sup]},
   {applications, [kernel, stdlib]},
   {mod, {maru_db_app,[]}},
   {start_phases, []}]}.

