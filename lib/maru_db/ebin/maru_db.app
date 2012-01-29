%% -*- erlang -*-
%% This is the application resource file (.app file) for the maru_db,
%% application.
{application, maru_db,
  [{description, "Maru db interface"},
   {vsn, "0.1.0"},
   {modules, [maru_db_app,
              maru_db_sup,

              maru_db,
              maru_db_mnesia]},
   {registered,[maru_db_sup]},
   {applications, [kernel, stdlib, mnesia, maru_idioms]},
   {mod, {maru_db_app,[]}},
   {start_phases, []}]}.
