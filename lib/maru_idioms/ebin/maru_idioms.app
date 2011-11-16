%% -*- erlang -*-
%% This is the application resource file (.app file) for the ct_model,
%% application.
{application, maru_idioms,
  [{description, "Idioms"},
   {vsn, "0.1.0"},
   {modules, [maru_idioms]},
   {registered, []},
   {applications, [kernel, stdlib, erlbrake]},
   {start_phases, []}]}.
