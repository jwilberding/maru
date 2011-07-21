%% This is the application resource file (.app file) for the maru_models,
%% application.
{application, maru_models,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [maru_models_app,
              maru_models_sup]},
   {registered,[maru_models_sup]},
   {applications, [kernel, stdlib]},
   {mod, {maru_models_app,[]}},
   {start_phases, []}]}.

