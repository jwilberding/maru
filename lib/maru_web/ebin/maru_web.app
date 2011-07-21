%% This is the application resource file (.app file) for the maru_web,
%% application.
{application, maru_web,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [maru_web_app,
              maru_web_sup]},
   {registered,[maru_web_sup]},
   {applications, [kernel, stdlib]},
   {mod, {maru_web_app,[]}},
   {start_phases, []}]}.

