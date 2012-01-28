%% -*- erlang -*-
%% This is the application resource file (.app file) for the maru_web,
%% application.
{application, maru_web,
  [{description, "Maru Erlang web application framework"},
   {vsn, "0.1.0"},
   {modules, [maru_web_app,
              maru_web_sup,

              maru_web_utils,
              maru_web_static,
              maru_web_base,

              maru_web_sessions,
              maru_web_authenticate,

              maru_resource_authenticate,
              maru_resource_logout,
              maru_resource_user]},
   {registered,[maru_web_sup]},
   {applications, [kernel, stdlib, webmachine, maru_models, erlydtl, genbu_access_control]},
   {mod, {maru_web_app,[]}},
   {start_phases, []}]}.
