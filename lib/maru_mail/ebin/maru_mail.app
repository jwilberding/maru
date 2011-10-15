%% -*- erlang -*-
%% This is the application resource file (.app file) for the ct_web,
%% application.
{application, maru_mail,
  [{description, "maru email library"},
   {vsn, "0.1.0"},
   {modules, [maru_mail]},
   {registered, [maru_mail_sup]},
   {applications, [kernel, stdlib, ssl, gen_smtp]},
   {mod, {maru_mail_app,[]}},
   {start_phases, []}]}.
