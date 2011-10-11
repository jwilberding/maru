%% -*- erlang -*-
%% This is the application resource file (.app file) for the maru_models,
%% application.
{application, maru_models,
  [{description, "Maru generic models"},
   {vsn, "0.1.0"},
   {modules, [maru_model_transform,
              maru_model_base,
              maru_model_types,
              maru_model_users,
              maru_model_sessions,
              maru_model_utils]},
   {registered,[]},
   {applications, [kernel, stdlib, sasl, crypto, bcrypt, ossp_uuid, jiffy, maru_db]},
   {start_phases, []}]}.

