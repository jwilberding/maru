%% -*- erlang --*
%% This is the application resource file (.app file) for the maru_models,
%% application.
{application, maru_models,
  [{description, "Maru generic models"},
   {vsn, "0.1.0"},
   {modules, [maru_model_transform,
              maru_model_base,
              maru_model_types,
              maru_model_users,
              jsonerl,
              sha2]},
   {registered,[]},
   {applications, [kernel, stdlib, sasl, crypto, bcrypt, ossp_uuid]},
   {start_phases, []}]}.

