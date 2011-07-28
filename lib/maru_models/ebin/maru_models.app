%% -*- erlang --*
%% This is the application resource file (.app file) for the maru_models,
%% application.
{application, maru_models,
  [{description, "Your Desc HERE"},
   {vsn, "0.1.0"},
   {modules, [maru_model_transform,
              maru_model_base,
              maru_model_types,
              jsonerl,
              sha2]},
   {registered,[maru_models_sup]},
   {applications, [kernel, stdlib]},
   {start_phases, []}]}.

