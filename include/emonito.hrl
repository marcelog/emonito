-ifndef(__EMONITO__HRL__).
-define(__EMONITO__HRL__, 1).

-type env_var_name():: string().
-type env_var_val():: string().
-type env_var():: {env_var_name(), env_var_val()}.
-type env():: [env_var()].

-endif.
