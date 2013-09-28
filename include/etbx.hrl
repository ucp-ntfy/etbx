%% @doc returns a "RECSPEC" that can be used by to_rec in order to 
%% perform conversions
-define(RECSPEC(R), [R | record_info(fields, R)]).

%% @doc maps the values from a property list into a record
-define(TO_REC(R, P), 
        list_to_tuple(
          [R | [proplists:get_value(X,P) || X <- record_info(fields,R)]])).
