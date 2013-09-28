%% @doc returns a "RECSPEC" that can be used by to_rec in order to 
%% perform conversions
-define(RECSPEC(R), {R, tuple_to_list(#R{}), record_info(fields, R)}).
