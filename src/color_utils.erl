%% @hidden

-module(color_utils).

%% API
-export([rem_p/2, corr_h/1]).

% As rem but it works on float number and always produces positive return value
-spec rem_p(X, N) -> number() when
    X :: number(),
    N :: pos_integer().

rem_p(X, N)
        when is_number(X),
             (N > 0) ->
    Div = erlang:trunc(X / N),
    Rem = (X - (Div * N)),
    case Rem of
        R when (R < 0) -> (R+N);
        R when (R >= 0) -> Rem
    end;
rem_p(_, _) ->
    erlang:error(badarg).

% Correct color hue be ensure be in [0..360]
-spec corr_h(H) -> number() when
    H :: number().

corr_h(H) when (H >= 0), (360 > H) ->
    H;
corr_h(H) when (H < 0), (H > -360) ->
    (360 + H);
corr_h(_) ->
    erlang:error(badarg).
