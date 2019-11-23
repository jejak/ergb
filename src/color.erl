-module(color).

%% API
-export([rgb_to_hsl/1, from_rgb/1, from_rgb_hex/1, new/3, from_fractions/3,
        to_rgb/1, to_hsl/1, to_rgb_hex/1, lighten/2, lighten_in_percent/2,
        darken/2, darken_in_percent/2, saturate/2, saturate_in_percent/2,
        desaturate/2, desaturate_in_percent/2, invert/1, mix/3, mix/2]).

-export_type([color/0]).

-define(is_color(H, S, L),
            is_number(H) andalso (H >= 0) andalso (360 > H) andalso
            is_number(S) andalso (S >= 0.0) andalso (1.0 >= S) andalso
            is_number(L) andalso (L >= 0.0) andalso (1.0 >= L)).

-define(is_pct_amount(Amount),
            is_number(Amount) andalso (Amount >= 0) andalso (100.0 >= Amount)).
-define(is_rgb(R, G, B),
            is_number(R) andalso (R >= 0) andalso (255 >= R) andalso
            is_number(G) andalso (G >= 0) andalso (255 >= G) andalso
            is_number(B) andalso (B >= 0) andalso (255 >= B)).

-define(is_fraction(V),
            is_number(V) andalso (V >= 0.0) andalso (1.0 >= V)).

%%------------------------------------------------------------------------------
-type rgb() :: {byte(), byte(), byte()}.

%% Define a color
-record(color,
    {hue                    :: float(),
     saturation             :: float(),
     lightness              :: float()
    }).

-opaque color() :: #color{}.
%%------------------------------------------------------------------------------

%% Convert color in {r,g,b} to {h,s,l) representation
-spec rgb_to_hsl(RGB) -> {H, S, L} when
    RGB :: rgb(),
    H :: float(),
    S :: float(),
    L :: float().

rgb_to_hsl({R, G, B} = RGB)
        when ?is_rgb(R, G, B) ->
    to_hsl( from_rgb(RGB) );
rgb_to_hsl(_) ->
    erlang:error(badarg).

%% New color (h,s,l)
-spec new(H,S,L) -> color() when
    H :: float(),
    S :: float(),
    L :: float().

new(H,S,L)
        when ?is_color(H,S,L) ->
    #color{ hue = erlang:float(H),
            saturation = erlang:float(S),
            lightness = erlang:float(L)};
new(_,_,_) ->
    erlang:error(badarg).

%% New color from (h,s,l) fractions
-spec from_fractions(H,S,L) -> color() when
    H :: float(),
    S :: float(),
    L :: float().

from_fractions(H, S, L)
        when ?is_fraction(H),
             ?is_fraction(S),
             ?is_fraction(L) ->
    new(360*H, S, L).

%% Color from (r,g,b)
%% Based on
%% https://www.rapidtables.com/convert/color/rgb-to-hsl.html
-spec from_rgb(RGB) -> color() when
    RGB :: rgb().

from_rgb({R, G, B})
            when ?is_rgb(R, G, B) ->
    RGB_ = lists:map(fun(X) -> (X/255) end, [R, G, B]),
    CMin = lists:min(RGB_),
    CMax = lists:max(RGB_),
    Delta = CMax - CMin,
    H = from_rgb_h(Delta, CMax, RGB_),
    L = from_rgb_l(CMax, CMin),
    S = from_rgb_s(Delta, L),
    new(H, S, L);
from_rgb(_) ->
    erlang:error(badarg).

from_rgb_h(Delta, _, _)
        when Delta == 0.0 ->
    0;
from_rgb_h(Delta, CMax, [RR, GG, BB])
        when CMax == RR ->
    60 * color_utils:rem_p(((GG - BB) / Delta), 6);
from_rgb_h(Delta, CMax, [RR, GG, BB])
        when CMax == GG ->
    color_utils:corr_h(60 * (((BB - RR) / Delta) + 2));
from_rgb_h(Delta, CMax, [RR, GG, BB])
        when CMax == BB ->
    color_utils:corr_h(60 * (((RR - GG) / Delta) + 4)).

from_rgb_s(Delta, _L)
        when Delta == 0 ->
    0;
from_rgb_s(Delta, L)
        when Delta =/= 0 ->
    (Delta / (1 - (erlang:abs(2 * L - 1)))).

from_rgb_l(CMax, CMin) ->
    ((CMax + CMin)/2).

%% Color from hex string rgb in #rrggbb format
-spec from_rgb_hex(StrOrInteger) -> color() when
    StrOrInteger :: string() | integer().

from_rgb_hex(Str)
        when is_list(Str) ->
    case io_lib:fread("#~2s~2s~2s", Str) of
        {ok, [RS,GS,BS], _} ->
                RGB = try
                        R = erlang:list_to_integer(RS, 16),
                        G = erlang:list_to_integer(GS, 16),
                        B = erlang:list_to_integer(BS, 16),
                        {R,G,B}
                    catch _:_ ->
                        erlang:error(badarg)
                    end,
                from_rgb(RGB);
        _ ->
            erlang:error(badarg)
    end;
from_rgb_hex(Integer)
        when is_integer(Integer),
        (Integer >= 0),
        (16#FFFFFF >= Integer) ->
    Str = erlang:integer_to_list(Integer, 16),
    from_rgb_hex("#" ++ Str).

%% Get (r,g,b) from color
%% Based on
%% https://www.rapidtables.com/convert/color/hsl-to-rgb.html
%% https://en.wikipedia.org/wiki/HSL_and_HSV
-spec to_rgb(Color) -> {R, G, B} when
    Color :: color(),
    R :: 0..255,
    G :: 0..255,
    B :: 0..255 .

to_rgb(#color{hue = H, saturation = S, lightness = L})
        when ?is_color(H,S,L) ->
    C = (1 - erlang:abs(2 * L - 1)) * S,
    %X = C * (1 - (erlang:abs( (erlang:trunc(H/60) rem 2) - 1))),
    X = C * (1 - (erlang:abs( color_utils:rem_p(H/60, 2) - 1))),
    %%X = C * (1 - (erlang:abs( rem2(H/60) - 1))),
    M = L - (C/2),
    RGB_ = case H of
                    H1 when H1>=0, H1<60 -> [C, X, 0];
                    H2 when H2>=60, H2<120 -> [X, C, 0];
                    H3 when H3>=120, H3<180 -> [0, C, X];
                    H4 when H4>=180, H4<240 -> [0, X, C];
                    H5 when H5>=240, H5<300 -> [X, 0, C];
                    H6 when H6>=300, H6<360 -> [C, 0, X]
                end,
    [R, G, B] = lists:map(fun(X_) ->
                    erlang:round((X_ + M) * 255)
                end, RGB_),
    {R, G, B};
to_rgb(_) ->
    erlang:error(badarg).

%% Get (h, s, l) from color
-spec to_hsl(Color) -> {H, S, L} when
    Color :: color(),
    H :: float(),
    S :: float(),
    L :: float().

to_hsl(#color{hue = H, saturation = S, lightness = L})
        when ?is_color(H,S,L) ->
    {H, S, L};
to_hsl(_) ->
    erlang:error(badarg).

%% Get hex string rgb in #rrggbb format from color
-spec to_rgb_hex(Color) -> HexString when
    Color :: color(),
    HexString :: string().

to_rgb_hex(#color{hue = H, saturation = S, lightness = L} = Color)
        when ?is_color(H,S,L) ->
    RGB = erlang:tuple_to_list( to_rgb(Color) ),
    Parts = ["#"] ++
            lists:map( fun(X) ->
                            lists:flatten(io_lib:format("~2.16.0B", [X]))
                        end,
                        RGB),
    lists:concat(Parts);
to_rgb_hex(_) ->
    erlang:error(badarg).

%% Lighten color
-spec lighten(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

lighten(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewLightness = case (L + (Amount/100.0)) of
                        V when (V < 1.0) -> V;
                        _ -> 1.0
                    end,
    #color{ hue = H,
            saturation = S,
            lightness = NewLightness};
lighten(_, _) ->
    erlang:error(badarg).

%% Lighten color in percentage
-spec lighten_in_percent(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

lighten_in_percent(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewLightness = L + ((1 - L) * (Amount/100.0)),
    #color{ hue = H,
            saturation = S,
            lightness = NewLightness};
lighten_in_percent(_, _) ->
    erlang:error(badarg).

%% Darken color
-spec darken(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

darken(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewLightness = case (L - (Amount/100.0)) of
                        V when (V > 0) -> V;
                        _ -> 0
                    end,
    #color{ hue = H,
            saturation = S,
            lightness = NewLightness};
darken(_, _) ->
    erlang:error(badarg).

%% Darken color in percentage
-spec darken_in_percent(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

darken_in_percent(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewLightness = (L * (1 - (Amount/100.0))),
    #color{ hue = H,
            saturation = S,
            lightness = NewLightness};
darken_in_percent(_, _) ->
    erlang:error(badarg).

%% Saturate color
-spec saturate(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

saturate(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewSaturation = case (S + (Amount/100.0)) of
                        V when (V < 1.0) -> V;
                        _ -> 1.0
                    end,
    #color{ hue = H,
            saturation = NewSaturation,
            lightness = L};
saturate(_, _) ->
    erlang:error(badarg).

%% Saturate color in percentage
-spec saturate_in_percent(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

saturate_in_percent(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewSaturation = ((1 - S) * (Amount/100.0)),
    #color{ hue = H,
            saturation = NewSaturation,
            lightness = L};
saturate_in_percent(_, _) ->
    erlang:error(badarg).

%% Desaturate color
-spec desaturate(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

desaturate(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewSaturation = case (S - (Amount/100.0)) of
                        V when (V > 0) -> V;
                        _ -> 0
                    end,
    #color{ hue = H,
            saturation = NewSaturation,
            lightness = L};
desaturate(_, _) ->
    erlang:error(badarg).

%% Desaturate color in percentage
-spec desaturate_in_percent(Amount, Color) -> Color2 when
    Amount :: number(),
    Color :: color(),
    Color2 :: color().

desaturate_in_percent(Amount, #color{hue = H, saturation = S, lightness = L})
        when ?is_pct_amount(Amount),
             ?is_color(H,S,L) ->
    NewSaturation = (S * (1 - (Amount/100.0))),
    #color{ hue = H,
            saturation = NewSaturation,
            lightness = L};
desaturate_in_percent(_, _) ->
    erlang:error(badarg).

%% Invert colors
-spec invert(Color) -> Color2 when
    Color :: color(),
    Color2 :: color().

invert(#color{hue = H, saturation = S, lightness = L} = Color)
        when ?is_color(H,S,L) ->
    RGB = erlang:tuple_to_list( to_rgb(Color) ),
    NewRGB = lists:map( fun(X) ->
                            255 - X
                        end,
                        RGB),
    from_rgb( erlang:list_to_tuple(NewRGB) );
invert(_) ->
    erlang:error(badarg).


%% Mix colors
-spec mix(OtherColor, Percentage, Color) -> MixedColor when
    OtherColor :: color(),
    Percentage :: float(),
    Color :: color(),
    MixedColor :: color().

mix(#color{hue = H2, saturation = S2, lightness = L2} = Other,
    Percentage,
    #color{hue = H1, saturation = S1, lightness = L1} = Color)
        when ?is_color(H2, S2, L2),
             ?is_pct_amount(Percentage),
             ?is_color(H1, S1, L1) ->
    Coeff = erlang:float(Percentage) / 100.0,
    C1 = erlang:tuple_to_list( to_rgb(Color) ),
    C2 = erlang:tuple_to_list( to_rgb(Other) ),
    ZippedRgb = lists:zip(C1, C2),
    ResultRgb = lists:map(fun({V1, V2}) ->
                            (V1 * (1 - Coeff)) + (V2 * Coeff)
                        end,
                        ZippedRgb),
    {H, S, L} = rgb_to_hsl( erlang:list_to_tuple(ResultRgb) ),
    new(H, S, L);
mix(_, _, _) ->
    erlang:error(badarg).

%% Mix colors equally
-spec mix(OtherColor, Color) -> MixedColor when
    OtherColor :: color(),
    Color :: color(),
    MixedColor :: color().

mix(OtherColor, Color) ->
    mix(OtherColor, 50, Color).
