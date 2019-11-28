%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% ergb: Color Conversion and Manipulation Library
%%
%% Copyright (c) 2019 Jeno Jakab
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%% @doc
%% ERGB library main module
%%
%% It contains the entry points of the library color conversion and
%% manipulation API.
%%
%% For general documentation, please, consult the overview page.
-module(color).

%% API
-export([rgb_to_hsl/1, from_rgb/1, from_rgb_hex/1, new/3, from_fractions/3,
        to_rgb/1, to_hsl/1, to_rgb_hex/1, lighten/2, lighten_in_percent/2,
        darken/2, darken_in_percent/2, saturate/2, saturate_in_percent/2,
        desaturate/2, desaturate_in_percent/2, invert/1, mix/3, mix/2,
        set_hue/2, set_saturation/2, set_lightness/2]).

-export_type([color/0, rgb/0]).

%%------------------------------------------------------------------------------

-include("color.hrl").

%%------------------------------------------------------------------------------
-type rgb() :: {byte(), byte(), byte()}.
%% It represents a color in the rgb color scheme. It is essentially a tuple
%% where:
%% <dl>
%%   <dt>first element of tuple</dt>
%%     <dd>
%%       Red component: 0..255
%%     </dd>
%%   <dt>second element of tuple</dt>
%%     <dd>
%%       Green component: 0..255
%%     </dd>
%%   <dt>third element of tuple</dt>
%%     <dd>
%%       Blue component: 0..255
%%     </dd>
%% </dl>

%% Define a color
-record(color,
    {hue                    :: float(),
     saturation             :: float(),
     lightness              :: float()
    }).

-opaque color() :: #color{}.
%% It represents a color in the ERGB library. Its internal structure is private.

%%------------------------------------------------------------------------------

%% @doc
%% Converts a color given in RGB representation to HSL representation
%% @param RGB an {@link rgb()} representing the color to be converted to HSL
%% @returns the resulted hue, saturation, lightness tuple
%% @end
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

%% @doc
%% Constructs a color instance from hue, saturation, lightness.
%% @param H the input hue: 0..360
%% @param S the input saturation: 0..1.0
%% @param L the input lightness: 0..1.0
%% @returns the constucted color() type instance
%% @end
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

%% @doc
%% Constructs a color instance from hue, saturation, lightness  fractions.
%% @param H the input hue: 0..1.0
%% @param S the input saturation: 0..1.0
%% @param L the input lightness: 0..1.0
%% @returns the color() type instance
%% @end
-spec from_fractions(H,S,L) -> color() when
    H :: float(),
    S :: float(),
    L :: float().

from_fractions(H, S, L)
        when ?is_fraction(H),
             ?is_fraction(S),
             ?is_fraction(L) ->
    new(360*H, S, L).

%% @doc
%% Constructs a color instance from a red, green, blue tuple.
%% @param RGB the {@link rgb()} input color
%% @returns the constucted color() type instance
%% @end
%% The calculations are based on
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

%% @doc
%% Constructs a color instance from an integer or from a rrggbb
%% formatted string.
%%
%% @param StrOrInteger
%% <ul>
%%   <li>
%%     if StrOrInteger param is string:
%%     <ul>
%%       <li>
%%         it must be in #rrggbb format, where the string represent a color
%%         in rgb in hex format as here:
%%         <ul>
%%           <li>rr - hexa-decimal-string representing the red component of the
%%                    color i.e. cc = decimal 204
%%           </li>
%%           <li>gg - same as rr but it represents the green component of the
%%                    color
%%           </li>
%%           <li>bb - same as rr but it represents the blue component of the
%%                    color
%%           </li>
%%         </ul>
%%       </li>
%%     </ul>
%%   </li>
%%   <li>
%%     if StrOrInteger is Integer:
%%     <ul>
%%       <li>
%%         It must be in range (0..16#ffffff) and
%%       </li>
%%       <li>
%%         it represent an color as hex number as 16#rrggbb and as above:
%%         <ul>
%%           <li>
%%             rr - is the red
%%           </li>
%%           <li>
%%            gg - is the green, and
%%           </li>
%%           <li>
%%             bb -is the blue color component as hex number
%%           </li>
%%         </ul>
%%       </li>
%%     </ul>
%%   </li>
%% </ul>
%% @returns the constucted color() type instance
%% @end
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

%% @doc
%% Converts a {@link color()} type instance to a red, green, blue tuple.
%% @param Color the input color() type instance
%% @returns the resulting {@link rgb()} type value
%% @end
%% The calculations are based on
%% https://www.rapidtables.com/convert/color/hsl-to-rgb.html
%% https://en.wikipedia.org/wiki/HSL_and_HSV
-spec to_rgb(Color) -> RGB when
    Color :: color(),
    RGB :: rgb().

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

%% @doc
%% Converts a {@link color()} type instance to a hue, saturation, lightness tuple.
%% @param Color the input color() type instance
%% @returns the resulting hue, saturation, lightness tuple
%% @end
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

%% @doc
%% Converts a {@link color()} type instance to #rrggbb formatted string.
%%
%% In the formatted string:
%% <ul>
%%   <li>rr - hexa-decimal-string representing the red component of the color
%%        i.e. cc = decimal 204</li>
%%   <li>gg - same as rr but it represents the green component of the color</li>
%%   <li>bb - same as rr but it represents the blue component of the color</li>
%% </ul>
%% @param Color the input color() type instance
%% @returns the resulting string
%% @end
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

%% @doc
%% Lightens a {@link color()} type instance adding an amount of lightness more
%% in the color.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting lightened {@link color()} type instance
%% @end
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

%% @doc
%% Lightens a {@link color()} type instance increasing its lightness by taking
%% away an amount of percentage darkness.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting lightened {@link color()} type instance
%% @end
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

%% @doc
%% Darkens a {@link color()} type instance adding an amount of darkness more
%% in the color.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting darkened {@link color()} type instance
%% @end
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

%% @doc
%% Darkens a {@link color()} type instance increasing its darkness by taking
%% away an amount of percentage lightness.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting darkened {@link color()} type instance
%% @end
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

%% @doc
%% Saturates a {@link color()} type instance adding an amount of saturation more
%% in the color.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting saturated {@link color()} type instance
%% @end
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

%% @doc
%% Saturates a {@link color()} type instance increasing its saturation  by
%% taking away an amount of percentage unsaturation.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting saturated {@link color()} type instance
%% @end
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

%% @doc
%% Desaturates a {@link color()} type instance adding an amount of unsaturation
%% more in the color.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting desaturated {@link color()} type instance
%% @end
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

%% @doc
%% Desaturates a {@link color()} type instance increasing its desaturation
%% by taking away an amount of percentage saturation.
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting desaturated {@link color()} type instance
%% @end
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

%% @doc
%% Invertes a {@link color()} type instance in rgb
%% @param Amount a percentage amount between 0..100.0
%% @param Color the input color() type instance
%% @returns the resulting inverted {@link color()} type instance
%% @end
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


%% @doc
%% Mixes two {@link color()} type instances with a percentage mixing coefficient
%% @param OtherColor the other input color() type instance to be mixed in
%% @param Percentage a percentage amount between 0..100.0 defining the other color
%% mixing-in coefficient
%% @param Color the input color() type instance to which the other color be
%% mixed
%% @returns the resulting mixed {@link color()} type instance
%% @end
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

%% @doc
%% Mixes two {@link color()} type instances equally (coefficient=0.5)
%% @param OtherColor the other input color() type instance to be mixed in
%% @param Color the input color() type instance to which the other color be
%% mixed
%% @returns the resulting mixed {@link color()} type instance
%% @end
-spec mix(OtherColor, Color) -> MixedColor when
    OtherColor :: color(),
    Color :: color(),
    MixedColor :: color().

mix(OtherColor, Color) ->
    mix(OtherColor, 50, Color).

%% @doc
%% Sets hue for a {@link color()} type instance
%% @param Hue the hue value between 0..360.0 to be set
%% @param Color the input color() type instance
%% @returns the resulting {@link color()} type instance
%% @end
-spec set_hue(Hue, Color) -> color() when
    Hue :: float(),
    Color :: color().

set_hue(Hue, #color{hue = H, saturation = S, lightness = L})
        when ?is_hue(Hue),
             ?is_color(H,S,L) ->
    new(Hue, S, L);
set_hue(_, _) ->
    erlang:error(badarg).

%% @doc
%% Sets saturation for a {@link color()} type instance
%% @param Saturation the saturation value between 0..1.0 to be set
%% @param Color the input color() type instance
%% @returns the resulting {@link color()} type instance
%% @end
-spec set_saturation(Saturation, Color) -> color() when
    Saturation :: float(),
    Color :: color().

set_saturation(Saturation, #color{hue = H, saturation = S, lightness = L})
        when ?is_saturation(Saturation),
             ?is_color(H,S,L) ->
    new(H, Saturation, L);
set_saturation(_, _) ->
    erlang:error(badarg).

%% @doc
%% Sets lightness for a {@link color()} type instance
%% @param Lightness the lightness value between 0..1.0 to be set
%% @param Color the input color() type instance
%% @returns the resulting {@link color()} type instance
%% @end
-spec set_lightness(Lightness, Color) -> color() when
    Lightness :: float(),
    Color :: color().

set_lightness(Lightness, #color{hue = H, saturation = S, lightness = L})
        when ?is_lightness(Lightness),
             ?is_color(H,S,L) ->
    new(H, S, Lightness);
set_lightness(_, _) ->
    erlang:error(badarg).
