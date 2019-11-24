-module(color_tests).
-include_lib("eunit/include/eunit.hrl").

should_init_color_object_with_css_like_hex_color_string_test() ->
    Color = color:from_rgb_hex("#333333"),
    ?assertEqual({0.0, 0.0, 0.2}, color:to_hsl(Color)).

should_init_color_object_with_integer_test() ->
    Color = color:from_rgb_hex(16#FF0000),
    ?assertEqual({0.0, 1.0, 0.5}, color:to_hsl(Color)).

should_init_color_object_with_rgb_values_test() ->
    Color = color:from_rgb({16#FF, 16#FF, 16#FF}),
    ?assertEqual({0.0, 0.0, 1.0}, color:to_hsl(Color)).

should_init_color_object_with_HSL_fraction_values_test() ->
    Color = color:from_fractions(0, 1.0, 0.5),
    ?assertEqual({0.0, 1.0, 0.5}, color:to_hsl(Color)).

should_darken_color_by_value_test() ->
    Color = color:new(0, 1.0, 0.5),
    ?assertEqual({0.0, 1.0, 0.3}, color:to_hsl( color:darken(20, Color) )).

should_darken_color_by_percent_test() ->
    Color = color:new(0, 0, 0.3),
    ?assertEqual({0.0, 0.0, 0.27}, color:to_hsl( color:darken_in_percent(10, Color) )).

should_lighten_color_by_value_test() ->
    Color = color:new(0, 1.0, 0.5),
    ?assertEqual({0.0, 1.0, 0.7}, color:to_hsl( color:lighten(20, Color) )).

should_lighten_color_by_percent_test() ->
    Color = color:new(0, 0, 0.3),
    ?assertEqual({0.0, 0.0, 0.37}, color:to_hsl( color:lighten_in_percent(10, Color) )).

should_saturate_color_by_value_test() ->
    Color = color:new(0, 1.0, 0.5),
    ?assertEqual({0.0, 1.0, 0.5}, color:to_hsl( color:saturate(20, Color) )).

should_saturate_color_by_percent_test() ->
    Color = color:new(0, 0, 0.3),
    ?assertEqual({0.0, 0.1, 0.3}, color:to_hsl( color:saturate_in_percent(10, Color) )).

should_desaturate_color_by_value_test() ->
    Color = color:new(0, 1.0, 0.5),
    ?assertEqual({0.0, 0.8, 0.5}, color:to_hsl( color:desaturate(20, Color) )).

should_desaturate_color_by_percent_test() ->
    Color = color:new(0, 0.5, 0.3),
    ?assertEqual({0.0, 0.45, 0.3}, color:to_hsl( color:desaturate_in_percent(10, Color) )).

should_invert_color_test() ->
    Color = color:new(0, 0, 1.0),
    ?assertEqual({0.0, 0.0, 0.0}, color:to_hsl( color:invert(Color) )).

should_mix_color_with_other_color_test() ->
    Color1 = color:from_rgb_hex("#1F77B4"),
    Color2 = color:from_rgb_hex("#D62728"),
    Color3 = color:mix(Color2, 10, Color1),
    ?assertEqual("#316FA6", color:to_rgb_hex(Color3)).

should_mix_colors_equally_test() ->
    Color1 = color:from_rgb_hex("#1F77B4"),
    Color2 = color:from_rgb_hex("#D62728"),
    Color3 = color:mix(Color2, Color1),
    ?assertEqual("#7B4F6E", color:to_rgb_hex(Color3)).

should_set_color_hue_test() ->
    Color = color:new(120, 0.5, 0.3),
    ?assertEqual({30.0, 0.5, 0.3}, color:to_hsl( color:set_hue(30, Color) )).

should_set_color_saturation_test() ->
    Color = color:new(120, 0.5, 0.3),
    ?assertEqual({120.0, 0.1, 0.3}, color:to_hsl( color:set_saturation(0.1, Color) )).

should_set_color_lightness_test() ->
    Color = color:new(120, 0.5, 0.3),
    ?assertEqual({120.0, 0.5, 0.1}, color:to_hsl( color:set_lightness(0.1, Color) )).
