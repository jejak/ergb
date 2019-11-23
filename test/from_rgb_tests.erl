-module(from_rgb_tests).
-include_lib("eunit/include/eunit.hrl").

black_test() ->
    ?assertEqual({0,0,0}, color:to_rgb(color:from_rgb({0,0,0}))).

white_test() ->
    ?assertEqual({255,255,255}, color:to_rgb(color:from_rgb({255,255,255}))).

lime_test() ->
    ?assertEqual({0,255,0}, color:to_rgb(color:from_rgb({0,255,0}))).

blue_test() ->
    ?assertEqual({0,0,255}, color:to_rgb(color:from_rgb({0,0,255}))).

yellow_test() ->
    ?assertEqual({255,255,0}, color:to_rgb(color:from_rgb({255,255,0}))).

cyan_test() ->
    ?assertEqual({0,255,255}, color:to_rgb(color:from_rgb({0,255,255}))).

magenta_test() ->
    ?assertEqual({255,0,255}, color:to_rgb(color:from_rgb({255,0,255}))).

silver_test() ->
    ?assertEqual({191,191,191}, color:to_rgb(color:from_rgb({191,191,191}))).

gray_test() ->
    ?assertEqual({128,128,128}, color:to_rgb(color:from_rgb({128,128,128}))).

maroon_test() ->
    ?assertEqual({128,0,0}, color:to_rgb(color:from_rgb({128,0,0}))).

olive_test() ->
    ?assertEqual({128,128,0}, color:to_rgb(color:from_rgb({128,128,0}))).

green_test() ->
    ?assertEqual({0,128,0}, color:to_rgb(color:from_rgb({0,128,0}))).

purple_test() ->
    ?assertEqual({128,0,128}, color:to_rgb(color:from_rgb({128,0,128}))).

teal_test() ->
    ?assertEqual({0,128,128}, color:to_rgb(color:from_rgb({0,128,128}))).

navy_test() ->
    ?assertEqual({0,0,128}, color:to_rgb(color:from_rgb({0,0,128}))).
