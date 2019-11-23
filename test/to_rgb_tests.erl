-module(to_rgb_tests).
-include_lib("eunit/include/eunit.hrl").

invalid_1_test() ->
    ?assertError(badarg, color:to_rgb(color:new(-1,0,0))).

invalid_2_test() ->
    ?assertError(badarg, color:to_rgb(color:new(371,0,0))).

invalid_3_test() ->
    ?assertError(badarg, color:to_rgb(color:new(0,33,0))).

invalid_4_test() ->
    ?assertError(badarg, color:to_rgb(color:new(0,-33,0))).

invalid_5_test() ->
    ?assertError(badarg, color:to_rgb(color:new(0,a,0))).

invalid_6_test() ->
    ?assertError(badarg, color:to_rgb(color:new(0,0,"XX"))).

invalid_7_test() ->
    ?assertError(badarg, color:to_rgb(color:new(0,0,"0.9"))).

black_test() ->
    ?assertEqual({0,0,0}, color:to_rgb(color:new(0,0,0))).

white_test() ->
    ?assertEqual({255,255,255}, color:to_rgb(color:new(0,0,1))).

lime_test() ->
    ?assertEqual({0,255,0}, color:to_rgb(color:new(120,1,0.5))).

blue_test() ->
    ?assertEqual({0,0,255}, color:to_rgb(color:new(240,1,0.5))).

yellow_test() ->
    ?assertEqual({255,255,0}, color:to_rgb(color:new(60,1,0.5))).

cyan_test() ->
    ?assertEqual({0,255,255}, color:to_rgb(color:new(180,1,0.5))).

magenta_test() ->
    ?assertEqual({255,0,255}, color:to_rgb(color:new(300,1,0.5))).

silver_test() ->
    ?assertEqual({191,191,191}, color:to_rgb(color:new(0,0,0.75))).

gray_test() ->
    ?assertEqual({128,128,128}, color:to_rgb(color:new(0,0,0.5))).

maroon_test() ->
    ?assertEqual({128,0,0}, color:to_rgb(color:new(0,1,0.25))).

olive_test() ->
    ?assertEqual({128,128,0}, color:to_rgb(color:new(60,1,0.25))).

green_test() ->
    ?assertEqual({0,128,0}, color:to_rgb(color:new(120,1,0.25))).

purple_test() ->
    ?assertEqual({128,0,128}, color:to_rgb(color:new(300,1,0.25))).

teal_test() ->
    ?assertEqual({0,128,128}, color:to_rgb(color:new(180,1,0.25))).

navy_test() ->
    ?assertEqual({0,0,128}, color:to_rgb(color:new(240,1,0.25))).
