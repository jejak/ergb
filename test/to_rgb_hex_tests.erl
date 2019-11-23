-module(to_rgb_hex_tests).
-include_lib("eunit/include/eunit.hrl").

black_test() ->
    ?assertEqual("#000000", color:to_rgb_hex(color:new(0,0,0))).

white_test() ->
    ?assertEqual("#FFFFFF", color:to_rgb_hex(color:new(0,0,1))).

lime_test() ->
    ?assertEqual("#00FF00", color:to_rgb_hex(color:new(120,1,0.5))).

blue_test() ->
    ?assertEqual("#0000FF", color:to_rgb_hex(color:new(240,1,0.5))).

yellow_test() ->
    ?assertEqual("#FFFF00", color:to_rgb_hex(color:new(60,1,0.5))).

cyan_test() ->
    ?assertEqual("#00FFFF", color:to_rgb_hex(color:new(180,1,0.5))).

magenta_test() ->
    ?assertEqual("#FF00FF", color:to_rgb_hex(color:new(300,1,0.5))).

silver_test() ->
    ?assertEqual("#BFBFBF", color:to_rgb_hex(color:new(0,0,0.75))).

gray_test() ->
    ?assertEqual("#808080", color:to_rgb_hex(color:new(0,0,0.5))).

maroon_test() ->
    ?assertEqual("#800000", color:to_rgb_hex(color:new(0,1,0.25))).

olive_test() ->
    ?assertEqual("#808000", color:to_rgb_hex(color:new(60,1,0.25))).

green_test() ->
    ?assertEqual("#008000", color:to_rgb_hex(color:new(120,1,0.25))).

purple_test() ->
    ?assertEqual("#800080", color:to_rgb_hex(color:new(300,1,0.25))).

teal_test() ->
    ?assertEqual("#008080", color:to_rgb_hex(color:new(180,1,0.25))).

navy_test() ->
    ?assertEqual("#000080", color:to_rgb_hex(color:new(240,1,0.25))).
