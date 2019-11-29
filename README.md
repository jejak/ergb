eRGB — Simple Color Conversion and Manipulation Erlang Library
------------------------
[![Build Status](https://api.travis-ci.org/jejak/ergb.svg?branch=master)](https://travis-ci.com/jejak/ergb)
This Erlang library is built to handle color conversion and manipulations.

It is started as an Erlang coding practicing project inspired by github/plashchynski/rgb ruby gem.

During time, there has been a hex package developed and published to the hex.pm repository from this Github project (https://hex.pm/packages/ergb).  

The library online documentation can be found at: https://hexdocs.pm/ergb.

## The eRGB lib
This library was built with the rebar3 Erlang build tool and its code has been structured accordingly to this fact.   

The color manipulation API resides in the library *color* module.

### Usage
The eRGB library can be included in any Erlang app/lib project regardless of the build tool used.

All is needed that this eRGB git repo must be added to the target app/lib dependences.

#### Adding to `rebar3` dependences
rebar.conf:
```erlang
{deps, [
    {ergb, {git, "https://github.com/jejak/ergb.git", {branch, "master"}}}
]}.

```
#### Adding to `erlang.mk` dependences
Makefile:
```
...
DEPS=ergb
dep_ergb = git https://github.com/jejak/ergb.git master

include erlang.mk
```

#### Usage Examples
```erlang
% Supported input data color forms:
Color1 = color:from_rgb_hex("#333333").
Color2 = color:from_rgb_hex(0xFF0000).
Color3 = color:from_rgb(115, 38, 38).
Color = color:new(0, 1.0, 0.5).

% Supported color manipulations ...
color:darken(20, Color).
color:darken_in_percent(10, Color).
color:lighten(20, Color).
color:lighten_in_percent(20, Color).
color:saturate(20, Color).
color:saturate_in_percent(20, Color).
color:desaturate(20, Color).
color:desaturate_in_percent(20, Color).
color:invert(Color).

% Mixing colors ...
color:mix(OtherColor, 20, Color). % Mix 20% of other color into this one
color:mix(OtherColor, Color). % 50% by default
color:mix(OtherColor, 20, Color).

% Adjust color hue, saturation, lightness manually ...
color:set_hue(145.2, Color).
color:set_saturation(0.2, Color).
color:set_lightness(0.3, Color).

% Supported output formats ...
color:to_rgb_hex(Color).
"#732626"
color:to_hsl(Color).
{0.0, 1.0, 0.5}
color:to_rgb(Color).
{115, 38, 38}
```

## Run eRGB Unit Tests
In order to run the eRGB unit tests, there is necessary to clone the eRGB git repo and run the existing tests.

The test machine requirements:
- erlang otp: 19.0 +
- git
- rebar3

### Cloning the repo and run the tests
Create an appropriate test folder on a developer machine and then run the following commands.
```sh
> cd <test-folder>
> git clone https://github.com/jejak/ergb.git
> cd ergb
> rebar3 eunit
```

## Support
Feel free to create [issues](https://github.com/jejak/ergb/issues).

## License
Please, see [LICENSE](https://github.com/jejak/ergb/blob/master/README.md) for licensing details.
