ergb — Simple color manipulation erlang library
------------------------
[![Build Status](https://api.travis-ci.org/jejak/ergb.svg?branch=master)](https://travis-ci.com/jejak/ergb)
This erlang library is built to handle color conversion and manipulations.

It is basically the erlang port of https://github.com/plashchynski/rgb ruby gem.

## The ergb lib
This library was built with the rebar3 Erlang build tool and its code has been structured accordingly to this fact.   

The color manipulation API resides in the library *color* module.

### Usage
The ergb library can be included in any Erlang app/lib project regardless of the build tool used.

All is needed that the ergb git repo must be added to the target app/lib dependences.

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
Color = color:new(0, 1.0, 0.5). % HSL

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

% Adjust color HSL (hue, saturation, and lightness values) manually ...
color:hue(0.1, Color).
color:saturation(0.2, Color).
color:lightness(0.3, Color).

% Supported output formats ...
color:to_rgb_hex(Color).
"#732626"
color:to_hsl(Color).
{0.0, 1.0, 0.5}
color:to_rgb(Color).
{115, 38, 38}
```

## Run ergb Unit Tests
In order to run the ergb unit tests, there is necessary to clone the ergb git repo and run the existing tests.

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
