ergb — Simple color manipulation erlang library
------------------------
[![Build Status](https://api.travis-ci.org/jejak/ergb.svg?branch=master)](https://travis-ci.com/jejak/ergb)
This erlang library is built to handle color conversion and manipulations.

It is basically the erlang port of https://github.com/plashchynski/rgb rgb ruby gem.

## Examples
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

## Support
Feel free to create [issues](https://github.com/jejak/ergb/issues).

## License
Please, see [LICENSE](https://github.com/jejak/ergb/blob/master/README.md) for licensing details.
