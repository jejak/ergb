
%%------------------------------------------------------------------------------
-define(is_hue(V),
            is_number(H) andalso (H >= 0) andalso (360 > H)).

-define(is_saturation(V),
            is_number(S) andalso (S >= 0.0) andalso (1.0 >= S)).

-define(is_lightness(V),
            is_number(L) andalso (L >= 0.0) andalso (1.0 >= L)).

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
