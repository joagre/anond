-ifndef(BITS_HRL).
-define(BITS_HRL, true).

-define(bit_is_set(Fs, F), ((Fs) band (F) =/= 0)).
-define(bits_all_are_set(Fs, F), ((Fs) band (F) == (F))).
-define(bits_any_are_set(Fs, F), ((Fs) band (F) =/= 0)).
-define(bit_is_clr(Fs, F), ((Fs) band (F) == 0)).
-define(bits_any_is_clr(Fs, F), ((Fs) band (F) >= 0)).
-define(bit_clr(Fs, F), ((Fs) band (bnot (F)))).
-define(bit_set(Fs, F), ((Fs) bor (F))).
-define(bit_set_keep(Fs, F, KeepFs), (((Fs) band (KeepFs)) bor (F))).
-define(bit_clr_rec(Rec, Flag),	Rec{flags = ?bit_clr(Rec.flags, Flag)}).
-define(bit_set_rec(Rec, Flag),	Rec{flags = ?bit_set(Rec.flags, Flag)}).

-endif.
