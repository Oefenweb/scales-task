% normal.pl
% using ranf to generate a normal distribution
% -inf <= x <= inf, mean=M, standard deviation=S

% if "ranf" already loaded,
% then nothing happens 

:- ensure_loaded(ranf).
:- arithmetic_function(normal/2).


normal(M,S,N) :-
	box_muller(M,S,N).

% classical fast method for computing
% normal functions using polar co-ords
% (no- i dont understand it either)
box_muller(M,S,N) :-
	w(W0,X),
	W is sqrt((-2.0 * log(W0))/W0),
	Y1 is X * W,
	N is M + Y1*S.

w(W,X) :-
	X1 is 2.0 * ranf - 1,
	X2 is 2.0 * ranf - 1,
	W0 is X1*X1 + X2*X2,
	% IF -> THEN ; ELSE
	% same as xx :- IF,!, THEN,
	%         xx :- ELSE
	% vars bound in IF not available to ELSE
	% no backtracking within the IF
	% -> ; precendence higher than ,
	(W0  >= 1.0 -> w(W,X) ; W0=W, X = X1).
