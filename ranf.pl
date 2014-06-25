% ranf.pl

% random floats 0 <= x < 1

% the swi-prolog "is" function can
% be programmed. predicates of
% arity N can be called as functions
% of arity N-1

:- arithmetic_function(ranf/0).

ranf(X) :-     
	N =  65536,
	% X is random(N) returns a 
        % random number from 0 .. N-1
	X is random(N)/N.