% deriveRules.pl
% Derives all the (extra) facts (information) from one scale.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define dynamic predicates.

:- dynamic heavier/2.
:- dynamic equal/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate derive/3 derives all the facts (information) from one
% scale. For example, the second case states that when there is a scale
% with an object A on the left, an object B on the right, and the scale
% is tipped to the left, then it can be derived that object A is heavier
% than object B. All the derived facts (information) are asserted to the
% knowledge base.

/* --- Remove same objects on both side. --- */

derive(L1, L2, Balance):-
          length(L1, Length1),
          Length1 > 1,
          length(L2, Length2),
          Length2 > 1,
          member(X, L1),
          member(X, L2),
          select(X, L1, NewL1),
          select(X, L2, NewL2),
          derive(NewL1, NewL2, Balance).

/* --- Situation: Two different objects on a scale. --- */

derive([A], [B], left):-
	  A \= B,
	  not(heavier(A, B)),
          assert(heavier(A, B)),
	  unique_relation(A, B).

derive([A], [B], right):-
	  A \= B,
	  not(heavier(B, A)),
          assert(heavier(B, A)),
	  unique_relation(B, A).

derive([A], [B], equal):-
	  A \= B,
	  not(equal(A, B)),
          assert(equal(A, B)),
	  unique_relation(A, B).

%derive([A], [A], left). % Inconsistent!

%derive([A], [A], right). % Inconsistent!

/*
Redundant!

derive([A], [A], equal):-
	not(equal(A, A)),
	assert(equal(A, A)),
	unique_relation(A, A).
*/

/* --- Situation: Three different objects on a scale. --- */

derive([A], [B, C], left):-
	  A \= B, A \= C, B \= C,
	  not(heavier(A, B)),
	  assert(heavier(A, B)),
	  unique_relation(A, B),
	  not(heavier(A, C)),
          assert(heavier(A, C)),
	  unique_relation(A, C).

derive([A], [B, C], right):-
	A \= B, A \= C, B \= C. % No information!

derive([A], [B, C], equal):-
	  A \= B, A \= C, B \= C,
	  not(heavier(A, B)),
	  assert(heavier(A, B)),
	  unique_relation(A, B),
	  not(heavier(A, C)),
          assert(heavier(A, C)),
	  unique_relation(A, C).

% Reversed.

derive([A, B], [C], left):-
	A \= B, A \= C, B \= C. % No information!

derive([A, B], [C], right):-
	  A \= B, A \= C, B \= C,
	  not(heavier(C, A)),
	  assert(heavier(C, A)),
	  unique_relation(C, A),
	  not(heavier(C, B)),
          assert(heavier(C, B)),
	  unique_relation(C, B).

derive([A, B], [C], equal):-
	  A \= B, A \= C, B \= C,
	  not(heavier(C, A)),
	  assert(heavier(C, A)),
	  unique_relation(C, A),
	  not(heavier(C, B)),
	  assert(heavier(C, B)),
	  unique_relation(C, B).

/* --- Situation: Three objects, two different objects on a scale. --- */

derive([A, A], [B], left):-
	  A \= B. % No information!

derive([A, A], [B], right):-
	  A \= B,
	  not(heavier(B, A)),
	  assert(heavier(B, A)),
	  unique_relation(B, A).

derive([A, A], [B], equal):-
	  A \= B,
	  not(heavier(B, A)),
	  assert(heavier(B, A)),
	  unique_relation(B, A).

% Reversed.

derive([A], [B, B], left):-
	  A \= B,
	  not(heavier(A, B)),
          assert(heavier(A, B)),
	  unique_relation(A, B).

derive([A], [B, B], right):-
	  A \= B. % No information!

derive([A], [B, B], equal):-
	  A \= B,
	  not(heavier(A, B)),
          assert(heavier(A, B)),
	  unique_relation(A, B).

%%%%%%%%%%

%derive([A], [B, A], left):-
	  %A \= B. % Inconsistent!

derive([A], [B, A], right):-
	  A \= B. % No information!

%derive([A], [B, A], equal):-
	  %A \= B. % Inconsistent!

% Reversed.

derive([A, B], [A], left):-
	  A \= B. % No information!

%derive([A, B], [A], right):-
	  %A \= B. % Inconsistent!

%derive([A, B], [A], equal):-
	  %A \= B. % Inconsistent!

%%%%%%%%%%

%derive([B], [B, A], left):-
	  %A \= B. % Inconsistent!

derive([B], [B, A], right):-
	  A \= B. % No information!

%derive([B], [B, A], equal):-
	  %A \= B. % Inconsistent!

% Reversed.

derive([A, B], [B], left):-
	  A \= B. % No information!

%derive([A, B], [B], right):-
	  %A \= B. % Inconsistent!

%derive([A, B], [B], equal):-
	  %A \= B. % Inconsistent!

/* --- Situation: Four objects, two different objects on a scale. --- */

derive([A, A], [B, B], left):-
	  A \= B,
	  not(heavier(A, B)),
	  assert(heavier(A, B)),
	  unique_relation(A, B).

derive([A, A], [B, B], right):-
	  A \= B,
	  not(heavier(B, A)),
	  assert(heavier(B, A)),
	  unique_relation(B, A).

derive([A, A], [B, B], equal) :-
	  A \= B,
	  not(equal(A, B)),
	  assert(equal(A, B)),
	  unique_relation(A, B).

/* --- Situation: Four objects, three different objects on a scale. --- */

derive([A, A], [B, C], left):-
	A \= B, A \= C, B \= C. % No information!

derive([A, A], [B, C], right):-
	A \= B, A \= C, B \= C. % No information!

derive([A, A], [B, C], equal):-
	A \= B, A \= C, B \= C. % No information!

% Reversed.

derive([B, C], [A, A], left):-
	A \= B, A \= C, B \= C. % No information!

derive([B, C], [A, A], right):-
	A \= B, A \= C, B \= C. % No information!

derive([B, C], [A, A], equal):-
	A \= B, A \= C, B \= C. % No information!

/* --- Situation: Four objects, four different objects on a scale. --- */

derive([A, B], [C, D], left):-
	A \= B, A \= C, A \= D, B \= C, B \= D, C \= D. % No information!

derive([A, B], [C, D], right):-
	A \= B, A \= C, A \= D, B \= C, B \= D, C \= D. % No information!

derive([A, B], [C, D], equal):-
	A \= B, A \= C, A \= D, B \= C, B \= D, C \= D. % No information!

/* --- Extra derivations. --- */

% The predicate derive_extra/3 derives all the extra facts (information)
% from one scale. For example, the second case states that when there is
% a scale with objects A and C on the left, objects B and D on the
% right, the scale is equal (in balance), and object B is heavier
% than object A, then it can be derived that object C is heavier than
% object D. All the derived facts (information) are asserted to the
% knowledge base.

% If there is a scale with more than one object on both sides, and there
% are two objects on the scale that have an 'equal' relation (and both
% objects are on a different side), then the two objects can be removed
% from the scale and a new derivation can be performed.

derive_extra(L1, L2, Balance) :-
	length(L1, Length1),
	Length1 > 1,
	length(L2, Length2),
	Length2 > 1,
	member(X, L1),
	member(Y, L2),
	X \= Y,
	(   equal(X, Y);
	    equal(Y, X)
	),
	select(X, L1, NewL1),
	select(Y, L2, NewL2),
	derive(NewL1, NewL2, Balance).

% Extra derivations for four objects, four different objects on a scale.

derive_extra([A, C], [B, D], equal) :-
	A \= B, A \= C, A \= D, B \= C, B \= D, C \= D,
	heavier(B, A), !,
	not(heavier(C, D)),
	assert(heavier(C, D)),
	unique_relation(C, D).

derive_extra([A, C], [B, D], equal) :-
	heavier(C, D), !,
	not(heavier(B, A)),
	assert(heavier(B, A)),
	unique_relation(B, A).

derive_extra([A, C], [B, D], equal) :-
	heavier(A, B), !,
	not(heavier(D, C)),
	assert(heavier(D, C)),
	unique_relation(D, C).

derive_extra([A, C], [B, D], equal) :-
	heavier(D, C), !,
	not(heavier(A, B)),
	assert(heavier(A, B)),
	unique_relation(A, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate unique_relation/2 checks wheteher two objects have a
% unique relation. The first case checks whether two objects (Object1
% and Object2) that have a 'heavier' relation, do not have a reversed
% 'heavier' relation and an 'equal' relation. The second case checks
% wheteher two objects (Object1 and Object2) that have an 'equal'
% relation, do not have a 'heavier' relation.
% In other words, the predicate unique_relation/2 checks whether two
% objects really have a unique relation.

unique_relation(Object1, Object2) :-
	heavier(Object1, Object2), !,
	not(heavier(Object2, Object1)),
	not(equal(Object1, Object2)),
	not(equal(Object2, Object1)).

unique_relation(Object1, Object2) :-
	(
	    equal(Object1, Object2);
	    equal(Object2, Object1)
	), !,
	not(heavier(Object1, Object2)),
	not(heavier(Object2, Object1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
