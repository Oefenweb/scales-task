% checkItemPrints.pl
% Finds the solution for a given item (if there is a solution) and
% mutually checks if the item in not inconsistent and/or redundant.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% IMPORTANT: The code in this file is almost the same as the code in the
% file 'checkItem.pl'. The predicate item_check/1 in this file can be
% called with a representation of an item as the only argument. If there
% is a unique solution, the solution is printed to the screen. If there
% is no unique solution (no heaviest object), if the item is
% inconsistent, or if the item contains redundant scales, a message is
% printed to the screen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consult the file 'deriveRules.pl' when consulting the program. The
% file 'deriveRules.pl' contains all the derivation rules for one
% scale.

:- consult(deriveRules).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define dynamic predicates.

:- dynamic heavier/2.
:- dynamic equal/2.
:- dynamic object/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate item_check/1 finds the solution for a given item. If
% there is a unique solution (a heaviest object), the solution is
% printed to the screen. If there is no unique solution, if the item is
% inconsitent, or of the item contains redundant scales, a message is
% printed to the screen.

/*

Step by step overview of the algorithm:

1. Reset the knowledge base.
2. Check if the item contains inconsistent scales. An item contains
   inconsistent scales if there are two scales within the item that have
   the same objects on the left side and on the right side, but which
   are tilted in different ways. The predicate fails if that is the
   case.
3. Obtain all the objects from the item and assert the objects to the
   knowledge base.
4. Assert all the facts about the relations between the objects to the
   knwoledge base, and mutually check for inconsistency and redundancy.
   A message is printed to the screen if the item is inconsistent and/or
   if the item contains redundant scales.
5. Find the haviest object. A message is printed to the screen if there
   is no unique solution.

*/

item_check(Representation) :-
	reset,
	check_inconsistent_scales(Representation),
	add_all_objects(Representation),
	add_all_facts(Representation), !,
	find_heaviest_object.

% If the predicate add_all_facts/1 fails, the item is inconsistent
% and/or redundant.

item_check(_) :-
	nl, print('Item inconsistent!'), nl, nl,
	print('There are two possibilities:'), nl, nl,
	print('    - The item contains inconsistent scales.'), nl,
	print('    - The item contains redundant scales.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate reset/0 resets the knowledge base: all the facts
% (relations) are retracted from the knowledge base.

reset :-
	retractall(heavier(_, _)),
	retractall(equal(_, _)),
	retractall(object(_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate check_inconsistent_scales/1 checks whether an item
% contains inconsistent scales. An item contains inconsistent scales if
% there are two scales within the item that have the same objects on the
% left side and on the right side, but which are tilted in different
% ways. For example, an item with the scales [[a], [b], left] and [[a],
% [b], right] or [[a], [b], equal] is inconsistent.

% Base case.
check_inconsistent_scales([]).

% Recursive step.
check_inconsistent_scales([Scale | Other_Scales]) :-
	check_scale(Scale, Other_Scales),
	check_inconsistent_scales(Other_Scales).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate check_scale/2 actually checks whether a scale is
% inconsistent with the other scales.

% Base case.
check_scale([_, _, _], []).

% Recursive step.
check_scale([ObjectsLeft, ObjectsRight, _], [[ObjectsLeft2, ObjectsRight2, _] | _]) :-
	ObjectsLeft == ObjectsLeft2,
	ObjectsRight == ObjectsRight2, !,
	false.

check_scale([ObjectsLeft, ObjectsRight, Position], [[_, _, _] | T]) :-
	check_scale([ObjectsLeft, ObjectsRight, Position], T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_all_objects/1 obtains all the (unique) objects from
% an item and asserts the objects to the knowledge base.

add_all_objects(Representation) :-
	flatten(Representation, Representation_Flattened),
	delete(Representation_Flattened, left, Result1),
	delete(Result1, right, Result2),
	delete(Result2, equal, All_Objects),
	remove_dup(All_Objects, Final_Objects),
	add_objects(Final_Objects).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate remove_dup/2 removes the duplicates from a given input
% list and returns a list with unique elements.

% Base case.
remove_dup([], []).

% Recursive step.
remove_dup([First | Rest], NewRest) :-
    member(First, Rest), !,
    remove_dup(Rest, NewRest).

remove_dup([First | Rest], [First | NewRest]) :-
    remove_dup(Rest, NewRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_objects/1 asserts all the elements (objects) from
% a given input list to the knowledge base. Note that the predicate
% first checks if the object is not already in the knowledge base before
% asserting it!

% Base case.
add_objects([]).

% Recursive step.
add_objects([H|T]) :-
	not(object(H)), !, % Check if the object is not already in the knowledge base.
	assert(object(H)),
	add_objects(T).

add_objects([_|T]) :-
	add_objects(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_all_facts/1 asserts all the facts (information) that
% can be derived from an item to the knowledge base.

% Base case.
add_all_facts([]).

% Recursive step.
add_all_facts([H | T]) :-
	% Assert all the facts (information) that can be derived from one scale to the knowledge base.
	add_facts_one_scale(H),
	% Derive facts (information) from multiple scales (transitivity).
	derive_facts,
	% Recursive step.
	add_all_facts(T),
	% Extra derivations!
	add_extra_facts_one_scale(H),
	% Derive facts (information) from multiple scales (again).
	derive_facts.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_facts_one_scale/3 asserts all the facts
% (information) that can be derived from one scale to the knowledge base
% (using the derive/3 predicate, see the file 'deriveRules.pl').

add_facts_one_scale([Objects_Left, Objects_Right, Position]) :-
	derive(Objects_Left, Objects_Right, Position), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_extra_facts_one_scale/3 asserts all the extra
% facts (information) that can be derived from one scale to the
% knowledge base (using the derive_extra/3 predicate, see the file
% 'deriveRules.pl').

add_extra_facts_one_scale([Objects_Left, Objects_Right, Position]) :-
	derive_extra(Objects_Left, Objects_Right, Position), !.

add_extra_facts_one_scale([_, _, _]) :-
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate derive_facts/0 asserts all the facts (information) that
% can be derived from multiple scales to the knowledge base
% (transitivity).

derive_facts :-
	add_transitivity_heavier,
	add_equal.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_transitivity_heavier/0 finds all the transitivity
% relations regarding the 'heavier' relation. The found relations are
% asserted to the knowledge base.

add_transitivity_heavier :-
	(   find_transitivity_heavier(Relation),
	!,
	assert(Relation),
	add_transitivity_heavier);
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate find_transitivity_heavier/1 finds (actually derives) a
% transitivity relation regarding the 'heavier'	relation. For example,
% if object A is heavier than object B, and object B is heavier
% than object C, then object A is also heavier than object C (first
% case). Another example: if object A is equal to object B, and object A
% is heavier than object C, then object B is also heavier than object C
% (second case). There are four cases of the last type. All these cases
% are incorporated in the predicate.

find_transitivity_heavier(heavier(A, C)) :-
	heavier(A, B),
	heavier(B, C),
	A \= C,
	% Check if the found relation is not already in the knowledge base.
	not(heavier(A, C)).

%%%%%%%%%%

find_transitivity_heavier(heavier(B, C)) :-
	equal(A, B),
	heavier(A, C),
	B \= C,
	% Check if the found relation is not already in the knowledge base.
	not(heavier(B, C)).

find_transitivity_heavier(heavier(C, B)) :-
	equal(A, B),
	heavier(C, A),
	B \= C,
	% Check if the found relation is not already in the knowledge base.
	not(heavier(C, B)).

find_transitivity_heavier(heavier(A, C)) :-
	equal(A, B),
	heavier(B, C),
	A \= C,
	% Check if the found relation is not already in the knowledge base.
	not(heavier(A, C)).

find_transitivity_heavier(heavier(C, A)) :-
	equal(A, B),
	heavier(C, B),
	A \= C,
	% Check if the found relation is not already in the knowledge base.
	not(heavier(C, A)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate add_equal/0 finds all the (transitivity) relations
% regarding the 'equal' relation. The found relations are asserted to
% the knowledge base.

add_equal :-
	(   find_equal(Relation),
	!,
	assert(Relation),
	add_equal);
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate find_equal/1 finds (actually derives) a (transitivity)
% relation regarding the 'equal' relation. For example, if object B is
% equal to object A, then object A is also equal to object B (first
% case). Another example: if object A is equal to object B, and object B
% is equal to object C, then object A is also equal to object C (second
% case).

find_equal(equal(A, B)) :-
	equal(B, A),
	% Check if the found relation is not already in the knowledge base.
	not(equal(A, B)).

%%%%%%%%%%

find_equal(equal(A, C)) :-
	equal(A, B),
	equal(B, C),
	A \= C,
	% Check if the found relation is not already in the knowledge base.
	not(equal(A, C)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate find_heaviest_object/1 finds the heaviest object. The
% predicate find_heaviest_object/0 calls the predicate
% determine_heaviest_object/3. That predicate actually finds the
% heaviest object. If a heaviest object is found, the object is printed
% to the screen. If there is no heaviest object (no unique solution), a
% message is printed to the screen.

find_heaviest_object :-
	nl, print('Determining the heaviest object...'), nl, nl,
	findall(X, object(X), All_Objects),
	(
	    determine_heaviest_object(All_Objects, All_Objects, Heaviest_Object) ->
	    print('Heaviest object: '), print(Heaviest_Object);
	    print('No unique solution found!')
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate determine_heaviest_object/3 actually finds the heaviest
% object. The predicate first finds the object for which there is no
% no other heavier object (first case). The predicate then checks if the
% found object has no 'equal' relations with other objects (second
% case).

determine_heaviest_object([Object | _], All_Objects, Heaviest_Object) :-
	heavier(X, Object), !,
	select(Object, All_Objects, Result),
	select(X, Result, New_Result),
	determine_heaviest_object([X | New_Result], Result, Heaviest_Object).

% If the found object has no 'equal' relations with other objects, the
% predicate check_other_objects/2 is called. This predicate checks if
% the possibly heaviest object is actually heavier than all the other
% objects.

determine_heaviest_object([Object | Rest], _, Object) :-
	not(equal(_, Object)), !,
	check_other_objects(Object, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate check_other_objects/2 checks if a possibly heaviest
% object is actually heavier than all the other objects.

% Base case.
check_other_objects(_, []).

% Recursive step.
check_other_objects(Object, [H | T]) :-
	heavier(Object, H),
	check_other_objects(Object, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
