% generateImproved.pl
% Generates items given a number of parameters.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consult the files 'checkItem2.pl' and 'writeToFile.pl' when
% consulting the program. The file 'checkItem2.pl' contains code for
% solving (and checking) an item. The file 'writeToFile.pl' contains
% code for writing the representation of an item to a text file (as a
% JSON string).

:- consult(checkItem2).
:- consult(writeToFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define dynamic predciate.

:- dynamic(side/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hardcoded positions and objects.

positions([left, right, equal]).
allObjects([a, b, c, d, e, f, g, h, i, j, k]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate generate_and_write/5 generates all items of a class
% given four parameters: the number of scales within an item
% ('NScales'), the number of different objects within an item
% ('NDifferent'), the total number of objects within an item ('NTotal'),
% and the class that specifies the parameters ('Class'). The fifth
% argument ('N_Desired') is the number of items you want to obtain
% from all the generated items. Every item that is generated and
% obtained, is written to a text file as a JSON string. When the
% predicate is called, the file 'all_items.txt' will be opened and all
% the items will be written in this text file.

generate_and_write(NScales, NDifferent, NTotal, Class, N_Desired) :-
	retractall(side(_, _)),
	findall([Item, Answer, Answer_Options], generate(NScales, NDifferent, NTotal, Item, Answer, Answer_Options), All_Items),
	get_N_items(All_Items, N_Desired, 1, Desired_Items),

	%print(Desired_Items), nl,
	randomize(Desired_Items, Final_Items),
	%print(Final_Items), nl,

	open('all_items.txt', append, Stream),
	write(Stream, '---------- '),
	write(Stream, NScales), write(Stream, ', '),
	write(Stream, NDifferent), write(Stream, ', '),
	write(Stream, NTotal),
	write(Stream, ' ----------'), nl(Stream),
	write_items(Stream, NScales, NDifferent, NTotal, Final_Items, Class), !,
	nl(Stream), % New line in stream.
	close(Stream),

	length(All_Items, N_All_Items),
	print('Total number of items generated: '), print(N_All_Items), nl,
	length(Final_Items, N_Final_Items),
	print('Number of items selected: '), print(N_Final_Items).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

randomize([], []).

randomize([[Item, Answer, Answer_Options] | Others], [[Randomized, Answer, Answer_Options] | Rest]) :-
	random_permutation(Item, Randomized),
	randomize(Others, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate write_items/3 writes an item to a text file using the
% predicate write_to_text_file/4 (see the file 'writeToFile.pl' for this
% predicate).

% Base case.
write_items(_, _, _, _, [], _).

% Recursive step.
write_items(Stream, NScales, NDifferent, NTotal, [[Item, Answer, Answer_Options] | Others], Class) :-
	write_to_text_file(Stream, NScales, NDifferent, NTotal, Item, Answer_Options, Answer, Class),
	write_items(Stream, NScales, NDifferent, NTotal, Others, Class).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate generate/6 generates an item given three parameters: the
% number of scales within an item ('NScales'), the number of different
% objects within an item ('NDifferent'), and the total number of objects
% within an item ('NTotal'). The function returns an item ('Item'),
% the right answer ('Answer'), and the answer options
% ('Answer_Options'). The answer for an item and the answer options are
% found with the predicate item_check/2 (see the file 'checkItem2.pl'
% for this predicate).

% Short description of the algorithm here...

% Generate items with one scale.
generate(1, NDifferent, NTotal, Item, Answer, Answer_Options):-
        allObjects(Objects),
        getNObjects(NDifferent, Objects, CObjects),

	findSides(CObjects, Side1, Side2),

	flatten([Side1, Side2], Appended),
	length(Appended, NTotal),
	countUnique(Appended, NDifferent),
	positions(Positions),
	member(CurrentPos, Positions),
        item_check([[Side1, Side2, CurrentPos]], Answer, Answer_Options),
        Item = [[Side1, Side2, CurrentPos]].

% Generate items with two scales.
generate(2, NDifferent, NTotal, Item, Answer, Answer_Options):-
        allObjects(Objects),
        getNObjects(NDifferent, Objects, CObjects),

	findSides(CObjects, Side1, Side2),
	assert(side(Side1, Side2)),
	findSides(CObjects, Side3, Side4),
	check_sides(Side3, Side4),

	flatten([Side1, Side2, Side3, Side4], Appended),
	length(Appended, NTotal),
	countUnique(Appended, NDifferent),
        positions(Positions),
        member(CurrentPos1, Positions),
        member(CurrentPos2, Positions),
        item_check([[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2]], Answer, Answer_Options),
        Item = [[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2]].

% Generate items with three scales.
generate(3, NDifferent, NTotal, Item, Answer, Answer_Options):-
        allObjects(Objects),
        getNObjects(NDifferent, Objects, CObjects),

	findSides(CObjects, Side1, Side2),
	assert(side(Side1, Side2)),
	findSides(CObjects, Side3, Side4),
	check_sides(Side3, Side4),
	assert(side(Side3, Side4)),
	findSides(CObjects, Side5, Side6),
	check_sides(Side5, Side6),

	flatten([Side1, Side2, Side3, Side4, Side5, Side6], Appended),
	length(Appended, NTotal),
	countUnique(Appended, NDifferent),
        positions(Positions),
        member(CurrentPos1, Positions),
        member(CurrentPos2, Positions),
        member(CurrentPos3, Positions),
        item_check([[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2], [Side5, Side6, CurrentPos3]], Answer, Answer_Options),
        Item = [[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2], [Side5, Side6, CurrentPos3]].

% Generate items with four scales.
generate(4, NDifferent, NTotal, Item, Answer, Answer_Options):-
        allObjects(Objects),
        getNObjects(NDifferent, Objects, CObjects),

	findSides(CObjects, Side1, Side2),
	assert(side(Side1, Side2)),
	findSides(CObjects, Side3, Side4),
	check_sides(Side3, Side4),
	assert(side(Side3, Side4)),
	findSides(CObjects, Side5, Side6),
	check_sides(Side5, Side6),
	assert(side(Side5, Side6)),
	findSides(CObjects, Side7, Side8),
        check_sides(Side7, Side8),

        flatten([Side1, Side2, Side3, Side4, Side5, Side6, Side7, Side8], Appended),
        length(Appended, NTotal),
        countUnique(Appended, NDifferent),
        positions(Positions),
        member(CurrentPos1, Positions),
	member(CurrentPos2, Positions),
	member(CurrentPos3, Positions),
	member(CurrentPos4, Positions),
        item_check([[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2], [Side5, Side6, CurrentPos3], [Side7, Side8, CurrentPos4]], Answer, Answer_Options),
        Item = [[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2], [Side5, Side6, CurrentPos3], [Side7, Side8, CurrentPos4]].

% Generate items with five scales.
generate(5, NDifferent, NTotal, Item, Answer, Answer_Options):-
        allObjects(Objects),
        getNObjects(NDifferent, Objects, CObjects),

	findSides(CObjects, Side1, Side2),
	assert(side(Side1, Side2)),
	findSides(CObjects, Side3, Side4),
	check_sides(Side3, Side4),
	assert(side(Side3, Side4)),
	findSides(CObjects, Side5, Side6),
	check_sides(Side5, Side6),
	assert(side(Side5, Side6)),
	findSides(CObjects, Side7, Side8),
        check_sides(Side7, Side8),
	assert(side(Side7, Side8)),
	findSides(CObjects, Side9, Side10),
	check_sides(Side9, Side10),

        flatten([Side1, Side2, Side3, Side4, Side5, Side6, Side7, Side8, Side9, Side10], Appended),
        length(Appended, NTotal),
        countUnique(Appended, NDifferent),
        positions(Positions),
        member(CurrentPos1, Positions),
	member(CurrentPos2, Positions),
	member(CurrentPos3, Positions),
	member(CurrentPos4, Positions),
	member(CurrentPos5, Positions),
        item_check([[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2], [Side5, Side6, CurrentPos3], [Side7, Side8, CurrentPos4], [Side9, Side10, CurrentPos5]], Answer, Answer_Options),
        Item = [[Side1, Side2, CurrentPos1], [Side3, Side4, CurrentPos2], [Side5, Side6, CurrentPos3], [Side7, Side8, CurrentPos4], [Side9, Side10, CurrentPos5]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate getNObjects/3 returns N objects from a list (the first
% N objects).

% Base case.
getNObjects(1, [H|_], [H]) :- !.

% Recursive step.
getNObjects(N, [H|T], [H|Result]) :-
	NewN is N - 1,
	getNObjects(NewN, T, Result).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate findSides/3 returns two sides for a scale given the
% number of differen objects within an item (the predicate actually
% generates the sides for a scale). There are four combination of sides
% possible:
% - One object on both sides.
% - One object on the left side and two objects on the right side.
% - Two objects on the left side and one object on the right side.
% - Two objects on both sides.
% These four combinations are incorporated in this predciate.

% One object on both sides. [X], [X] is excluded while this is
% inconsistent or redundant (depending on how the scale is tilted).

findSides(CObjects, Side1, Side2) :-
	getElement(CObjects, Side1),
	subtract(CObjects, Side1, NCObjects),
	getElement(NCObjects, Side2).

% One object on the left side and two object on the right side.
% [X], [X, X] is excluded while this is inconsistent or redundant
% (depending on how the scale is tilted).

findSides(CObjects, Side1, Side2) :-
	getElement(CObjects, Side1),
	getCombination(CObjects, Side2),
	append(Side1, Side1, Double),
	Side2 \= Double.

% Two objects on the left side and one object on the right side.
% [X, X], [X] is excluded while this is inconsistent or redundant
% (depending on how the scale is tilted).

findSides(CObjects, Side1, Side2) :-
	getCombination(CObjects, Side1),
	getElement(CObjects, Side2),
	append(Side2, Side2, Double),
	Side1 \= Double.

% Two objects on both sides. Same sides are excluded while this in
% inconsistent or redundant (depending on how the scale is tilted).

findSides(CObjects, Side1, Side2) :-
	getCombination(CObjects, Side1),
	getCombination(CObjects, Side2),
	Side1 \= Side2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate getElement/2 obtains (and returns) an element from a
% list.

% Base case.
getElement([H|_], [H]).

% Recursive step.
getElement([_|T], Ans) :-
	getElement(T, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate getCombination/2 returns a combination of two elements
% from a list.

% Base case.
getCombination([H|T], [H|Element]) :-
	getElement([H|T], Element).

% Recursive step.
getCombination([_|T], Ans) :-
	getCombination(T, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate check_sides/2 checks whether there already is a scale
% with the same sides. If so, the predicate returns false. It is
% redundant or inconsistent if there are two scales with the same
% sides within an item (this depends on how the scales are tilted).

check_sides(Side1, Side2) :-
	(
	    side(Side1, Side2)%; % Reduces the possible configurations of scales within an item!
	    %side(Side2, Side1)
	) ->
	false;
	true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate countUnique/2 checks wheter a list contains N unique
% elements.

% Base case.
countUnique([_], 1).

% Recursive step.
countUnique([H|T], N) :-
	member(H, T), !,
	countUnique(T, N).

countUnique([_|T], NewN) :-
	countUnique(T, N), !,
	NewN is N + 1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate get_N_items/4 returns N items from a list with items.
% The items are selected in such a way that the 'distance' between the
% items is equal.

% If the total number of items in the list is smaller than or equal to
% N, the whole list is returned.

get_N_items(All_Items, N, _, All_Items) :-
	length(All_Items, N_Items),
	N_Items =< N, !.

% If N is smaller than the number of items in the list, the number of
% items in the list is divided by N. The resulting answer is floored and
% results in the step size. The predicate get_items/5 then selects the
% first item from the list, than the '1 + step size' item from the list
% etc. untill N items are selected from the list.

get_N_items(All_Items, N, Current, Final_Items) :-
	length(All_Items, N_Items),
	Step_Size is floor(N_Items / N),
	get_items(All_Items, N, Step_Size, Current, Final_Items).

% Base case.
get_items(_, 0, _, _, []) :- !.

% Recursive step.
get_items(All_Items, Counter, Step_Size, Current, [Item1 | Rest]) :-
	element_at(Current, All_Items, Item1),
	New_Counter is Counter - 1,
	New_Current is Current + Step_Size,
	get_items(All_Items, New_Counter, Step_Size, New_Current, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate element_at/3 returns the nth element from a list.

% Base case.
element_at(1, [Element | _], Element) :- !.

% Recursive step.
element_at(N, [_ | Other_Elements], Element) :-
	N1 is N - 1,
	element_at(N1, Other_Elements, Element).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate checkDoubles/1 checks if an item contains two
% identical scales. The predicate fails if this is the case.

% Can be removed because of check_sides?

checkDoubles([List1, List2]) :-
	List1 \= List2.

checkDoubles([List|Tail]) :-
	not(member(List, Tail)),
	checkDoubles(Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_print([]).

test_print([H | T]) :-
	print(H), nl,
	test_print(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
getScale(CObjects, [Left, Right, CurrentPos]):-
      findSide(CObjects, Left), findSide(CObjects, Right),
      positions(Positions), member(CurrentPos, Positions).


generate(1, NDifferent, NTotal, NBalanced, [[Left, Right, CurrentPos]]):-
      allObjects(Objects), getNObjects(NDifferent, Objects, CObjects),
      getScale(CObjects, NDifferent, [Left, Right, CurrentPos]),
      appendAll([Left, Right], AllObjects),
      length(AllObjects, NTotal),
      countUnique(AllObjects, NDifferent),
      count(equal, [CurrentPos], NBalanced),
      item_check([[Left, Right, CurrentPos]]).


generate(2, NDifferent, NTotal, NBalanced, [[L1, R1, Pos1], [L2, R2, Pos2]]):-
      allObjects(AllObjects),
      NDifferent is N1 + N2,
      getNObjects(N1, AllObjects, CObjects1),
      deleteList(AllObjects, CObjects1, NewObjects),
      getNObjects(N2, NewObjects, CObjects2),
      getScale(CObjects1, N1, [L1, R1, Pos1]),
      getScale(CObjects2, N2, [L2, R2, Pos2]),
      print(CObjects1), nl, print(CObjects2),
      appendAll([L1, R1, L2, R2], AllObjects),
      length(AllObjects, NTotal),
      countUnique(AllObjects, NDifferent),
      count(equal, [Pos1, Pos2], NBalanced),
      item_check([[L1, R1, Pos1], [L2, R2, Pos2]]).
*/
