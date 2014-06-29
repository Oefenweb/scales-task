% generate.pl
% Generates game items given a number of parameters.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consult the files 'checkItem.pl' and 'writeToFile.pl' when
% consulting the program. The file 'checkItem.pl' contains code for
% solving and checking an item. The file 'writeToFile.pl' contains
% code for writing the representation of an item to a text file (as a
% JSON string).

:- consult(writeToFile).
:- consult(checkItem).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Define dynamic predicate.

:- dynamic(side/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Hardcoded positions and objects.

positions([left, right, equal]).
allObjects([a, b, c, d, e, f, g, h, i, j, k]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate generate_and_write/5 generates all items of a class
% given four parameters: the class index that specifies the parameter
% values ('Class'), the number of scales within an item ('NScales'), the
% number of different objects within an item ('NDifferent'), and the
% total number of objects within an item ('NTotal'). The fifth argument
% ('N_Desired') is the number of items you want to obtain from all the
% generated items. Every item that is generated and obtained, is written
% to a text file as a JSON string (with meta information). When the
% predicate is called, the file 'test.txt' will be opened and all the
% items will be written in this text file.

generate_and_write(Class, NScales, NDifferent, NTotal, N_Desired) :-
	retractall(side(_, _)),
	findall([Item, Answer, Answer_Options], generate(NScales, NDifferent, NTotal, Item, Answer, Answer_Options), All_Items),
	get_N_items(All_Items, N_Desired, 1, Desired_Items),
	randomize(Desired_Items, Final_Items),
	open('test.txt', append, Stream),
	write(Stream, '---------- '),
	write(Stream, NScales), write(Stream, ', '),
	write(Stream, NDifferent), write(Stream, ', '),
	write(Stream, NTotal),
	write(Stream, ' ----------'), nl(Stream),
	write_items(Stream, NScales, NDifferent, NTotal, Final_Items, Class), !,
	nl(Stream),
	close(Stream),
	length(All_Items, N_All_Items),
	print('Total number of items generated: '), print(N_All_Items), nl,
	length(Final_Items, N_Final_Items),
	print('Number of items selected: '), print(N_Final_Items).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate generate/6 generates an item given three parameters: the
% number of scales within an item ('NScales'), the number of different
% objects within an item ('NDifferent'), and the total number of objects
% within an item ('NTotal'). The function returns an item ('Item'),
% the right answer ('Answer'), and the answer options
% ('Answer_Options'). The answer to an item and the answer options are
% found with the predicate item_check/2 (see the file 'checkItem.pl'
% for this predicate).

/*

Step by step overview of the algorithm:

1. Store ALL the objects in the list 'Objects'.
2. Select NDifferent objects from 'Objects' and store the selected
   objects in the list 'CObjects'
3. Obtain the required number of sides (backtracking enabled)
5. Put all the sides in the list 'Appended'
6. Check if the list 'Appended' is of length NTotal
7. Check if the list 'Appended' contains NDifferent objects
8. Get NScales members from the the list with positions (backtracking
   enabled).
9. Check if the game item can be solved
10.Return the item, the answer, and the answer options

*/

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

%%%%%%%%%%

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

%%%%%%%%%%

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

%%%%%%%%%%

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

%%%%%%%%%%

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
% different objects within an item (the predicate actually generates the
% sides for a scale). There are four possible combinations of sides:
% - One object on both sides.
% - One object on the left side and two objects on the right side.
% - Two objects on the left side and one object on the right side.
% - Two objects on both sides.
% These four combinations are incorporated in this predicate.

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

% Two objects on both sides. Same sides are excluded while this
% inconsistent or redundant (depending on how the scale is tilted).

findSides(CObjects, Side1, Side2) :-
	getCombination(CObjects, Side1),
	getCombination(CObjects, Side2),
	Side1 \= Side2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate getElement/2 obtains (and returns) one element from a
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
	not(side(Side1, Side2)). % Reduces the possible configurations of scales within an item!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate countUnique/2 checks whether a list contains N unique
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

% The predicate randomize/2 randomizes the elements within a list. The
% predicate is used to randomize the scales within an item when the item
% has been generated.

% Base case.
randomize([], []).

% Recursive step.
randomize([[Item, Answer, Answer_Options] | Others], [[Randomized_Item, Answer, Answer_Options] | Rest]) :-
	random_permutation(Item, Randomized_Item),
	randomize(Others, Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate write_items/6 writes an item to a text file using the
% predicate write_to_text_file/8 (see the file 'writeToFile.pl' for this
% predicate).

% Base case.
write_items(_, _, _, _, [], _).

% Recursive step.
write_items(Stream, NScales, NDifferent, NTotal, [[Item, Answer, Answer_Options] | Others], Class) :-
	write_to_text_file(Stream, NScales, NDifferent, NTotal, Item, Answer_Options, Answer, Class),
	write_items(Stream, NScales, NDifferent, NTotal, Others, Class).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
