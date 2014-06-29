% writeToFile.pl
% Writes the representation of an item to a text file as a JSON string
% (with meta information).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Consult the file 'normal.pl' when consulting the program. The file
% 'normal.pl' contains code for generating a normal distribution.

:- consult(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate write_to_text_file/8 writes the representation of an
% item to a text file as a JSON string. The predicate also writes some
% meta information to the text file. Every item is written on a new
% line. Every line contains the following information:
% - The class index of the item
% - The representation of the item as a JSON string
% - A list with answer options for the item
% - The right answer to the item (within a list)
% - The maximum response time in seconds (is set to 20 for all the
%   items)
% - A rating for the item
% - A start rating for the item (is equal to the rating)
% All this information is space separated. The rating of a game items is
% calculated as follows: (NScales + NDifferent + NTotal) - 3 + a random
% number from the normal distribution with mean zero and SD 0.5.

write_to_text_file(Stream, NScales, NDifferent, NTotal, Item, Answer_Options, Answer, Class) :-
	write(Stream, Class),

	write(Stream, '\s{'),
	modify_representation(Stream, Item, 1, NScales), !,
	write(Stream, '}'),

	write(Stream, '\s['), write_answer_options(Stream, Answer_Options), write(Stream, ']'),

	write(Stream, '\s'), write(Stream, '["'), write(Stream, Answer), write(Stream, '"]'),

	write(Stream, '\s'), write(Stream, '20'),

	normal(0, 0.5, Noise),
	Rating is (NScales + NDifferent + NTotal) - 3 + Noise,
	Start_Rating = Rating,

	write(Stream, '\s'), write(Stream, Rating),

	write(Stream, '\s'), write(Stream, Start_Rating),
	nl(Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate modify_representation/4 actually writes the
% representation of an item to a text file as a JSON string.

% Base case.
modify_representation(_, [], _, 0).

% Recursive step.
modify_representation(Stream, [[ObjectsLeft, ObjectsRight, Position] | T], ItemNumber, Counter) :-
	write(Stream,'"'), write(Stream, ItemNumber), write(Stream, '":{"objectsLeft":"'),
	write_objects(Stream, ObjectsLeft),
	write(Stream, '","objectsRight":"'),
	write_objects(Stream, ObjectsRight),
	write(Stream, '","position":"'), write(Stream, Position), write(Stream, '"}'),
	NewItemNumber is ItemNumber + 1,
	NewCounter is Counter - 1,
	(   NewCounter \= 0 ->
	(   write(Stream, ','),
	    modify_representation(Stream, T, NewItemNumber, NewCounter)
	);
	modify_representation(Stream, T, NewItemNumber, NewCounter)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate write_objects/2 writes the elements (objects) from a
% list to a text file.

% Base case.
write_objects(_, []).

% Recursive step.
write_objects(Stream, [H | T]) :-
	write(Stream, H),
	write_objects(Stream, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate write_answer_options/2 writes the answer options for an
% item to a text file. The answer options are written in sub-lists and
% are enclosed within one list.

write_answer_options(_, []).

write_answer_options(Stream, [H | T]) :-
	write(Stream, '["'), write(Stream, H), write(Stream, '"]'),
	(   T \= [] ->
	(    write(Stream, ','),
	     write_answer_options(Stream, T)
	);
	write_answer_options(Stream, T)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%






