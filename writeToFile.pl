% writeToFile.pl
% Writes the representation of an item to a text file as a JSON string.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult(normal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The predicate write_to_text_file/5 writes the representation of an
% item to a text file as a JSON string. This string also contains the
% right answer and the class number of the item.

write_to_text_file(Stream, NScales, NDifferent, NTotal, Item, Answer_Options, Answer, Class) :-
	write(Stream, Class),
	write(Stream, '\s'),

	write(Stream, '{'),
	modify_representation(Stream, Item, 1, NScales), !,
	%write(Stream, ', "answer_options":"'),
	%write_objects(Stream, Answer_Options),
	%write(Stream, '", "answer":"'), write(Stream, Answer),
	%write(Stream, '", "class":'), write(Stream, Class),
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






