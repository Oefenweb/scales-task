The Prolog files in this folder contain the code for generating game items.

This README file briefly explains how to run the code in order to generate 
game items:

1) Download SWI-Prolog: http://www.swi-prolog.org/Download.html
2) Follow the standard installation procedure.
3) When SWI-Prolog has been downloaded, go to the directory where the Prolog 
   files are stored (.pl files).
4) Open the file 'generate.pl' (double click on the file).
5) The Prolog terminal will be opened. Two error messages and a warning will 
   be shown. These messages can be ignored.
6) Open the file 'generate.pl' in the SWI-Prolog editor: go to 'file' in the
   top left corner and select 'Edit generate.pl'. 
7) You are now in the SWI-Prolog editor. Compile the file 'generate.pl'. This
   can be done in three ways:
   7.1 Go to 'Compile' and select 'Compile buffer'
   7.2 Typ Ctrl-c Ctrl-b (shortcut for 7.1)
   7.3 Typ 'consult('generate.pl').' in the Prolog terminal.
8) Return to the Prolog terminal. The following information is printed in the
   terminal:
   %    ranf compiled 0.00 sec, 3 clauses
   %   normal compiled 0.00 sec, 11 clauses
   %  writeToFile compiled 0.00 sec, 22 clauses
   %   deriveRules compiled 0.02 sec, 40 clauses
   %  checkItem compiled 0.02 sec, 78 clauses
   % generate compiled 0.05 sec, 139 clauses   
9) The file 'generate.pl' is now consulted correctly. In order to generate
   game items, typ: 
   'generate_and_write(+ClassNumber, +NScales, +NDifferent, +NTotal, +N).'
   For example, you can typ 'generate_and_write(test, 1, 2, 2, 4).'. This will 
   generate game items of class 'test' with 1 scale, 2 different objects, 
   2 objects in total, and of all the generated game items, 4 game items will
   be selected.
10) The game items will be generated once you hit 'enter'. In case of the 
    example above, the following information will be printed in the terminal:

    ?- generate_and_write(test, 1, 2, 2, 4).
    Total number of items generated: 4
    Number of items selected: 4
    true.

    Depending on the arguments, this can take a moment.
12) The generated game items are written to the text file 'test.txt',
    which is in the same directory as the Prolog files. If you opens this
    file, the generated game items are visible. Every game item is written on 
    a new line. Every line contains the following information:
    - The class index of the item
    - The representation of the item as a JSON string
    - A list with answer options for the item
    - The right answer to the item (within a list)
    - The maximum response time in seconds (is set to 20 for all the items)
    - A rating for the item
    - A start rating for the item (is equal to the rating)
    All this information is space separated.  
13) If the predicate generate_and_write/5 is called again, the game items will
    be written to the same text file ('test.txt'). The text file will not be 
    overwritten; the generated game items will be appended to the text 
    file. 
	
-------------------------------------------------------------------------------
	
Some information regarding the arguments of the predicate generate_and_write/5:
- The program can only generate game items up to five scales. If +NScales > 5,
  the predicate generate_and_write/5 will fail directly.
- In principle, +NDifferent and +NTotal can be any number. However, the
  predicate generate_and_write/5 will not generate game items if it is not 
  possible to generate game items with the specified parameters, e.g:

  ?- generate_and_write(test, 1, 3, 2, 1).
  Total number of items generated: 0
  Number of items selected: 0

- The program will go out of global stack if the arguments are too complex. 
  This goes for game items with four and five scales for which +NDifferent 
  and +NTotal are too great (it then becomes too complex to generate game 
  items). The excel file 'Classes.xlsx' shows the classes that can be 
  generated without the program going out of global stack.
- +N specifies the number of game items that must be obtained from all the
  generated game items. If the number of game items that have been generated
  is smaller than or equal to N, all the generated items are written to the
  text file. If +N is smaller than the number of game items that have been
  generated, the items are selected in such a way that the 'distance' 
  between the items is equal.
   
If you want to change the text file to which the generated game items are 
written, go to line 46 in the file 'generate.pl' and replace 'test.txt' by 
'file_name.txt'. If 'file_name.txt' already exists, the generated game items
will be written to the text file. If the text file does not exist yet, it will
be created during execution time (note that the text file is in the same 
directory as the program files).

-------------------------------------------------------------------------------

The folder 'checkItem with prints' contains a modified version of the file 
'checkItem.pl'. The predicate item_check/2 in the file 'checkItem.pl' is 
used in the file 'generate.pl' to check if a generated game item can be solved 
and to find the right answer. The predicate item_check/1 in the file 
'checkItemPrints.pl' is a modified version of the file 'checkItems.pl'. The
predicate item_check/1 can be called as follows: 

?- item_check([[[a], [b], right], [[b], [c], right]]).

Determining the heaviest object...

Heaviest object: c
true.

Given a representation of a game item, the predicate prints the heaviest
object. If there is no unique solution, if the game item in inconsistent, or
if the game item contains redundant scales, a message is printed in the
terminal.    

-------------------------------------------------------------------------------

Every predicate in each file is provided with comments that explain what the 
predicate does.