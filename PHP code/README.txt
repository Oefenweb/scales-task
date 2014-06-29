This is the README file for the PHP code.

Note that the PHP code only works when it is run on a server that supports 
PHP, like XAMPP Apache!

-------------------------------------------------------------------------------

PHP files:

- index.php
- createScale.php
- visualisationItem.php
- randomiseObjects.php
- otherFunctions.php

Images:

- Weight_1.png
- Weight_2.png
- Weight_3.png
- Weight_4.png
- Weight_5.png
- Weight_6.png
- Weight_7.png
- Scale.png
- Stand.png

Other files:

- all_items.txt (contains the representations of the game items as JSON 
  strings with meta information)

-------------------------------------------------------------------------------

This part of the README file briefly explains the code within every PHP file.

- index.php: contains the code for reading the file 'all_items.txt' and uses
  the functions in all the other php files to create an image of a game item.

- creatScale.php: contains the code for creating an image of a single scale by
  combining different images.

- visualisationItem.php: contains the code for creating an image of a game 
  item.

- randomiseObjects.php: contains the code for randomising which images are
  linked to which objects.

- otherFunctions.php: contains some other functions that are used to visualise 
  a game item.

-------------------------------------------------------------------------------
  
For a more detailed description of all the functions within the PHP files, the 
comments in the code can be consulted.

-------------------------------------------------------------------------------

If you run the file index.php, an image of the first game item will be shown.
If you refresh the web page, you can see that different images are assigned to
the objects (though the same objects are represented by the same image). If 
you whish to visualise another game item, go to line 57 in the file index.php
and change '$allItems[0]' to '$allItems["number"]' where "number" must be an
integer between 0 and 1499 (there are 1500 game items in total). If "number"
is not an integer between 0 and 1499, an empty image will be shown. 

-------------------------------------------------------------------------------