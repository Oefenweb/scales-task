<?php
	// Second year Project Artificial Intelligence 2014
	// Gerben van der Huizen - 10460748
	// Vincent Erich - 10384081
	// Michiel Boswijk - 10332553
	// Michael Chen - 10411941
	
	// Include necessary php files.
	include 'visualisationItem.php';
	include 'createScale.php';
	include 'randomiseObjects.php';
	include 'otherFunctions.php';

	// The size of the canvas where the item is outputted.
	$width = isset($_GET['width']) ? $_GET['width'] + 0 : 800;
	$height = isset($_GET['height']) ? $_GET['height'] + 0 : 500;

	// Read the text file 'all_items.txt'. This text file contains the
	// the generated game items. 
	$handle = @fopen("all_items.txt", "r");
	$arrayOfItems = array();
	
	// Store all the lines from the text file in an array.
	if ($handle) 
	{
		while (($buffer = fgets($handle, 4096)) !== false) 
		{
			array_push($arrayOfItems, $buffer);
		}
		if (!feof($handle)) 
		{
			echo "Error: unexpected fgets() fail\n";
		}
		fclose($handle);
	}

	$result = count($arrayOfItems);
	$allItems = array();
	
	// For every line in the array, split the line on white spaces.
	// Every element of the line is stored in an array.
	for ($i = 0; $i <= ($result - 1); $i++) 
	{
		$stringArray = explode(' ', $arrayOfItems[$i]);
		if(in_array('----------', $stringArray) == 0)
		{	
			if(count($stringArray) > 1)
			{
				$item = json_decode($stringArray[1]);
				array_push($allItems, $item);
			}
		}
	}
	
	// Creat an image of the first item in the text file!
	// Change the index of $allItems for a different item!
	$allObjects = getObjects($allItems[0]);		// Get all the objects in the item.
	$newObjects = randomObjects($allObjects);	// Randomize the objects used in the item.
	$scalePictures = createScales($newObjects);	// Create images of the scales within the item.
	$canvas = createCanvas($width, $height);	// Create the canvas.
	
	scalePositions($canvas, $scalePictures);	// Determine the positions of the scales on the canvas.
	
	// Output the image of the game item.
	header('Content-type: image/png');
	imagepng($canvas);
	imagedestroy($canvas);
?>