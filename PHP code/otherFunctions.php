<?php
	// Second year Project Artificial Intelligence 2014
	// Gerben van der Huizen - 10460748
	// Vincent Erich - 10384081
	// Michiel Boswijk - 10332553
	// Michael Chen - 10411941
	
	/**
	 * Get the values from the objects within the object that represents the
	 * whole game item.
	 *
	 * The function 'getObjects' goes through the objects within the object 
	 * that represents the whole game item, and returns all the values of
	 * these objects in an array.
	 * The variable $obj is the main object which is looped through (the game
	 * item).
	 *
	 * @var obj
	 */	
	function getObjects($obj, $key = null)
	{
		$scaleObjects = array();
		if (is_object($obj)) {
			foreach ($obj as $x => $value) {
				if (is_object($value)) {
					foreach ($value as $y) {
						array_push($scaleObjects, $y);
					}
				}
			}
		} 
		return $scaleObjects;
	}

	/**
	 * Constructs a visual representation of a whole game item
	 *
	 * The function 'createScales' uses the function 'createScale' to create
	 * images for all the scales in the array $scaleObjects. The function 
	 * returns an array with images for all the scales.
	 *
	 * @var scaleObjects
	 */	
	function createScales($scaleObjects)
	{
		$scale = array();
		$scalePictures = array();
		$counter = 0;
		foreach($scaleObjects as $x) 
		{
			array_push($scale, $x);
			$counter++;
			if ($counter == 3) 
			{
				$scalePicture = createScale($scale);
				array_push($scalePictures, $scalePicture);
				$scale = array();
				$counter = 0;
			}
		}
		return $scalePictures;
	}
?>