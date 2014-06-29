<?php
	// Second year Project Artificial Intelligence 2014
	// Gerben van der Huizen - 10460748
	// Vincent Erich - 10384081
	// Michiel Boswijk - 10332553
	// Michael Chen - 10411941
	
	/**
	 * Creates a list with random numbers from a list with chars.
	 *
	 * The function 'randomObjects' replaces the old representation of objects,
	 * a string of chars, with the the new representation of objects, a string
	 * of random numbers. The random number represent the objects. 
	 * Random objects are needed in 'createScale.php', so that the objects are 
	 * not always represented by the same image. The function uses the 
	 * the variables $numberArray and $stringArray to remember which chars and
	 * numbers have already been handled so the same random number (between 1
	 * and 7) can not be selected again (unless it is assigned to the same 
	 * object). The for loop skips the non-object strings.
	 * There are different cases for the array $scaleObjects for when a side 
	 * contains two objects instead of one. 
	 * The function returns a similar array as $scaleObjects but with random 
	 * numbers assigned to the objects instead of chars.
	 *
	 * @var scaleObjects
	 */	
	function randomObjects($scaleObjects) 
	{
		$numberObjects = count($scaleObjects);
		$numberArray = array();
		$stringArray = array();
		$counter = 1;
		for ($i = 0; $i <= ($numberObjects - 1); $i++) 
		{
			if(($counter % 3) != 0)
			{
				if(strlen($scaleObjects[$i]) == 2)
				{
					$assignedNumbers = array();
					$objects = str_split($scaleObjects[$i]);
					foreach($objects as $x)
					{
						if(in_array($x, $stringArray))
						{
							$index = array_search($x, $stringArray);
							array_push($numberArray, $numberArray[$index]);
							array_push($stringArray, $x);
							array_push($assignedNumbers, $numberArray[$index]);
							
						}else 
						{
							array_push($stringArray, $x);
							$number = checkRandom($numberArray);
							array_push($numberArray, $number);
							array_push($assignedNumbers, $number);
						}
					}
					$assignedNumber1 = strval($assignedNumbers[0]);
					$assignedNumber2 = strval($assignedNumbers[1]);
					$assignedNumbersString = "$assignedNumber1$assignedNumber2";
					$scaleObjects[$i] = $assignedNumbersString;
				} else
				{
					if(in_array($scaleObjects[$i], $stringArray))
					{
						$index = array_search($scaleObjects[$i], $stringArray);
						array_push($numberArray, $numberArray[$index]);
						array_push($stringArray, $scaleObjects[$i]);
						$number = $numberArray[$index];
					} else 
					{
						array_push($stringArray, $scaleObjects[$i]);
						$number = checkRandom($numberArray);
						array_push($numberArray, $number);
					}
					$assignedNumber = strval($number);
					$scaleObjects[$i] = $assignedNumber;
				}		
			}
			$counter++;
		}
		return $scaleObjects;
	}
	/**
	 * This function returns a random number
	 *
	 * This function returns a random number (between 1 and 7) that does not appear
	 * in the numberArray.
	 *
	 * @var numberArray
	 */	
	function checkRandom($numberArray)
	{
		$number = mt_rand(1,7);
		while(in_array($number, $numberArray)) 
		{
			$number = mt_rand(1,7);
		}
		return $number;
	}
?>