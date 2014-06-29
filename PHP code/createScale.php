<?php
	// Second year Project Artificial Intelligence 2014
	// Gerben van der Huizen - 10460748
	// Vincent Erich - 10384081
	// Michiel Boswijk - 10332553
	// Michael Chen - 10411941

	/**
	 * Create an image of a scale from different components (images).
	 *
	 * The function 'createScale' first sets a temporary height and width for the 
	 * scale image (imDestFinal).
	 * Once this image is created, the image of the balance is placed in the 
	 * centre of the imDestFinal image.
	 * Next, the horizontal coordinates and the height for the images of the
	 * objects are determined (which are to be placed on the balance).
	 * With these coordinates, the images of the objects can be placed on the
	 * right positions with the function 'plotObjects'. The function 
	 * 'rotateObjects' rotates the objects and the balance (unless the scale
	 * is balanced). The image of the stand, on which the balance is placed,
	 * is also positioned by the function 'rotateObjects'.
	 * Now that we have the final image of the scale as a variable, it is 
	 * displayed in the browser so it is visible.
	 * 
	 * @var objectArray
	 */
	function createScale($objectArray)
	{
		$width = isset($_GET['width']) ? $_GET['width'] + 0 : 800;
		$height = isset($_GET['height']) ? $_GET['height'] + 0 : 500;
			
		$imDestFinal = imagecreatetruecolor($width, $height);
		$backgroundColor = imagecolorallocatealpha($imDestFinal, 255, 255, 255, 127);
		imagesavealpha($imDestFinal, true);
		imagefill($imDestFinal, 0, 0, $backgroundColor);

		$pieces = $objectArray;
		$stringBalance = $pieces[2];

		$scaleImg = 'Scale.png';
		$scale = imagecreatefrompng($scaleImg);	

		$heightScale = imagesy($scale);
		$widthScale = imagesx($scale);

		imagecopyresampled($imDestFinal, $scale, 0, 200, 0, 0, 800, imagesy($scale), imagesx($scale), imagesy($scale));
		imagedestroy($scale);

		$objectHeight = ($height / 2) - ($height / 2.9) ;

		$objectLeft1 = 10;
		$objectLeft2 = 140;

		$objectRight1 = $width - 270;
		$objectRight2 = $width - 140;

		$horPositionArray = array($objectLeft1, $objectLeft2, $objectRight1, $objectRight2);

		$newImg = plotObjects($pieces, $imDestFinal, $objectHeight, $horPositionArray);
		$rotation = rotateObjects($stringBalance, $newImg);

		return($rotation);
		imagedestroy($imDestFinal);
		imagedestroy($rotation);
	}
	
	/**
	 * Rotate the image of the balance according to the given arguments.
	 *
	 * The function 'rotateObjects' rotates the image 'imDestFinal' according
	 * to the argument $string. This argument determines how the scale should
	 * be tipped (in balance, tipped to the left, or tipped to the right). The
	 * function also positions the image of the stand (on which the balance is 
	 * placed). The function returns an image in which the the stand is placed 
	 * and with the balance and the objects rotated.
	 * 
	 * @var string
	 * @var imDestFinal
	 */
	function rotateObjects($string, $imDestFinal) 
	{
		$imageString = 'Stand.png';
		$stand = imagecreatefrompng($imageString);
		$transColor = imagecolorallocatealpha($imDestFinal, 0, 0, 0, 127);
		if(strcmp($string, 'left') == 0)
		{	
			$rotate = imagerotate($imDestFinal, 35, $transColor);
			imagecopyresized($rotate, $stand, 380, 425, 0, 0, 200, 230, 100, 87);
			$rotate = imagescale($rotate, 942, 868,  IMG_BICUBIC_FIXED);
		} elseif(strcmp($string, 'right') == 0)
		{
			$rotate = imagerotate($imDestFinal, -35, $transColor);
			imagecopyresized($rotate, $stand, 355, 420, 0, 0, 200, 230, 100, 87);
		} elseif(strcmp($string, 'equal') == 0)
		{
			imagecopyresized($imDestFinal, $stand, 300, 250, 0, 0, 200, 230, 100, 87);
			$temp = imagerotate($imDestFinal, 0, $transColor);
			$rotate = createCanvas(942, 868);
			imagecopyresized($rotate, $temp, (imagesx($rotate) - 800) / 2, (imagesy($rotate) - 500) / 2, 0, 0, 800, 500, 800, 500);
		} else 
		{
			echo("Wrong String!!!");
			$rotate = 0;
		}
		imagedestroy($stand);
		return $rotate;
	}

	/**
	 * Places the objects on the balance according to the number of objects on
	 * a scale.
	 *
	 * The function 'plotObjects' gets the objects from the array $pieces 
	 * (example the array $piece: ['12', '3', 'left']) and puts them in an array.
	 * First, the function checks how many objects are present on a scale.
	 * Based on this amount, it selects which method should be used to place 
	 * the objects in the image on the balance ($imDestFinal).
	 * The variable $objectHeight represents the vertical coordinates of the 
	 * objects (should be the same for each object). The variable 
	 * $horPositionArray represents the horizontal coordinates of the objects. 
	 * The function returns an image with the balance (in the same 
	 * position) and the objects placed on the correct positions.
	 * 
	 * @var pieces
	 * @var imDestFinal
	 * @var objectHeight
	 * @var horPositionArray
	 */
	function plotObjects($pieces, $imDestFinal, $objectHeight, $horPositionArray)
	{
		$left = $pieces[0];
		$arrayLeft = str_split($left);

		$right = $pieces[1];
		$arrayRight = str_split($right);
		$numberArray = array_merge($arrayLeft, $arrayRight);

		$numberObjects = count($numberArray);

		if($numberObjects == 2)
		{
			$image1 = chooseObject((int)$numberArray[0]);
			$image2 = chooseObject((int)$numberArray[1]);
			imagecopyresized($imDestFinal, $image1, $horPositionArray[0], $objectHeight, 0, 0, 125, 125, 302, 302);
			imagecopyresized($imDestFinal, $image2, $horPositionArray[3], $objectHeight, 0, 0, 125, 125, 302, 302);
			imagedestroy($image1);
			imagedestroy($image2);
		}
		if($numberObjects == 3)
		{
			$image1 = chooseObject((int)$numberArray[0]);
			$image2 = chooseObject((int)$numberArray[1]);
			$image3 = chooseObject((int)$numberArray[2]);
			imagecopyresized($imDestFinal, $image1, $horPositionArray[0], $objectHeight, 0, 0, 125, 125, 302, 302);
			if(count($arrayLeft) > count($arrayRight)) 
			{
				imagecopyresized($imDestFinal, $image2, $horPositionArray[1], $objectHeight, 0, 0, 125, 125, 302, 302);
				imagecopyresized($imDestFinal, $image3, $horPositionArray[3], $objectHeight, 0, 0, 125, 125, 302, 302);
			} else
			{
				imagecopyresized($imDestFinal, $image2, $horPositionArray[2], $objectHeight, 0, 0, 125, 125, 302, 302);
				imagecopyresized($imDestFinal, $image3, $horPositionArray[3], $objectHeight, 0, 0, 125, 125, 302, 302);
			}
			imagedestroy($image1);
			imagedestroy($image2);
			imagedestroy($image3);
		}
		if($numberObjects == 4)
		{
			$image1 = chooseObject((int)$numberArray[0]);
			$image2 = chooseObject((int)$numberArray[1]);
			$image3 = chooseObject((int)$numberArray[2]);
			$image4 = chooseObject((int)$numberArray[3]);
			$imageArray = array($image1, $image2, $image3, $image4);
			for ($j = 0; $j <= ($numberObjects - 1); $j++) 
			{
				imagecopyresized($imDestFinal, $imageArray[$j], $horPositionArray[$j], $objectHeight, 0, 0, 125, 125, 302, 302);
			}
			imagedestroy($image1);
			imagedestroy($image2);
			imagedestroy($image3);
			imagedestroy($image4);
		}
		return $imDestFinal;
	}
	
	/**
	 * Chooses the image that should be used to represent an object.
	 *
	 * The function 'chooseObject' chooses an image that should be used to 
	 * represent an object and returns a position for that image.
	 *
	 * @var number
	 */
	function chooseObject($number) 
	{
		switch ($number) 
		{
		case 1:
			$imageString = 'Weight_1.png';
			break;
		case 2:
			$imageString = 'Weight_2.png';
			break;
		case 3:
			$imageString = 'Weight_3.png';
			break;
		case 4:
			$imageString = 'Weight_4.png';
			break;
		case 5:
			$imageString = 'Weight_5.png';
			break;
		case 6:
			$imageString = 'Weight_6.png';
			break;
		case 7:
			$imageString = 'Weight_7.png';
			break;
		}
		$src_im = imagecreatefrompng($imageString);
		return $src_im;
	}
?>