<?php
	// Second year Project 2014
	// Gerben van der Huizen - 10460748
	// Vincent Erich - 10384081
	// Michiel Boswijk - 10332553
	// Michael Chen - 10411941

	/**
	 * Creates a canvas for the visualised game item
	 *
	 * createCanvas function creates a canvas according to the arguments width and height.
	 * The visualised game item will be placed inside this canvas.
	 * The width and height of the canvas are predefined and changeable. The function also
	 * fills the created canvas with a transparent background.
	 *
	 * @var width
	 * @var height
	 */
	function createCanvas($width, $height)
	{
		$canvas = imagecreatetruecolor($width, $height);
		$backgroundColor = imagecolorallocatealpha($canvas, 255, 255, 255, 127);
		imagesavealpha($canvas, true);
		imagefill($canvas, 0, 0, $backgroundColor);
		return $canvas;
	}

	/**
	 * Determines the positions per scale
	 *
	 * The scalePositions function has two arguments: the canvas image 
	 * created by the createCanvas function and an array with images of the scales 
	 * created by the function createScale (see other file). According to the
	 * number of images inside the array, different position templates will be presented.
	 *
	 * @var canvas
	 * @var arrayScales
	 */
	function scalePositions($canvas, $arrayScales)
	{
		$widthCanvas = imagesx($canvas);
		$heightCanvas = imagesy($canvas);
		$widthScale = imagesx($arrayScales[0]);
		$heightScale = imagesy($arrayScales[0]);
		$numberOfScales = count($arrayScales); 
		$ratio1 = $widthCanvas / $widthScale; 
		$ratio2 = ($widthCanvas / 2) / $widthScale; 
		$ratio3 = ($widthCanvas / 3) / $widthScale; 
		switch ($numberOfScales)
		{
			case 1:
				if($ratio1 * $heightScale <= $heightCanvas)
				{
					$goodRatio = $ratio1; 
				} else
				{
					$goodRatio = $heightCanvas / $heightScale;
				}
				$resizeX = $goodRatio * $widthScale;
				$resizeY = $goodRatio * $heightScale;
				imageCopyresized($canvas, $arrayScales[0], ($widthCanvas - $resizeX) / 2, 0, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				break;
			case 2:
				if($ratio2 * $heightScale <= $heightCanvas)
				{
					$goodRatio = $ratio2; 
				} else
				{
					$goodRatio = $heightCanvas / $heightScale;
				}
				$resizeX = $goodRatio * $widthScale;
				$resizeY = $goodRatio * $heightScale;
				imageCopyresized($canvas, $arrayScales[0], 0 , ($heightCanvas - $resizeY) / 2, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[1], ($widthCanvas - $resizeX), (imagesy($canvas) - $resizeY) / 2, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				break;
			case 3:
				if($ratio2 * $heightScale <= ($heightCanvas / 2))
				{
					$goodRatio = $ratio2; 
				} else
				{
					$goodRatio = ($heightCanvas/2) / $heightScale;
				}
				$resizeX = $goodRatio * $widthScale;
				$resizeY = $goodRatio * $heightScale;
				imageCopyresized($canvas, $arrayScales[0], ($widthCanvas - $resizeX) / 2, 0 , 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[1], 0, $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[2], ($widthCanvas - $resizeX), $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				break;
			case 4:
				if($ratio2 * $heightScale <= ($heightCanvas / 2))
				{
					$goodRatio = $ratio2; 
				} else
				{
					$goodRatio = ($heightCanvas/2) / $heightScale;
				}
				$resizeX = $goodRatio * $widthScale;
				$resizeY = $goodRatio * $heightScale;
				imageCopyresized($canvas, $arrayScales[0], 0, 0, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[1], $widthCanvas - $resizeX, 0, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[2], $widthCanvas - $resizeX, $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[3], 0, $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				break;
			case 5:
				if($ratio3 * $heightScale <= ($heightCanvas / 2))
				{
					$goodRatio = $ratio3; 
				} else
				{
					$goodRatio = ($heightCanvas/2) / $heightScale;
				}
				$resizeX = $goodRatio * $widthScale;
				$resizeY = $goodRatio * $heightScale;
				imageCopyresized($canvas, $arrayScales[0], ($widthCanvas - ($resizeX * 2)) / 2, 0, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[1], $widthCanvas / 2, 0, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[2], ($widthCanvas - ($resizeX * 3)) / 3, $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[3], $widthCanvas - ((($widthCanvas - ($resizeX * 3)) / 3 + $resizeX) * 2) + ($widthCanvas - ($resizeX * 3)) / 6, $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				imageCopyresized($canvas, $arrayScales[4], $widthCanvas - (($widthCanvas - ($resizeX * 3)) / 3) - $resizeX, $resizeY, 0, 0, $resizeX, $resizeY, $widthScale, $heightScale);
				break;
			default:
				echo "Number of scales is unknown!";
		}
	}
?>