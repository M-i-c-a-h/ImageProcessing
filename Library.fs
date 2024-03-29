//
// F# image processing functions.
//
// Name: Micah Olugbamila
// Course: CS 341 
// Date: 03/23/2024
// U. of Illinois, Chicago
// CS 341, Spring 2024
//

namespace ImageLibrary

module Operations =
  
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  /// 
  /// truncateNum
  /// if argument value is greater than 255, 255 is returned ,if value is less than 0, 0 is returned,
  /// otherwise value is returned back
  let trucanteNum (num : float) = 
    if num < 0 then 0
    else if num > 255 then 255
    else int(num) 
 
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///
  /// sepia helper function
  /// ApplySepiaOnRow L : (int * int * int) list
  /// function takes in a list of tuples, manipulates each value of tuple and returns manipulated values as list of tuples
  let rec ApplySepiaOnRow (L : (int * int * int) list) =
    match L with
    | [] -> []
    | head :: tail -> 
        let (R,G,B) = head in

          let newR = trucanteNum ((0.393 * float(R)) + (0.769 * float(G)) + (0.189 * float(B)))
          let newG = trucanteNum ((0.349 * float(R)) + (0.686 * float(G)) + (0.168 * float(B)))
          let newB = trucanteNum ((0.272 * float(R)) + (0.534 * float(G)) + (0.131 * float(B)))

        let imageBuilder = (newR,newG,newB) in 
        imageBuilder :: ApplySepiaOnRow tail


  //
  // Sepia:
  //
  // Applies a sepia filter onto the image and returns the 
  // resulting image as a list of lists. 
  // The sepia filter adjusts the RGB values of each pixel
  // according to the following formulas:
  //    newRed = 0.393*origRed + 0.769*origGreen + 0.189*origBlue
  //    newGreen = 0.349*origRed + 0.686*origGreen + 0.168*origBlue
  //    newBlue = 0.272*origRed + 0.534*origGreen + 0.131*origBlue
  // We will use truncation to cast from the floating point result 
  // to the integer value.
  // 
  // If any of these values exceed 255, then 255 should be used
  // instead for that value.
  //
  // Returns: updated image.
  //
  let rec Sepia (width:int) 
                    (height:int) 
                    (depth:int) 
                    (image:(int*int*int) list list) = 
    match image with
    | [] -> []
    | head :: tail ->
        let newImage = ApplySepiaOnRow head in
        newImage  :: Sepia width height depth tail
        



///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///
  /// Increase Intesity helper function
  /// intesify_xxx (L : (int * int * int) list) (value : float)
  /// function takes in a list of tuples, manipulates XXX pixel values of each tuple 
  /// and returns manipulated values as list of tuples
  let rec intensifyRed (L : (int * int * int) list) (value : float) =
    match L with
    | [] -> []
    | head :: tail ->
        let (R,G,B) = head in 
          let newR = trucanteNum (float(R) * value) 
      
        let imageBuilder = (newR,G,B) in
        imageBuilder :: intensifyRed tail value
      
  let rec intensifyGreen (L : (int * int * int) list) (value : float) =
    match L with
    | [] -> []
    | head :: tail ->
        let (R,G,B) = head in
          let newG = trucanteNum (float(G) * value)

        let imageBuilder = (R,newG,B) in
        imageBuilder :: intensifyGreen tail value


  let rec intensifyBlue (L : (int * int * int) list) (value : float) =
    match L with
    | [] -> []
    | head :: tail ->
        let (R,G,B) = head in
          let newB = trucanteNum (float(B) * value)
          
        let imageBuilder = (R,G,newB) in
        imageBuilder :: intensifyBlue tail value  

        
  //
  // Increase Intensity
  //
  // Increase the intensity of a particular RGB channel
  // according to the values of the parameters.
  // The intensity is the scaling factor by which the
  // channel selected s-hould be increased (or decreased 
  // if the value is less than 1).
  // The channel is one of 'r', 'g', or 'b' which 
  // correspond to red, green, and blue respectively.
  // If the channel is not one of those three values,
  // do not modify the image.
  // Remember that the maximum value for any pixel 
  // channel is 255, so be careful of overflow!
  //
  // Returns: updated image.
  //
  let rec IncreaseIntensity (width:int) 
                    (height:int)
                    (depth:int)
                    (image:(int*int*int) list list)
                    (intensity:double)
                    (channel:char) = 

    let factor = 
      if intensity < 0 then -1.0/intensity
      else intensity

    match image with
    | [] -> []
    | head :: tail ->
        let newImage = 
          match channel with
          | 'r' | 'R' -> intensifyRed head factor
          | 'g' | 'G' -> intensifyGreen head factor
          | 'b' | 'B' -> intensifyBlue head factor
          | _ -> head
        newImage :: IncreaseIntensity width height depth tail intensity channel


  //
  // FlipHorizontal:
  //
  // Flips an image so that what’s on the left is now on 
  // the right, and what’s on the right is now on the left. 
  // That is, the pixel that is on the far left end of the
  // row ends up on the far right of the row, and the pixel
  // on the far right ends up on the far left. This is 
  // repeated as you move inwards toward the row's center.
  //
  // Returns: updated image.
  //
  let rec FlipHorizontal (width:int)
                         (height:int)
                         (depth:int)
                         (image:(int*int*int) list list) = 
    match image with
    | [] -> []
    | head :: tail ->
        let reversedRow = List.rev head in 
        reversedRow :: FlipHorizontal width height depth tail
    

  //
  // Rotate180:
  //
  // Rotates the image 180 degrees.
  //
  // Returns: updated image.
  //
  let rec Rotate180 (width:int)
                        (height:int)
                        (depth:int)
                        (image:(int*int*int) list list) = 
    match image with
    | [] -> []
    | _ -> FlipHorizontal width height depth (List.rev image)


  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  /// 
  /// distanceBetweenPixels
  /// returns the distance bewteen two pixels (x1, y1, z1) and (x2, y2, z2)
  let distanceBetweenPixels (L : (int * int * int) ) (M : (int * int * int) ) =
    let (x1, y1, z1) = L
    let (x2, y2, z2) = M

    let distance = sqrt( float((pown (x1 - x2) 2) + (pown (y1 - y2) 2) + (pown (z1 - z2) 2)) )
    distance

  
  //////////////////////////////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////////////////////////////////////////////////////////////////////////////
  /// 
  /// EdgeDetectRow
  /// perform Edge detection on a row 
  let rec EdgeDetectRow ((bottomRow : (int * int * int) list))
                        ((currentRow : (int * int * int) list))
                        (width : int)
                        (currWidth : int)
                        (threshold : int) =

    if currWidth < width then
      match currentRow with
      | [] -> []
      | head :: tail ->
          let rightPixel = List.head tail
          let bottomPixel = bottomRow.[currWidth-1]

          let newRow =
            match (distanceBetweenPixels head rightPixel > threshold) || (distanceBetweenPixels head bottomPixel > threshold) with
            | true ->  (0,0,0)
            | false -> (255,255,255)
          newRow :: (EdgeDetectRow bottomRow tail width (currWidth+1) threshold)

    else []


  //
  // Edge Detection:
  //
  // Edge detection is an algorithm used in computer vision to help
  // distinguish different objects in a picture or to distinguish an
  // object in the foreground of the picture from the background.
  //
  // Edge Detection replaces each pixel in the original image with
  // a black pixel, (0, 0, 0), if the original pixel contains an 
  // "edge" in the original image.  If the original pixel does not
  // contain an edge, the pixel is replaced with a white pixel 
  // (255, 255, 255).
  //
  // An edge occurs when the color of pixel is "significantly different"
  // when compared to the color of two of its neighboring pixels. 
  // We only compare each pixel in the image with the 
  // pixel immediately to the right of it and with the pixel
  // immediately below it. If either pixel has a color difference
  // greater than a given threshold, then it is "significantly
  // different" and an edge occurs. Note that the right-most column
  // of pixels and the bottom-most column of pixels can not perform
  // this calculation so the final image contain one less column
  // and one less row than the original image.
  //
  // To calculate the "color difference" between two pixels, we
  // treat the each pixel as a point on a 3-dimensional grid and
  // we calculate the distance between the two points using the
  // 3-dimensional extension to the Pythagorean Theorem.
  // Distance between (x1, y1, z1) and (x2, y2, z2) is
  //  sqrt ( (x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2 )
  //
  // The threshold amount will need to be given, which is an 
  // integer 0 < threshold < 255.  If the color distance between
  // the original pixel either of the two neighboring pixels 
  // is greater than the threshold amount, an edge occurs and 
  // a black pixel is put in the resulting image at the location
  // of the original pixel. 
  //
  // Returns: updated image.
  //

  let rec EdgeDetect (width:int)
                     (height:int)
                     (depth:int)
                     (image:(int*int*int) list list)
                     (threshold:int) = 
    match image with
    | [] -> []
    | head :: tail ->
        if List.length tail > 0 then  
          let bottomRow = List.head tail
          let newImage = EdgeDetectRow bottomRow head width 1 threshold in
          newImage :: EdgeDetect width height depth tail threshold

        else []

  