###########IMAGE FILTER################

#' Greyscale filter
#'
#' This function takes a colour vector and returns a greyscale vector.
#' @param rgb the vector.
#' @return greyscale vector.
#' @export
greyscale_filter <- function(rgb) {

  #Calculate the greyscale value from the rgb-vector
  greyscale_value <- round(rgb[1] * 0.299 + rgb[2] * 0.587 + rgb[3] * 0.114)

  return(as.raw(c(greyscale_value, greyscale_value, greyscale_value)))
}


#' Two colour filter
#'
#' This function takes a colour vector and returns a new vector with the colour of choice
#' @param rgb The colour vector.
#' @param colour The colour vector of choice for the filter
#' @return A filtered colour vector with white colour and colour of choice
#' @export
two_colour_filter <- function(rgb, colour) {

  # Extract RGB values from the input vector
  R <- rgb[1]
  G <- rgb[2]
  B <- rgb[3]

  #Calculate the greyscale value from the rgb-vector
  greyscale_value <- round(R * 0.299 + G * 0.587 + B * 0.114)

  # Apply the cutoff for two-colour effect
  if (greyscale_value > 127) {
    # colour if greater than cutoff
    return(as.raw(colour))
  }  else {
    # colour if less than cutoff
    return(as.raw(c(255, 255, 255)))
  }
}


#'Apply greyscale filter
#'
#' This function takes the greyscale function and applies it to every pixel in the image.
#' @param image The bitmap image.
#' @return The filtered image.
#' @export
apply_greyscale_filter <- function(image) {

  # Copy of the image to store filtered values
  filtered_image <- image

  # Loop through each pixel in the image
  for (row in 1:dim(image)[3]) {

    for (col in 1:dim(image)[2]) {

      # Extract RGB values for the current pixel
      R <- as.numeric(image[1, col, row])
      G <- as.numeric(image[2, col, row])
      B <- as.numeric(image[3, col, row])

      # Apply the greyscale filter function to the RGB-vector
      greyscale_pixel <- greyscale_filter(c(R, G, B))

      # Update the filtered bitmap
      filtered_image[1, col, row] <- as.raw(greyscale_pixel[1])
      filtered_image[2, col, row] <- as.raw(greyscale_pixel[2])
      filtered_image[3, col, row] <- as.raw(greyscale_pixel[3])
    }
  }
  return(filtered_image)
}

#' Apply two colour filter
#'
#' This function takes the two colour filter and applies it to every pixel in the image.
#' @param image The bitmap image
#' @param colour The colour for the filter given as a vector, for example black c(0, 0, 0)
#' @return The filtered image
#' @export
apply_two_colour_filter <- function(image, colour) {

  # Copy of the image to store filtered values
  filtered_image <- image

  # Loop through each pixel in the image
  for (row in 1:dim(image)[3]) {

    for (col in 1:dim(image)[2]) {

      # Extract RGB values for the current pixel
      R <- as.numeric(image[1, col, row])
      G <- as.numeric(image[2, col, row])
      B <- as.numeric(image[3, col, row])

      # Apply the two colour filter function to the RGB-vector
      two_colour_pixel <- two_colour_filter(c(R, G, B), colour)

      # Update the filtered bitmap
      filtered_image[1, col, row] <- as.raw(two_colour_pixel[1])
      filtered_image[2, col, row] <- as.raw(two_colour_pixel[2])
      filtered_image[3, col, row] <- as.raw(two_colour_pixel[3])
    }
  }
  return(filtered_image)
}

#' Apply image filter
#'
#' This function applies the greyscale or two colour filter of choice.
#' @param image The image to be filtered.
#' @param fun The filter of choice, greyscale or colour.
#' @param colour Filter colour of choice as a vector. Default is red c(255, 0, 0)
#' @return The filtered image
#' @export
apply_image_filter <- function(image, fun, colour = c(255, 0, 0), ...) {

  # Apply greyscale filter
  if (fun == "greyscale") {
    return(apply_greyscale_filter(image))
  }

  # Apply two-colour filter
  else (fun == "colour")

  return(apply_two_colour_filter(image, colour, ...))
}
