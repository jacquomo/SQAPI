#' Get pixel dimensions of an image
#'
#' Reads a single image (local file or URL) and returns its pixel width
#' and height. Useful for converting Squidle+ normalised point coordinates
#' to pixel coordinates when image dimensions are not present in the
#' annotation export.
#'
#' @param image_path Character. Path or URL to a single image file.
#'
#' @return A named list with \code{pixel_width} and \code{pixel_height}
#'   (integers).
#'
#' @details Requires the \pkg{magick} package.
#'
#' @examples
#' \dontrun{
#' get_image_dimensions("https://example.com/image.jpg")
#' }
#'
#' @export
get_image_dimensions <- function(image_path) {

  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required.")
  }

  img <- magick::image_read(image_path)
  info <- magick::image_info(img)

  list(
    pixel_width = as.integer(info$width),
    pixel_height = as.integer(info$height)
  )
}
