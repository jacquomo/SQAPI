#' Get pixel dimensions of an image
#'
#' Reads a single image (local file or URL) and returns its pixel width
#' and height. Useful for converting Squidle+ normalised point coordinates
#' (0-1) to pixel coordinates when \code{point.media.width_px} /
#' \code{point.media.height_px} are not present in the annotation export
#' and only an image path (e.g. \code{point.media.path_best}) is available.
#'
#' @param image_path Character. Path or URL to a single image file.
#'
#' @return A named list with \code{pixel_width} and \code{pixel_height}
#'   (integers).
#'
#' @details Requires the \pkg{magick} package. For adding dimensions to an
#'   entire annotation data frame, use \code{\link{add_image_dimensions}}
#'   instead of calling this row-by-row - it caches by unique image path so
#'   repeated points on the same image only trigger one image read, and it
#'   won't abort the whole pipeline if one image fails to load.
#'
#' @examples
#' \dontrun{
#' get_image_dimensions("https://example.com/path/to/image.jpg")
#' }
#'
#' @export
get_image_dimensions <- function(image_path) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    stop("Package 'magick' is required for get_image_dimensions(). ",
         "Install it with install.packages('magick').")
  }
  img  <- magick::image_read(image_path)
  info <- magick::image_info(img)
  list(pixel_width = info$width, pixel_height = info$height)
}

#' Add pixel dimensions to a Squidle+ annotation data frame
#'
#' Convenience wrapper around \code{\link{get_image_dimensions}} that adds
#' \code{pixel_width} and \code{pixel_height} columns to a Squidle+
#' annotation data frame, based on a column of image paths/URLs (typically
#' \code{point.media.path_best}). Dimensions are looked up once per unique
#' image path rather than once per annotation point, and a failed image
#' read produces \code{NA} dimensions with a warning rather than stopping
#' the whole pipeline.
#'
#' @param annotations A data frame of Squidle+ annotations, one row per
#'   annotation point.
#' @param path_col Character. Name of the column holding image paths/URLs.
#'   Default \code{"point.media.path_best"}.
#' @param quiet Logical. If \code{FALSE} (default), prints progress while
#'   reading images and reports any that failed.
#'
#' @return \code{annotations} with two additional columns,
#'   \code{pixel_width} and \code{pixel_height}, joined back onto every
#'   row that shares the same \code{path_col} value.
#'
#' @seealso \code{\link{get_image_dimensions}}
#'
#' @examples
#' \dontrun{
#' raw_all <- add_image_dimensions(raw_all)
#' }
#'
#' @export
add_image_dimensions <- function(annotations,
                                 path_col = "point.media.path_best",
                                 quiet = FALSE) {

  paths <- unique(annotations[[path_col]])
  paths <- paths[!is.na(paths)]

  dims   <- vector("list", length(paths))
  names(dims) <- paths
  failed <- character(0)

  for (i in seq_along(paths)) {
    if (!quiet) cat(sprintf("\rReading image dimensions: %d / %d", i, length(paths)))
    dims[[i]] <- tryCatch(
      get_image_dimensions(paths[i]),
      error = function(e) {
        failed <<- c(failed, paths[i])
        list(pixel_width = NA_integer_, pixel_height = NA_integer_)
      }
    )
  }
  if (!quiet) cat("\n")

  if (length(failed) > 0) {
    warning(sprintf("Could not read %d image(s) - dimensions set to NA:\n%s",
                    length(failed), paste(failed, collapse = "\n")))
  }

  dims_df <- dplyr::bind_rows(dims, .id = path_col)

  dplyr::left_join(annotations, dims_df, by = path_col)
}
