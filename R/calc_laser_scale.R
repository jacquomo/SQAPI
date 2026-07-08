#' Calculate a pixel-to-real-world scale factor from paired laser points
#'
#' Uses two laser-dot annotation points per image (a fixed, known distance
#' apart on the camera rig) to derive a scale factor for that image, and
#' from it the real-world footprint area of the image. This is the
#' standard laser-scale method for estimating organism density and image
#' area from stereo-BRUV / AUV / ROV imagery where absolute scale isn't
#' otherwise known.
#'
#' @param annotations A data frame of Squidle+ annotations, one row per
#'   annotation point, including the two laser-dot points per image.
#' @param laser_label_pattern Character. Regex matched against
#'   \code{label_col} (case-insensitive) to identify laser-dot
#'   annotations, e.g. \code{"laser"} will match \code{"LASER"},
#'   \code{"Laser (10cm)"}, \code{"laser1"} / \code{"laser2"}, etc. If your
#'   two laser dots use genuinely different label names this still works
#'   as long as both match the pattern and nothing else does.
#' @param known_distance_cm Numeric. The fixed real-world separation
#'   between the two laser dots on your rig (measured once per rig setup),
#'   in centimetres.
#' @param image_id_col Character. Column identifying the image each point
#'   belongs to, e.g. \code{"point.media.key"} or \code{"point.media_id"}.
#' @param x_col,y_col Character. Columns holding point x/y coordinates.
#' @param width_col,height_col Character. Columns holding the image's
#'   pixel width/height (e.g. from \code{\link{add_image_dimensions}}, or
#'   from Squidle+'s own \code{point.media.width_px} /
#'   \code{point.media.height_px} if present in your export).
#' @param label_col Character. Column holding the annotation label used to
#'   identify laser points, e.g. \code{"label.name"} or
#'   \code{"label.lineage_names"}.
#' @param coords_normalised Logical. \code{TRUE} (default) if \code{x_col}
#'   / \code{y_col} are normalised 0-1 image coordinates (standard
#'   Squidle+ export) and need multiplying by \code{width_col} /
#'   \code{height_col} to get pixel coordinates. Set \code{FALSE} if your
#'   export already has pixel coordinates.
#'
#' @return A data frame with one row per image (only images with exactly
#'   2 laser points are included) and columns: \code{pixel_distance},
#'   \code{width_px}, \code{height_px}, \code{cm_per_pixel},
#'   \code{pixels_per_cm}, \code{image_width_cm}, \code{image_height_cm},
#'   \code{image_area_m2}.
#'
#' @details Images with anything other than exactly 2 matching laser
#'   points are excluded and reported in a warning, since automatically
#'   guessing which 2 of 3+ points form the true pair (or scaling from a
#'   single point) would silently corrupt the scale factor.
#'
#' @seealso \code{\link{calc_density}} for using the resulting scale
#'   factor to estimate organism density.
#'
#' @examples
#' \dontrun{
#' scale_df <- calc_laser_scale(
#'   raw_all,
#'   known_distance_cm = 10,
#'   image_id_col = "point.media.key",
#'   width_col    = "pixel_width",
#'   height_col   = "pixel_height",
#'   label_col    = "label.lineage_names"
#' )
#' }
#'
#' @export
calc_laser_scale <- function(annotations,
                             laser_label_pattern = "laser",
                             known_distance_cm   = 10,
                             image_id_col  = "point.media.key",
                             x_col         = "point.x",
                             y_col         = "point.y",
                             width_col     = "pixel_width",
                             height_col    = "pixel_height",
                             label_col     = "label.name",
                             coords_normalised = TRUE) {

  laser_pts <- annotations %>%
    dplyr::filter(stringr::str_detect(.data[[label_col]],
                                      stringr::regex(laser_label_pattern, ignore_case = TRUE))) %>%
    dplyr::transmute(
      image_id  = .data[[image_id_col]],
      x         = .data[[x_col]],
      y         = .data[[y_col]],
      width_px  = .data[[width_col]],
      height_px = .data[[height_col]]
    )

  if (nrow(laser_pts) == 0) {
    stop("No annotations matched laser_label_pattern = '", laser_label_pattern,
         "'. Check label_col / laser_label_pattern.")
  }

  if (coords_normalised) {
    laser_pts <- laser_pts %>%
      dplyr::mutate(x_px = x * width_px, y_px = y * height_px)
  } else {
    laser_pts <- laser_pts %>% dplyr::mutate(x_px = x, y_px = y)
  }

  # flag images that don't have exactly 2 laser points - can't safely pick
  # a pair automatically (missed a dot, mislabeled, or >2 dots present)
  n_per_image <- laser_pts %>% dplyr::count(image_id, name = "n_laser_pts")
  bad_images  <- n_per_image %>% dplyr::filter(n_laser_pts != 2)

  if (nrow(bad_images) > 0) {
    warning(sprintf(
      "%d image(s) have != 2 laser points and will be excluded from scale calculation:\n%s",
      nrow(bad_images),
      paste(utils::capture.output(print(bad_images, n = Inf)), collapse = "\n")
    ))
  }

  laser_pts %>%
    dplyr::semi_join(n_per_image %>% dplyr::filter(n_laser_pts == 2), by = "image_id") %>%
    dplyr::group_by(image_id) %>%
    dplyr::summarise(
      pixel_distance = sqrt((diff(x_px))^2 + (diff(y_px))^2),
      width_px  = dplyr::first(width_px),
      height_px = dplyr::first(height_px),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      cm_per_pixel    = known_distance_cm / pixel_distance,
      pixels_per_cm   = pixel_distance / known_distance_cm,
      image_width_cm  = width_px  * cm_per_pixel,
      image_height_cm = height_px * cm_per_pixel,
      image_area_m2   = (image_width_cm / 100) * (image_height_cm / 100)
    )
}


scale_df   <- calc_laser_scale(raw_all, known_distance_cm = 10)
