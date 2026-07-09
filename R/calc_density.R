#' Calculate organism density per image using a laser-derived scale factor
#'
#' Counts every non-laser annotation label (based on label.lineage_names) per image and divides by the
#' image footprint area (from \code{\link{calc_laser_scale}}) to give
#' organism density in points per square metre.
#'
#' @param annotations A data frame of Squidle+ annotations, one row per
#'   annotation point.
#' @param scale_df Output of \code{\link{calc_laser_scale}} - a data frame
#'   with one row per image and an \code{image_area_m2} column.
#' @param laser_label_pattern Character. Regex (case-insensitive) used to
#'   exclude laser-dot annotations from the counts. Should match whatever
#'   was passed to \code{\link{calc_laser_scale}}.
#' @param image_id_col Character. Column identifying the image each point
#'   belongs to. Must match \code{image_id} in \code{scale_df}.
#' @param label_col Character. Column holding the annotation label to
#'   count and report density for.
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom stringr str_detect
#'
#' @return A data frame with one row per image x label combination:
#'   \code{image_id}, \code{label}, \code{n_points}, \code{image_area_m2},
#'   \code{density_per_m2}.
#'
#' @details If your workflow needs density from a different level (e.g.
#'   point-count annotations already summarised elsewhere, or biomass
#'   instead of raw counts), adapt the \code{count()} step accordingly -
#'   this function assumes one row per counted individual/point.
#'
#' @seealso \code{\link{calc_laser_scale}}
#'
#' @examples
#' \dontrun{
#' scale_df   <- calc_laser_scale(raw_all, known_distance_cm = 10)
#' density_df <- calc_density(raw_all, scale_df)
#' }
#'
#' @export
calc_density <- function(annotations,
                         scale_df,
                         laser_label_pattern = "laser",
                         image_id_col = "point.media.key",
                         label_col    = "label.lineage_names") {

  counts <- annotations %>%
    dplyr::filter(!stringr::str_detect(.data[[label_col]],
                                       stringr::regex(laser_label_pattern, ignore_case = TRUE))) %>%
    dplyr::transmute(image_id = .data[[image_id_col]], label = .data[[label_col]]) %>%
    dplyr::count(image_id, label, name = "n_points")

  counts %>%
    dplyr::left_join(scale_df %>% dplyr::select(image_id, image_area_m2), by = "image_id") %>%
    dplyr::mutate(density_per_m2 = n_points / image_area_m2)
}
