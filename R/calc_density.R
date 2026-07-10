#' Calculate organism density per image using a laser-derived scale factor
#'
#' Counts every non-laser annotation label (based on label.lineage_names)
#' per image and divides by the image footprint area to give organism
#' density in points per square metre. Density metrics are joined back onto
#' the original annotation dataframe, retaining all annotation attributes.
#'
#' @param annotations A data frame of Squidle+ annotations, one row per
#'   annotation point.
#' @param scale_df Output of \code{\link{calc_laser_scale}} with one row per
#'   image and a \code{point.media.key} and \code{image_area_m2} column.
#' @param laser_label_pattern Character. Regex (case-insensitive) used to
#'   exclude laser-dot annotations.
#' @param label_col Character. Column holding the annotation label.
#'
#' @return The original annotation dataframe with added columns:
#'   \code{image_area_m2}, \code{n_points}, and \code{density_per_m2}.
#'
#' @export
calc_density <- function(annotations,
                         scale_df,
                         laser_label_pattern = "laser",
                         label_col = "label.lineage_names") {

  density_df <- annotations %>%
    dplyr::filter(!stringr::str_detect(
      .data[[label_col]],
      stringr::regex(laser_label_pattern, ignore_case = TRUE)
    )) %>%
    dplyr::count(
      point.media.key,
      label = .data[[label_col]],
      name = "n_points"
    ) %>%
    dplyr::left_join(
      scale_df %>%
        dplyr::select(point.media.key, image_area_m2),
      by = "point.media.key"
    ) %>%
    dplyr::mutate(
      density_per_m2 = n_points / image_area_m2
    )

  annotations %>%
    dplyr::left_join(
      density_df,
      by = c(
        "point.media.key",
        setNames("label", label_col)
      )
    )
}
