#' Prepare Data
#'
#' Prepare data for analysis by adding water year, reference date,
#' and water year categories, and filtering by time window and
#' base year(s).
#'
#' @param df A dataset, i.e., output of
#'   [read_wat_binary()].
#' @param time_window A vector of time windows for use with
#'   [watplotter.core::extract_window()].
#' @param wy_category A vector of water year categories for use with
#'   [watplotter.core::extract_wycategory()]. Ignored if `NA` or `NULL`.
#' @param base_year A vector of base years for use with
#'   [watplotter.core::extract_baseyear()]. Ignored if `NA` or `NULL`.
#' @param icf_period A vector of ICF periods for use with
#'   [watplotter.core::extract_icf()]. Ignored if `NA` or `NULL`.
#' @return The filtered, standardized dataset.
#'
#' @importFrom dplyr select any_of
#' @importFrom watplotter.core standardize_dataset extract_window
#'   extract_wycategory extract_baseyear extract_icf
#' @export
prepare_data = function(df, time_window = NULL, wy_category = NULL,
  base_year = NULL, icf_period = NULL) {

  # assign water year categories (if provided)
  if (!(all(is.na(wy_category)) || is.null(wy_category))) {
    df = extract_wycategory(df, wy_category)
  }

  # filter by base years (if provided)
  if (!(all(is.na(base_year)) || is.null(base_year))) {
    df = extract_baseyear(df, base_year)
  }

  # extract by time window(s)
  if (!all(is.null(time_window) | is.na(time_window))) {
    df = extract_window(df, time_window)
  }

  # extract by ICF period(s)
  if (!all(is.null(icf_period) | is.na(icf_period))) {
    df = extract_icf(df, icf_period)
  }

  select(df, any_of(unname(fields())))
}


#' Augment Data
#'
#' Augment a dataset by adding water year and reference date,
#' and attaching supplemental information such as water year
#' categories, and base years.
#'
#' @param df A dataset, i.e., output of [read_wat_binary()].
#' @inheritParams watplotter.core::assign_wycategory
#' @inheritParams watplotter.core::assign_baseyear
#' @inheritParams watplotter.core::assign_icf
#' @return The standardized, augmented dataset.
#'
#' @importFrom watplotter.core standardize_dataset
#'   assign_wycategory assign_baseyear assign_icf
#' @export
augment_data = function(df, wy_categories = NULL, event_years = NULL, icf_periods = NULL) {

  # assign water year categories (if provided)
  if (!(all(is.na(wy_categories)) || is.null(wy_categories))) {
    df = assign_wycategory(df, wy_categories)
  }

  # assign base years (if provided)
  if (!(all(is.na(event_years)) || is.null(event_years))) {
    df = assign_baseyear(df, event_years)
  }

  # assign ICF periods (if provided)
  if (!(all(is.na(icf_periods)) || is.null(icf_periods))) {
    df = assign_icf(df, icf_periods)
  }

  df
}
