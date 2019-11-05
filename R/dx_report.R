#' Render diagnostic report
#' @param dx_obj An object of class dx
#' @param output_dir Directory to output report into
#' @param output_file Filename of generated report
#' @param roc_options Arguments to be passed to dx_rox
#' @param forest_options Arguments to be passed to dx_forest
#' @export
#' @examples
#' \dontrun{
#' dx_obj <- dx(
#'   data = dx_heart_failure,
#'   study_name = "Heart Attack Prediction",
#'   data_description = "Validation Data",
#'   true_varname = "truth",
#'   pred_varname = "predicted",
#'   outcome_label = "Heart Attack",
#'   threshold_range = c(.1,.2,.3),
#'   setthreshold = .3,
#'   grouping_variables = c("AgeGroup", "Sex", "AgeSex")
#' )
#' dx_report(dx_obj, roc_options = list(curve_color = "blue"))
#' }
dx_report <- function(dx_obj, roc_options = list(), forest_options = list(),
                      output_dir = getwd(), output_file = "test.html") {
  rmarkdown::render(
    input = system.file("dx_report.Rmd", package = "diagnosticSummary"),
    output_dir = output_dir,
    output_file = output_file,
    params = list(dx_obj = dx_obj, roc_options = roc_options, forest_options = forest_options),
    envir = new.env(parent = globalenv())
  )
  utils::browseURL(paste0(output_dir, "/", output_file))
}
