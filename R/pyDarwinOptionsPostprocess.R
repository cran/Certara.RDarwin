#' Create pyDarwin Postprocess Options
#'
#' Generates a list of postprocessing options to be used in pyDarwin
#' optimization process.
#'
#' @param use_r Logical: Whether to use R for postprocessing. If set to TRUE, R
#'   will be used to execute the post-processing script specified in
#'   `post_run_r_code`. Default: FALSE.
#' @param post_run_r_code Character: The file path to the R script that contains
#'   post-processing code. This script will be executed after the pyDarwin
#'   optimization process finishes.
#'   For NSGA-III (MOGA with 3 objectives), the R script must return a list
#'   containing two vectors: the first for objectives, the second for constraints
#'   (empty vector if no constraints). For other cases, it should return a
#'   vector containing a penalty value and a text string.
#'   Default: "\{project_dir\}/simplefunc.R".
#' @param r_timeout Numeric: The time limit (in seconds) for the execution of
#'   the post-processing R script. If the R script takes longer to execute than
#'   this timeout value, it will be terminated. Default: 30.
#' @param use_python Logical: Whether to use Python for postprocessing. If set
#'   to TRUE, Python will be used to execute the post-processing script
#'   specified in `post_run_python_code`. Default: FALSE.
#' @param post_run_python_code Character: The file path to the Python script
#'   that contains post-processing code.
#'   The script must contain a function `post_process(run_dir_path)` or
#'   `post_process2(model_run_object)`.
#'   For NSGA-III (MOGA with 3 objectives), this function must return a tuple of
#'   two lists: the first for objectives, the second for constraints (empty list
#'   if no constraints). For other cases, it should return a tuple containing a
#'   penalty value and a text string.
#'   Default: "\{project_dir\}/simplefunc.py".
#'
#' @return A list of postprocessing options in pyDarwin optimization process.
#'
#' @examples
#' # Create postprocess options with default values
#' postprocess_options <- pyDarwinOptionsPostprocess()
#'
#' # Create postprocess options with custom values
#' postprocess_options_custom <-
#'   pyDarwinOptionsPostprocess(use_r = TRUE,
#'                              post_run_r_code = "{project_dir}/postprocess.R",
#'                              r_timeout = 60,
#'                              use_python = TRUE,
#'                              post_run_python_code = "{project_dir}/postprocess.py")
#'
#' @export
pyDarwinOptionsPostprocess <- function(use_r = FALSE,
                                       post_run_r_code = "{project_dir}/simplefunc.R",
                                       r_timeout = 30,
                                       use_python = FALSE,
                                       post_run_python_code = "{project_dir}/simplefunc.py") {
  stopifnot(is.logical(use_r))
  stopifnot(is.character(post_run_r_code))
  stopifnot(is.numeric(r_timeout))
  stopifnot(is.logical(use_python))
  stopifnot(is.character(post_run_python_code))

  if (!use_r) {
    # post_run_r_code <- NULL # These are nulled if use_r is FALSE
    # r_timeout <- NULL
  }

  if (!use_python) {
    # post_run_python_code <- NULL # Nulled if use_python is FALSE
  }

  temp_list <- list(
    use_r = if (use_r) TRUE else NULL,
    post_run_r_code = if (use_r) post_run_r_code else NULL,
    r_timeout = if (use_r) r_timeout else NULL,
    use_python = if (use_python) TRUE else NULL,
    post_run_python_code = if (use_python) post_run_python_code else NULL
  )

  ReturnedList <- temp_list[!sapply(temp_list, is.null)]
  if (length(ReturnedList) == 0) {
    return(list())
  }

  ReturnedList
}
