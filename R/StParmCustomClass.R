#' Create a new StParm Custom object
#'
#' @inheritParams StParmCustom
#'
#' @return A new StParmCustom object
#'
#' @noRd
#' @keywords internal NLME
new_StParmCustom <- function(StParmName = "",
                             Statement = "",
                             PMLStructure = character()) {
  stopifnot(length(StParmName) == 1)
  stopifnot(length(Statement) == 1)

  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for StParmCustom ",
         StParmName)
  }

  stopifnot(is.character(StParmName) &
              .check_0nzchar(StParmName))

  stopifnot(is.character(Statement) &
              .check_0nzchar(Statement))

  stopifnot(is.character(PMLStructure))

  structure(
    list(
      StParmName = StParmName,
      Statement = Statement,
      PMLStructure = PMLStructure
    ),
    class = "StParmCustom"
  )
}

StParmCustom <- function(StParmName = "",
                         Statement = "",
                         PMLStructure = character()) {
  new_StParmCustom(
    StParmName = StParmName,
    Statement = Statement,
    PMLStructure = PMLStructure
  )

}

#' @export
output.StParmCustom <- function(x, ...) {
  paste0("stparm", x$Statement)
}

#' @export
print.StParmCustom <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
