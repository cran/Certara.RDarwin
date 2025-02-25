#' Create a new Covariate Custom object
#'
#' @inheritParams CovariateCustom
#'
#' @return A new CovariateCustom object
#'
#' @noRd
#' @keywords internal NLME
new_CovariateCustom <- function(
    Name = "",
    Direction = c("Forward", "Backward", "Interpolate"),
    Type = c("Continuous", "Categorical"),
    Statement = "",
    PMLStructure = character()) {
  stopifnot(length(Name) == 1)
  stopifnot(length(Statement) == 1)
  match.arg(Type)
  match.arg(Direction)

  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for CovariateCustom ",
         Name)
  }

  stopifnot(is.character(Name) &
              .check_0nzchar(Name))

  stopifnot(is.character(Statement) &
              .check_0nzchar(Statement))

  stopifnot(is.character(PMLStructure))

  structure(
    list(
      Name = Name,
      Direction = Direction,
      Type = Type,
      Statement = Statement,
      PMLStructure = PMLStructure
    ),
    class = "CovariateCustom"
  )
}

CovariateCustom <- function(Name = "",
                            Direction = "Forward",
                            Type = "Continuous",
                            Statement = "",
                            PMLStructure = character()) {
  new_CovariateCustom(
    Name = Name,
    Direction = Direction,
    Type = Type,
    Statement = Statement,
    PMLStructure = PMLStructure
  )

}

#' @export
output.CovariateCustom <- function(x, ...) {
  paste0(x$Statement)
}

#' @export
print.CovariateCustom <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
