#' Create a new Dosepoint Custom object
#'
#' @inheritParams DosepointCustom
#'
#' @return A new DosepointCustom object
#'
#' @noRd
#' @keywords internal NLME
new_DosepointCustom <- function(
    DosepointName = "",
    DoseType = c("dosepoint", "dosepoint2"),
    dobefore = c(),
    doafter = c(),
    rate = c(),
    duration = c(),
    idosevar = c(),
    infdosevar = c(),
    infratevar = c(),
    bioavail = c(),
    tlag = c(),
    Statement = "",
    PMLStructure = character()) {
  stopifnot(length(DosepointName) == 1)
  stopifnot(length(Statement) == 1)

  match.arg(DoseType)

  if (length(rate) > 0 &&
      length(duration) > 0) {
    stop("Cannot specify both duration and rate for ", DosepointName)
  }

  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for DosepointCustom ",
         DosepointName)
  }

  stopifnot(is.character(DosepointName) &
              .check_0nzchar(DosepointName))

  stopifnot(is.character(Statement) &
              .check_0nzchar(Statement))

  stopifnot(is.character(PMLStructure))

  structure(
    list(
      DosepointName = DosepointName,
      DoseType = DoseType,
      dobefore = dobefore,
      doafter = doafter,
      rate = rate,
      duration = duration,
      idosevar = idosevar,
      infdosevar = infdosevar,
      infratevar = infratevar,
      bioavail = bioavail,
      tlag = tlag,
      Statement = Statement,
      PMLStructure = PMLStructure
    ),
    class = "DosepointCustom"
  )
}

DosepointCustom <- function(DosepointName = "",
                            DoseType = "dosepoint",
                            dobefore = c(),
                            doafter = c(),
                            rate = c(),
                            duration = c(),
                            idosevar = c(),
                            infdosevar = c(),
                            infratevar = c(),
                            bioavail = c(),
                            tlag = c(),
                            Statement = "",
                            PMLStructure = character()) {
  new_DosepointCustom(
    DosepointName = DosepointName,
    DoseType = DoseType,
    dobefore = dobefore,
    doafter = doafter,
    rate = rate,
    duration = duration,
    idosevar = idosevar,
    infdosevar = infdosevar,
    infratevar = infratevar,
    bioavail = bioavail,
    tlag = tlag,
    Statement = Statement,
    PMLStructure = PMLStructure
  )

}

#' @export
output.DosepointCustom <- function(x, ...) {
  paste0(x$DoseType, x$Statement)
}

#' @export
print.DosepointCustom <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
