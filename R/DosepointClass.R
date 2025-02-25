.check_StparmString <- function(classInstance) {
  if (length(classInstance) > 0 &
      (!inherits(classInstance, "StParm") &
       !inherits(classInstance, "character"))) {
    message(paste(
      as.character(substitute(classInstance, env = environment())),
      "should be StParm or character class."
    ))
    FALSE
  } else {
    TRUE
  }
}

.check_Stparm <- function(classInstance) {
  if (length(classInstance) > 0 &
      (!inherits(classInstance, "StParm"))) {
    message(paste(as.character(
      substitute(classInstance, env = environment())
    ),
    "should be StParm class."))
    FALSE
  } else {
    TRUE
  }
}


#' Create a new Dosepoint object
#'
#' @inheritParams Dosepoint
#'
#' @return A new Dosepoint object
#'
#' @examples
#' TlagStParm <- StParm("Tlag",
#'        Type = "LogNormal",
#'        ThetaStParm = Theta(Name = "tvTlag", InitialEstimates = 0.1))
#'
#' A1 <- new_Dosepoint(DosepointName = "A1",
#'               State = "Present",
#'               tlag = TlagStParm,
#'               bioavail = StParm("F"),
#'               duration = StParm("D"))
#'
#' @noRd
#' @keywords internal NLME
new_Dosepoint <- function(DosepointName = character(),
                          State = c("Present", "None", "Searched"),
                          tlag = c(),
                          bioavail = c(),
                          duration = c(),
                          rate = c(),
                          PMLStructure = character()) {
  stopifnot(length(DosepointName) == 1)
  stopifnot(is.character(DosepointName) &
              .check_0nzchar(DosepointName))
  stopifnot(is.character(PMLStructure))

  if (!all(
    .check_Stparm(tlag),
    .check_Stparm(bioavail),
    .check_StparmString(duration),
    .check_StparmString(rate)
  )) {
    stop("Cannot create ", DosepointName, " class.")
  }

  State <- match.arg(State)

  structure(
    list(
      DosepointName = DosepointName,
      State = State,
      tlag = tlag,
      bioavail = bioavail,
      duration = duration,
      rate = rate,
      PMLStructure = PMLStructure
    ),
    class = "Dosepoint"
  )
}

#' Validate a Dosepoint object
#'
#' @param DosepointInstance A Dosepoint object
#'
#' @return The validated Dosepoint object
#'
#' @examples
#' DosepointInstance <-
#'   new_Dosepoint(DosepointName = "A1",
#'                 State = "Present",
#'                 tlag = 0.5,
#'                 bioavail = 0.8,
#'                 duration = 4,
#'                 )
#'
#' validate_Dosepoint(DosepointInstance)
#'
#' @noRd
#' @keywords internal NLME
validate_Dosepoint <- function(DosepointInstance) {
  if (length(DosepointInstance$duration) != 0 &
      length(DosepointInstance$rate) != 0) {
    stop(
      "Duration and Rate cannot be added simuilataneously for the dosepoint ",
      DosepointInstance$DosepointName
    )
  }

  DosepointInstance
}

#' Create a new Dosepoint object and validate it
#'
#' @param DosepointName A character string giving the name of the Dosepoint.
#' @param State A character string giving the state of the Dosepoint, must be
#'   one of "None", "Present", "Searched".
#' @param tlag An optional structural parameter giving the time lag for the
#'   doses coming into current Dosepoint.
#' @param bioavail An optional structural parameter giving the bioavailability
#'   of the doses coming into current Dosepoint.
#' @param duration An optional structural parameter giving the duration of
#'   infusion for the doses coming into current Dosepoint.
#' @param rate An optional structural parameter giving the rate of infusion for
#'   the doses coming into current Dosepoint.
#' @param State A character string representing the state of the Dosepoint.
#'   Possible values are:
#'   * `None`: current Dosepoint is not used.
#'   * `Present` (the default): current Dosepoint is used as is.
#'   * `Searched`: current Dosepoint is added as a token to be searched.
#' @param PMLStructure A character string that indicates bounded PML structure.
#'
#' @return A new Dosepoint object
#'
#' @family Dosepoints
#' @seealso [list_Dosepoints()]
#'
#' @examples
#' TlagStParm <- StParm("Tlag",
#'                      Type = "LogNormal",
#'                      ThetaStParm = Theta(Name = "tvTlag",
#'                                          InitialEstimates = 0.1))
#'
#' A1 <- Dosepoint(DosepointName = "A1",
#'                 State = "Present",
#'                 tlag = TlagStParm,
#'                 bioavail = StParm("F"))
#'
#' @export
Dosepoint <- function(DosepointName = "A1",
                      State = "Present",
                      tlag = c(),
                      bioavail = c(),
                      duration = c(),
                      rate = c(),
                      PMLStructure = character()) {
  DosepointInstance <- new_Dosepoint(
    DosepointName = DosepointName,
    State = State,
    tlag = tlag,
    bioavail = bioavail,
    duration = duration,
    rate = rate,
    PMLStructure = PMLStructure
  )

  if (.check_0nzchar(DosepointInstance$PMLStructure)) {
    DosepointInstance <-
      .assign_TextToVariable(ParmList = DosepointInstance,
                             "PMLStructure",
                             DosepointInstance$PMLStructure)
  }

  validate_Dosepoint(DosepointInstance)
}

.get_ParameterValue <- function(x, ParameterName) {
  if (length(x[[ParameterName]]) > 0) {
    if (inherits(x[[ParameterName]], "StParm")) {
      if (x[[ParameterName]]$State == "None") {
        Value <- ""
      } else {
        Value <-
          paste(", ", ParameterName, "=", x[[ParameterName]]$StParmName)
        if (x[[ParameterName]]$State == "Searched") {
          Value <- add_TokensNLME(
            TokenName = paste("", #x$PMLStructure,
                              x[[ParameterName]]$StParmName,
                              sep = "_"),
            ListElementName = "StParm",
            TokenValues = c("", Value),
            DoNotChangeTokenListMain = FALSE
          )
        }

      }
    } else {
      # character
      Value <- paste(", ", ParameterName, "=", x[[ParameterName]])
    }
  } else {
    Value <- ""
  }

  Value
}

#' Generate output for Dosepoint object
#'
#' Generates the output for a Dosepoint object, including the dosepoint statement for the NONMEM model.
#'
#' @param x A Dosepoint object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A character string representing the output for the Dosepoint object.
#'
#' @examples
#' A1 <- Dosepoint(DosepointName = "A1", State = "Present", tlag = "Tlag", bioavail = StParm("F"))
#' output(dose)
#'
#' @noRd
#' @keywords internal NLME
output.Dosepoint <- function(x, ...) {
  if (x$State == "None") {
    return("")
  }

  x <- validate_Dosepoint(x)
  if (.check_0nzchar(x$PMLStructure)) {
    x <-
      .assign_TextToVariable(ParmList = x, "PMLStructure", x$PMLStructure)
  }

  tlagValue <- .get_ParameterValue(x, "tlag")
  bioavailValue <- .get_ParameterValue(x, "bioavail")
  durationValue <- .get_ParameterValue(x, "duration")
  rateValue <- .get_ParameterValue(x, "rate")

  DosepointValue <-
    paste0(
      "dosepoint(",
      x$DosepointName,
      tlagValue,
      bioavailValue,
      durationValue,
      rateValue,
      paste0(", idosevar = ", x$DosepointName, "Dose"),
      paste0(", infdosevar = ", x$DosepointName, "InfDose"),
      paste0(", infratevar = ", x$DosepointName, "InfRate)")
    )

  for (DosepointArg in c("tlag", "bioavail", "duration", "rate")) {
    if (!is.null(x[[DosepointArg]]) &
        inherits(x[[DosepointArg]], "StParm")) {
      DosepointValue <- paste(DosepointValue,
                              output(x[[DosepointArg]]),
                              sep = "\n\t")
    }
  }

  if (x$State == "Searched") {
    DosepointValue <- add_TokensNLME(
      TokenName = paste(x$PMLStructure,
                        x$DosepointName,
                        sep = "_"),
      ListElementName = "Dosepoint",
      TokenValues = c("", DosepointValue),
      DoNotChangeTokenListMain = FALSE
    )
  }

  DosepointValue
}

#' @export
print.Dosepoint <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
