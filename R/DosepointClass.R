.check_StparmExpression <- function(classInstance) {
  if (length(classInstance) > 0 &
      (!inherits(classInstance, "StParm") &
       !inherits(classInstance, "Expression"))) {
    message(paste(
      as.character(substitute(classInstance, env = environment())),
      "should be StParm or Expression class."
    ))
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

  if (inherits(tlag, "character")) {
    tlag <- Expression(tlag)
    message("tlag converted to expression for dosepoint ", DosepointName)
  }

  if (inherits(bioavail, "character")) {
    bioavail <- Expression(bioavail)
    message("bioavail converted to expression for dosepoint ", DosepointName)
  }

  if (inherits(duration, "character")) {
    duration <- Expression(duration)
    message("duration converted to expression for dosepoint ", DosepointName)
  }

  if (inherits(rate, "character")) {
    rate <- Expression(rate)
    message("rate converted to expression for dosepoint ", DosepointName)
  }

  if (!all(
    .check_StparmExpression(tlag),
    .check_StparmExpression(bioavail),
    .check_StparmExpression(duration),
    .check_StparmExpression(rate)
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
#'   one of "None", "Present", "Searched". Default is "Present".
#' @param tlag An optional parameter for the time lag. Can be an
#'   \code{\link{StParm}} object, an \code{\link{Expression}} object, or a
#'   character string (which will be converted to an Expression).
#' @param bioavail An optional parameter for bioavailability. Can be an
#'   \code{\link{StParm}} object, an \code{\link{Expression}} object, or a
#'   character string (which will be converted to an Expression).
#' @param duration An optional parameter for the duration of infusion. Can be an
#'   \code{\link{StParm}} object, an \code{\link{Expression}} object, or a
#'   character string (which will be converted to an Expression).
#' @param rate An optional parameter for the rate of infusion. Can be an
#'   \code{\link{StParm}} object, an \code{\link{Expression}} object, or a
#'   character string (which will be converted to an Expression).
#' @param PMLStructure A character string that indicates a specific PML structure
#'   this Dosepoint definition should be associated with.
#'
#' @return A new Dosepoint object.
#'
#' @family Dosepoints
#' @seealso [list_Dosepoints()], [add_Dosepoint()], [modify_Dosepoint()], [StParm()], [Expression()]
#'
#' @examples
#' # Using StParm objects
#' TlagStParm <- StParm("Tlag",
#'                      Type = "LogNormal",
#'                      ThetaStParm = Theta(Name = "tvTlag", InitialEstimates = 0.1))
#' FStParm <- StParm("F", ThetaStParm = Theta(Name = "tvF")) # Assuming Theta exists
#'
#' dp1 <- Dosepoint(DosepointName = "GutInput",
#'                  State = "Present",
#'                  tlag = TlagStParm,
#'                  bioavail = FStParm)
#'
#' # Using Expression objects
#' dp2 <- Dosepoint(DosepointName = "Infusion",
#'                  rate = Expression("RateVal",
#'                                    ContainedStParms =
#'                                      list(StParm("RateVal",
#'                                                  ThetaStParm = Theta("tvRateVal")))))
#'
#' # Using a character string (will be converted to Expression internally)
#' dp3 <- Dosepoint(DosepointName = "Bolus",
#'                  bioavail = "SystemicF") # Converted to Expression("SystemicF")
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
      if (x[[ParameterName]]$State == "None") {
        Value <- ""
      } else {
        if (inherits(x[[ParameterName]], "StParm")) {
          Value <-
            paste(",", ParameterName, "=", x[[ParameterName]]$StParmName)
        } else {
          # expression
          Value <-
            paste(",", ParameterName, "=", output(x[[ParameterName]]))
        }

        if (x[[ParameterName]]$State == "Searched") {
          ExprType <- ifelse(inherits(x[[ParameterName]], "StParm"), "StParm", "Expression")
          if (ExprType == "StParm") {
            Value <- add_TokensNLME(
              TokenName = paste0("_",
                                x[[ParameterName]]$StParmName),
              ListElementName = ExprType,
              TokenValues = c("", Value),
              DoNotChangeTokenListMain = FALSE
            )
          } else { # expression
            Value <- add_TokensNLME(
              TokenName = paste(x$DosepointName,
                                ParameterName,
                                ExprType,
                                sep = "_"),
              ListElementName = ExprType,
              TokenValues = c("", Value),
              DoNotChangeTokenListMain = FALSE
            )

          }
        }

      }
  } else {
    Value <- ""
  }

  Value
}

#' Generate output for Dosepoint object
#'
#' Generates the output for a Dosepoint object
#'
#' @param x A Dosepoint object.
#' @param ... Additional arguments (currently unused).
#'
#' @return A character string representing the output for the Dosepoint object.
#'
#' @examples
#' A1 <- Dosepoint(DosepointName = "A1", State = "Present", tlag = "Tlag", bioavail = StParm("F"))
#' output(A1)
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
    if (!is.null(x[[DosepointArg]])) {
      if (inherits(x[[DosepointArg]], "StParm")) {
        DosepointValue <- paste(DosepointValue,
                                output(x[[DosepointArg]]),
                                sep = "\n\t")

      } else if (inherits(x[[DosepointArg]], "Expression")) {
        for (StParmInstance in x[[DosepointArg]]$ContainedStParms) {
          Value <- output(StParmInstance)
          if (x[[DosepointArg]]$State == "Searched") {
            # we allow the stparm in the expression only in the state 'Present'
            # but the expression could be searched
            Value <- add_TokensNLME(
              TokenName = paste(x$DosepointName,
                                DosepointArg,
                                "Expression",
                                sep = "_"),
              ListElementName = "Expression",
              TokenValues = c("", Value),
              DoNotChangeTokenListMain = FALSE
            )
          }

          DosepointValue <- paste(DosepointValue,
                                  Value,
                                  sep = "\n\t")
        }
      }
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
