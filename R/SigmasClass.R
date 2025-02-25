new_Sigmas <- function(Additive = 0,
                       LogAdditive = 0,
                       Proportional = 0.1,
                       AdditiveMultiplicative = list(PropPart = 0, AddPart = 0),
                       MixRatio = list(PropPart = 0, AddPart = 0),
                       Power = list(PowerPart = 0, StdevPart = 0),
                       ObservationName = "") {
  Additive <- .check_SigmaPart(Additive, "Additive")

  LogAdditive <- .check_SigmaPart(LogAdditive, "LogAdditive")

  Proportional <- .check_SigmaPart(Proportional, "Proportional")

  AdditiveMultiplicative$AddPart <-
    .check_SigmaPart(AdditiveMultiplicative$AddPart,
                     "Additive part of Additive+Multiplicative")
  AdditiveMultiplicative$PropPart <-
    .check_SigmaPart(AdditiveMultiplicative$PropPart,
                     "Proportional part of Additive+Multiplicative")

  if (!inherits(AdditiveMultiplicative$PropPart[[1]], "StParm")) {
    PropPart <-
      .check_SigmaPart(
        AdditiveMultiplicative$PropPart,
        "Proportional part of Additive+Multiplicative"
      )
    InputName <- .get_InputName(ObservationName)
    StParmName <- paste0(InputName, "MultStdev")
    AdditiveMultiplicative$PropPart <- NULL
    if (PropPart == 0) {
      State <- "None"
    } else {
      State <- "Present"
    }

    AdditiveMultiplicative$PropPart[[StParmName]] <-
      StParm(
        StParmName = StParmName,
        State = State,
        ThetaStParm = Theta(
          Name = paste0("tv", StParmName),
          InitialEstimates = unname(PropPart)
        ),
        OmegaStParm = Omega(Name = paste0("n", StParmName),
                            State = "None")
      )
  }

  MixRatio$AddPart <-
    .check_SigmaPart(MixRatio$AddPart, "Additive part of MixRatio")

  if (!inherits(MixRatio$PropPart[[1]], "StParm")) {
    PropPart <-
      .check_SigmaPart(MixRatio$PropPart, "Proportional part of MixRatio")
    InputName <- .get_InputName(ObservationName)
    StParmName <- paste0(InputName, "MixRatio")
    MixRatio$PropPart <- NULL
    if (PropPart == 0) {
      State <- "None"
    } else {
      State <- "Present"
    }

    MixRatio$PropPart[[StParmName]] <-
      StParm(
        StParmName = StParmName,
        State = State,
        ThetaStParm = Theta(
          Name = paste0("tv", StParmName),
          InitialEstimates = unname(PropPart)
        ),
        OmegaStParm = Omega(Name = paste0("n", StParmName),
                            State = "None")
      )
  }



  Power$PowerPart <-
    .check_SigmaPart(Power$PowerPart, "Power part of Power Sigma")
  Power$StdevPart <-
    .check_SigmaPart(Power$StdevPart, "Stdev part of Power Sigma")

  structure(
    list(
      Additive = Additive,
      LogAdditive = LogAdditive,
      Proportional = Proportional,
      AdditiveMultiplicative = AdditiveMultiplicative,
      MixRatio = MixRatio,
      Power = Power,
      ObservationName = ObservationName
    ),
    class = "Sigmas"
  )
}

.check_SigmaPart <- function(ErrorPart, ErrorName) {
  if (inherits(ErrorPart[[1]], "StParm")) {
    return(ErrorPart)
  }

  if (is.null(ErrorPart) ||
      ((length(ErrorPart) == 1 &&
        is.na(ErrorPart)))) {
    ErrorPart <- 0
  }

  if (is.list(ErrorPart)) {
    stop(
      "Please check ",
      ErrorName,
      " Error model. It should be StParm class instace enclosed in list."
    )
  }

  if (length(ErrorPart) != 1 ||
      !is.numeric(ErrorPart) ||
      ErrorPart < 0) {
    stop(
      "Please check ",
      ErrorName,
      " Error model. It should be non-negative numeric value, not ",
      paste(ErrorPart, collapse = ", ")
    )
  }

  ErrorPart
}

validate_Sigmas <- function(SigmasInstance) {
  MixRatioEstimate <-
    SigmasInstance$MixRatio$PropPart[[1]]$ThetaStParm$InitialEstimates$Estimate
  if (SigmasInstance$MixRatio$PropPart[[1]]$Type %in% c("LogNormal1", "LogNormal", "Normal") &&
      MixRatioEstimate < 0) {
    stop(
      "For MixRatio error model of observation ",
      SigmasInstance$ObservationName,
      " proportional part should be more than zero or zeroed (not used) ",
      "when the type of the associated structural parameter is not set",
      " to 'LogNormal2' nor 'LogitNormal'."
    )
  }

  if (SigmasInstance$MixRatio$AddPart < 0) {
    stop(
      "For MixRatio error model of observation ",
      SigmasInstance$ObservationName,
      " additive part should be more than zero or zeroed (not used)."
    )
  }

  if (sum(any(MixRatioEstimate == 0),
          SigmasInstance$MixRatio$AddPart == 0) == 1) {
    stop(
      "For MixRatio error model of observation ",
      SigmasInstance$ObservationName,
      " both parts (proportional and additive) should be more than zero or zeroed (not used).\n",
      "Right now ",
      ifelse(
        SigmasInstance$MixRatio$AddPart == 0,
        " additive ",
        " proportional "
      ),
      "part is zeroed."
    )
  }

  # AddMult part
  AddMultEstimate <-
    SigmasInstance$AdditiveMultiplicative$PropPart[[1]]$ThetaStParm$InitialEstimates$Estimate
  if (SigmasInstance$AdditiveMultiplicative$PropPart[[1]]$Type %in% c("LogNormal1", "LogNormal", "Normal") &&
      AddMultEstimate < 0) {
    stop(
      "For AdditiveMultiplicative error model of observation ",
      SigmasInstance$ObservationName,
      " proportional part should be more than zero or zeroed (not used) ",
      "when the type of the associated structural parameter is not set",
      " to 'LogNormal2' nor 'LogitNormal'."
    )
  }

  if (SigmasInstance$AdditiveMultiplicative$AddPart < 0) {
    stop(
      "For AdditiveMultiplicative error model of observation ",
      SigmasInstance$ObservationName,
      " additive part should be more than zero or zeroed (not used)."
    )
  }

  if (sum(any(AddMultEstimate == 0),
          SigmasInstance$AdditiveMultiplicative$AddPart == 0) == 1) {
    stop(
      "For AdditiveMultiplicative error model of observation ",
      SigmasInstance$ObservationName,
      " both parts (proportional and additive) should be more than zero or zeroed (not used)\n",
      "Right now ",
      ifelse(
        SigmasInstance$AdditiveMultiplicative$AddPart == 0,
        " additive ",
        " proportional "
      ),
      "part is zeroed."
    )
  }

  if (sum(any(SigmasInstance$Power$PowerPart <= 0),
          SigmasInstance$Power$StdevPart <= 0) == 1) {
    stop(
      "For Power error model of observation ",
      SigmasInstance$ObservationName,
      " both parts (power and Stdev) should be more than zero or zeroed (not used).\n",
      "Right now ",
      ifelse(
        SigmasInstance$Power$StdevPart == 0,
        " Stdev ",
        " power "
      ),
      "part is zeroed."
    )
  }

  SigmasInstance
}

#' Create an instance of Sigmas class.
#'
#' This function creates a new instance of different error models object to be
#' applied. 0s are treated as no values.
#' @param Additive The additive error sigma value.
#' @param LogAdditive The log-additive error sigma value.
#' @param Proportional The proportional error sigma value.
#' @param AdditiveMultiplicative A list specifying the additive and
#'   multiplicative parts for the additive-multiplicative error model. The list
#'   should have elements `PropPart` and `AddPart`. Alternatively the
#'   proportional part (`PropPart`) could be presented as `StParm`, see
#'   [StParm()].
#' @param MixRatio A list specifying the proportional and additive parts for the
#'   mix-ratio error model. The list should have elements `PropPart` and
#'   `AddPart`. Alternatively the proportional part (`PropPart`) could be
#'   presented as `StParm`, see [StParm()].
#' @param Power A numeric vector specifying the standard deviation and power
#'   parts for the power error model. The vector should have names `StdevPart`
#'   and `PowerPart`.
#' @inheritParams Observation
#'
#' @return A Sigmas class instance.
#'
#' @family Observations
#'
#' @examples
#' RSE_CObs <-
#'   Observation(SigmasChosen =
#'     Sigmas(MixRatio = list(PropPart = 2,
#'                            AddPart = 0.01),
#'            Proportional = 0))
#' models <-
#'   create_ModelPK(CompartmentsNumber = 2,
#'                  CObs = RSE_CObs)
#' print(models)
#'
#' @export
Sigmas <- function(Additive = 0,
                   LogAdditive = 0,
                   Proportional = 0.1,
                   AdditiveMultiplicative = list(PropPart = 0, AddPart = 0),
                   MixRatio = list(PropPart = 0, AddPart = 0),
                   Power = list(PowerPart = 0, StdevPart = 0),
                   ObservationName = "") {
  SigmasInstance <-
    new_Sigmas(
      Additive = Additive,
      LogAdditive = LogAdditive,
      Proportional = Proportional,
      AdditiveMultiplicative = AdditiveMultiplicative,
      MixRatio = MixRatio,
      Power = Power,
      ObservationName = ObservationName
    )

  validate_Sigmas(SigmasInstance)
}
