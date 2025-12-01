#' Create a new Observation object
#'
#' @inheritParams Observation
#'
#' @return A new Observation object
#'
#' @examples
#' new_Observation(ObservationName = "CObs",
#'                 SigmasChosen = list(
#'                   Additive = 2,
#'                   LogAdditive = 0,
#'                   Proportional = 0.1,
#'                   AdditiveMultiplicative = c(PropPart = 0.1, AddPart = 1)
#'                 ),
#'                 Frozen = FALSE,
#'                 ResetObs = FALSE,
#'                 PMLStructure = "1Cpt")
#'
#' @noRd
#' @keywords internal NLME
new_Observation <- function(ObservationName = "",
                            SigmasChosen = Sigmas(Proportional = 0.1),
                            BQL = FALSE,
                            BQLValue = NA,
                            Frozen = FALSE,
                            ResetObs = FALSE,
                            Covariates = list(),
                            PMLStructure = character()) {
  stopifnot(length(ObservationName) == 1)

  if (!inherits(SigmasChosen, "Sigmas")) {
    SigmasChosen <-
      Sigmas(
        Additive = SigmasChosen[["Additive"]],
        LogAdditive = SigmasChosen[["LogAdditive"]],
        Proportional = SigmasChosen[["Proportional"]],
        AdditiveMultiplicative = list(
          PropPart = SigmasChosen$AdditiveMultiplicative["PropPart"],
          AddPart = SigmasChosen$AdditiveMultiplicative["AddPart"]
        ),
        MixRatio = list(
          PropPart = SigmasChosen$MixRatio["PropPart"],
          AddPart = SigmasChosen$MixRatio["AddPart"]
        ),
        Power = list(
          PowerPart = SigmasChosen$Power["Power"],
          StdevPart = SigmasChosen$Power["Stdev"]
        ),
        ObservationName = ObservationName
      )
  } else if (SigmasChosen$ObservationName == "") {
    # need to rebuild to get valid stparmnames
    SigmasChosen$ObservationName <- ObservationName

    SigmasChosen <-
      .update_SigmasNames(
        SigmasChosen = SigmasChosen,
        SigmaType = "AdditiveMultiplicative",
        PropNameDefault = "MultStdev",
        ObservationName = ObservationName
      )
    SigmasChosen <-
      .update_SigmasNames(
        SigmasChosen = SigmasChosen,
        SigmaType = "MixRatio",
        PropNameDefault = "MixRatio",
        ObservationName = ObservationName
      )
  }

  stopifnot(length(BQL) == 1)
  stopifnot(length(BQLValue) == 1)
  stopifnot(length(Frozen) == 1 || length(SigmasChosen))
  stopifnot(length(ResetObs) == 1)

  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for Observation ",
         ObservationName)
  }

  stopifnot(is.character(ObservationName) &
              .check_0nzchar(ObservationName))

  stopifnot(is.logical(BQL))
  stopifnot(is.na(BQLValue) || is.numeric(BQLValue))
  stopifnot(is.logical(Frozen))
  stopifnot(is.logical(ResetObs))

  if (!missing(Covariates)) {
    if (inherits(Covariates, "Covariate")) {
      Covariates <- list(Covariates)
    } else if (is.list(Covariates)) {
      stopifnot(all(sapply(Covariates, function(x)
        inherits(x, "Covariate"))))
    }
  } else {
    Covariates <- list()
  }

  stopifnot(is.character(PMLStructure))

  structure(
    list(
      ObservationName = ObservationName,
      SigmasChosen = SigmasChosen,
      BQL = BQL,
      BQLValue = BQLValue,
      Frozen = Frozen,
      ResetObs = ResetObs,
      Covariates = Covariates,
      PMLStructure = PMLStructure
    ),
    class = "Observation"
  )
}


validate_Observation <- function(ObservationInstance) {
  if (ObservationInstance$BQL &&
      !is.na(ObservationInstance$BQLValue) &&
      ObservationInstance$BQLValue <= 0) {
    stop("BQL value for ",
         ObservationInstance$ObservationName,
         " should be positive.")
  } else if (!ObservationInstance$BQL &&
             !is.na(ObservationInstance$BQLValue)) {
    warning("BQL value won't be used since BQL flag is FALSE.")
  }

  ObservationInstance
}

#' Create an instance of Observation class.
#'
#' This function creates a new instance of Observation object and validates it.
#'
#' @param ObservationName A character string giving the name of the Observation.
#' @param SigmasChosen a \link{Sigmas} class instance or a list specifying the
#'   chosen sigma values for different error models. 0s are treated as no
#'   values. Inside Observation class it is transormed and kept as \link{Sigmas}
#'   class. The list could contain the following error models:
#' * Additive The additive error sigma value.
#' * LogAdditive The log-additive error sigma value.
#' * Proportional The proportional error sigma value.
#' * AdditiveMultiplicative A numeric vector specifying the additive and multiplicative parts
#'   for the additive-multiplicative error model. The vector should have names
#'   `PropPart` and `AddPart`.
#' * MixRatio A numeric vector specifying the proportional and additive parts for the mix-ratio
#'   error model. The vector should have names `PropPart` and `AddPart`.
#' * Power A numeric vector specifying the standard deviation and power parts for the power error model.
#'   The vector should have names `StdevPart` and `PowerPart`.
#'
#' @param BQL A logical value indicating whether the dataset contains BQL values
#'   and they should be taken into account (M3 method).
#' @param BQLValue An optional numeric positive value of static LLOQ. Applicable
#'   only when BQL argument is `TRUE`. Any observed value less than or equal to
#'   that LLOQ value is treated as censored.
#' @param Frozen A logical value indicating if the standard deviation (Stdev) is
#'   frozen.
#' @param ResetObs A logical value indicating if the Observation variable should
#'   be reset to 0 after observation (`doafter={A0=0;}`). Applicable for
#'   elimination compartment.
#' @param Covariates A list of covariates (`Covariate` instances) that should be
#'   included in the model, but not linked to any of structural parameters.
#'   Used with "Emax" PD models ('C' covariate is added automatically when
#'   creating a new model, but should be added manually when modifying the model).
#' @param PMLStructure Character specifying the name of PML structure in which
#'   the observation should be added. For the naming convention of
#'   PMLStructures, see Details section of [get_PMLParametersSets()].
#'
#' @return A new Observation object
#'
#' @family Observations
#' @examples
#' A0Obs <-
#'   Observation(ObservationName = "A0Obs",
#'               SigmasChosen = list(Additive = 2,
#'                                   Power = c(Stdev = 10, Power = 0.5)),
#'               Frozen = FALSE,
#'               ResetObs = TRUE,
#'               PMLStructure = "PK1FOC")
#'
#' CObs <- Observation("CObs", Frozen = TRUE, PMLStructure = "2Cpt")
#'
#' @export
Observation <- function(ObservationName = "CObs",
                        SigmasChosen = Sigmas(Proportional = 0.1),
                        BQL = FALSE,
                        BQLValue = NA,
                        Frozen = FALSE,
                        ResetObs = FALSE,
                        Covariates = list(),
                        PMLStructure = character()) {
  ObservationInstance <- new_Observation(
    ObservationName = ObservationName,
    SigmasChosen = SigmasChosen,
    BQL = BQL,
    BQLValue = BQLValue,
    Frozen = Frozen,
    ResetObs = ResetObs,
    Covariates = Covariates,
    PMLStructure = PMLStructure
  )

  if (.check_0nzchar(ObservationInstance$PMLStructure)) {
    ObservationInstance <-
      .assign_TextToVariable(ParmList = ObservationInstance,
                             "PMLStructure",
                             ObservationInstance$PMLStructure)
  }

  validate_Observation(ObservationInstance)
}

.get_InputName <- function(ObservationName) {
  if (nchar(ObservationName) > 3 &
      tolower(substring(ObservationName, nchar(ObservationName) - 2)) == "obs") {
    InputName <-
      substr(ObservationName, 1, nchar(ObservationName) - 3)
  } else {
    InputName <- substr(ObservationName, 1, 1)
  }

  InputName
}

.update_SigmasNames <-
  function(SigmasChosen,
           SigmaType,
           PropNameDefault,
           ObservationName) {
    if (inherits(SigmasChosen[[SigmaType]]$PropPart[[1]], "StParm") &&
        SigmasChosen[[SigmaType]]$PropPart[[1]]$StParmName == PropNameDefault) {
      OldMultStdev <-
        SigmasChosen[[SigmaType]]$PropPart[[PropNameDefault]]
      SigmasChosen[[SigmaType]]$PropPart[[PropNameDefault]] <- NULL
      InputName <- .get_InputName(ObservationName)
      StParmName <- paste0(InputName, OldMultStdev$StParmName)
      SigmasChosen[[SigmaType]]$PropPart[[StParmName]] <-
        StParm(
          StParmName = StParmName,
          Type = OldMultStdev$Type,
          State = OldMultStdev$State,
          ThetaStParm = Theta(
            InitialEstimates = OldMultStdev$ThetaStParm$InitialEstimates,
            State = OldMultStdev$ThetaStParm$State,
            Frozen = OldMultStdev$ThetaStParm$Frozen,
            StParmName = StParmName
          ),
          OmegaStParm = Omega(
            InitialOmega = OldMultStdev$OmegaStParm$InitialOmega,
            State = OldMultStdev$OmegaStParm$State,
            Frozen = OldMultStdev$OmegaStParm$Frozen,
            StParmName = StParmName
          ),
          Covariates = OldMultStdev$Covariates
        )

    }

    SigmasChosen
  }

#' @export
output.Observation <- function(x, ...) {
  x <- validate_Observation(x)

  InputName <- .get_InputName(x$ObservationName)
  if (length(x$Covariates) == 1 &&
      x$Covariates[[1]]$Type == "Continuous") {
    ObserveRHS <-
      paste0(x$ObservationName, "(", x$Covariates[[1]]$Name, ")")
  } else {
    ObserveRHS <- x$ObservationName
  }

  ErrorName <- paste0(InputName, "Eps")
  OutputObservationParts <- c()
  ErrorModelNames <- names(x$SigmasChosen)
  ErrorModelNames <-
    ErrorModelNames[ErrorModelNames != "ObservationName"]

  if (x$BQL) {
    ObserveTextFinalPart <- ", bql"
    if (!is.na(x$BQLValue)) {
      ObserveTextFinalPart <-
        paste0(ObserveTextFinalPart, "=", x$BQLValue)
    }
  } else {
    ObserveTextFinalPart <- ""
  }

  if (x$ResetObs) {
    ObserveTextFinalPart <-
      paste0(ObserveTextFinalPart, ", doafter={", InputName, "=0; }")
  }

  ObserveTextFinalPart <- paste0(ObserveTextFinalPart, ")")

  ErrorTypes <-
    c(
      "Additive",
      "LogAdditive",
      "Proportional",
      "AdditiveMultiplicative",
      "MixRatio",
      "Power"
    )
  if (length(x$Frozen) == 1) {
    if (x$Frozen) {
      Freeze <- rep("(freeze)", length(ErrorTypes))
    } else {
      Freeze <- rep("", length(ErrorTypes))
    }

    names(Freeze) <- ErrorTypes
  } else {
    Freeze <- ifelse(x$Frozen, "(freeze)", "")
  }


  SigmasChosen <- validate_Sigmas(x$SigmasChosen)
  for (ErrorModelIndex in seq_along(ErrorModelNames)) {
    # adding error() part
    ErrorModelText <- ""
    if (ErrorModelNames[ErrorModelIndex] %in% c("Additive", "LogAdditive", "Proportional")) {
      if (SigmasChosen[[ErrorModelIndex]] <= 0)
        next
      ErrorModelText <-
        paste0("error", "(", ErrorName, Freeze[ErrorModelNames[ErrorModelIndex]], " = ", SigmasChosen[[ErrorModelIndex]])
    } else if (ErrorModelNames[ErrorModelIndex] %in% c("AdditiveMultiplicative", "MixRatio")) {
      if (SigmasChosen[[ErrorModelIndex]]$AddPart == 0)
        next

      ErrorModelText <-
        paste0("error",
               "(",
               ErrorName,
               Freeze[ErrorModelNames[ErrorModelIndex]],
               " = ",
               SigmasChosen[[ErrorModelIndex]]$AddPart)
    } else if (ErrorModelNames[ErrorModelIndex] == "Power") {
      if (SigmasChosen[[ErrorModelIndex]]$StdevPart == 0)
        next

      ErrorModelText <-
        paste0("error",
               "(",
               ErrorName,
               Freeze[ErrorModelNames[ErrorModelIndex]],
               " = ",
               SigmasChosen[[ErrorModelIndex]]$StdevPart)
    }

    ErrorModelText <- paste0(ErrorModelText, ")\n\t")

    # adding observe() part
    if (ErrorModelNames[ErrorModelIndex] == "Additive") {
      ErrorModelText <- paste0(
        ErrorModelText,
        "observe(",
        ObserveRHS,
        " = ",
        InputName,
        " + ",
        ErrorName,
        ObserveTextFinalPart
      )

    } else if (ErrorModelNames[ErrorModelIndex] == "LogAdditive") {
      ErrorModelText <- paste0(
        ErrorModelText,
        "observe(",
        ObserveRHS,
        " = ",
        InputName,
        " * exp(",
        ErrorName,
        ")",
        ObserveTextFinalPart
      )

    } else if (ErrorModelNames[ErrorModelIndex] == "Proportional") {
      ErrorModelText <- paste0(
        ErrorModelText,
        "observe(",
        ObserveRHS,
        " = ",
        InputName,
        " * (1 + ",
        ErrorName,
        ")",
        ObserveTextFinalPart
      )

    } else if (ErrorModelNames[ErrorModelIndex] == "AdditiveMultiplicative") {
      # Add + Mult
      ErrorModelText <- paste0(
        ErrorModelText,
        "observe(",
        ObserveRHS,
        " = ",
        InputName,
        " + ",
        ErrorName,
        "*sqrt(1 + ",
        InputName,
        "*",
        InputName,
        "*(",
        SigmasChosen[[ErrorModelIndex]]$PropPart[[1]]$StParmName,
        "/sigma())^2)",
        ObserveTextFinalPart
      )

      # adding stparm part
      ErrorModelText <- paste(ErrorModelText,
                              output(SigmasChosen[[ErrorModelIndex]]$PropPart[[1]]),
                              sep = "\n\t")

    } else if (ErrorModelNames[ErrorModelIndex] == "MixRatio") {
      # Mix Ratio
      ErrorModelText <- paste0(
        ErrorModelText,
        "observe(",
        ObserveRHS,
        " = ",
        InputName,
        " + ",
        ErrorName,
        "*(1 + ",
        InputName,
        "*",
        SigmasChosen[[ErrorModelIndex]]$PropPart[[1]]$StParmName,
        ")",
        ObserveTextFinalPart
      )

      # adding stparm part
      ErrorModelText <- paste(ErrorModelText,
                              output(SigmasChosen[[ErrorModelIndex]]$PropPart[[1]]),
                              sep = "\n\t")
    } else if (ErrorModelNames[ErrorModelIndex] == "Power") {
      # Power model
      ErrorModelText <- paste0(
        ErrorModelText,
        "observe(",
        ObserveRHS,
        " = ",
        InputName,
        " + ",
        InputName,
        "^",
        SigmasChosen[[ErrorModelIndex]]$PowerPart,
        "*",
        ErrorName,
        ObserveTextFinalPart
      )
    }

    OutputObservationParts <-
      c(OutputObservationParts, ErrorModelText)
  }

  if (length(OutputObservationParts) > 1) {
    OutputObservationParts <-
      add_TokensNLME(
        TokenName = paste("", #x$PMLStructure,
                          x$ObservationName,
                          sep = "_"),
        ListElementName = "Observation",
        TokenValues = OutputObservationParts,
        DoNotChangeTokenListMain = FALSE
      )
  }

  OutputObservationParts
}

#' @export
print.Observation <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
