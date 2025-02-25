#' Get Model Terms to Map
#'
#' This function retrieves the model terms that can be mapped from a set of PML
#' models.
#'
#' @param PMLParametersSets An object of class "PMLModels" containing PML model
#'   parameters.
#' @return A list with two elements: "Required" and
#'   "Optional," representing the model terms that can be mapped.
#'
#' @examples
#' # Load your PMLModels object
#' PMLParametersSets <-
#'   create_ModelPK(
#'     Absorption = c("First-Order", "Weibull"),
#'     CObs = Observation(
#'       ObservationName = "CObs",
#'       BQL = TRUE),
#'     A1 = Dosepoint(
#'       DosepointName = "A1",
#'       rate = StParm(StParmName = "Rate")),
#'     Weight = Covariate(
#'       Name = "Weight",
#'       Center = "Median")
#' )
#'
#' # Get the model terms to map
#' terms_to_map <- get_ModelTermsToMap(PMLParametersSets)
#' print(terms_to_map$Required)
#' print(terms_to_map$Optional)
#'
#' @seealso [create_ModelPK()] [create_ModelPD()] [create_CustomSpace()]
#'
#' @keywords NLME
#' @export
get_ModelTermsToMap <- function(PMLParametersSets) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))

  IDTime <- c("id")
  TimeBasedArray <-
    sapply(PMLParametersSets, function(x) {
      x$Type == "PK" ||
        (x$Type == "Custom" && x$TimeBased)
    })

  if (all(TimeBasedArray)) {
    IDTime <- c(IDTime, "time")
  } else if (any(TimeBasedArray)) {
    IDTime <- c(IDTime, "time")
    warning(
      "Not all spaces are time based, i.e. ",
      paste0(
        "`",
        which(!TimeBasedArray, arr.ind = TRUE),
        "`",
        "-",
        names(PMLParametersSets),
        collapse = ", "
      ),
      call. = FALSE,
      immediate. = TRUE
    )
  }

  CovariateNames <-
    list_Covariates(PMLParametersSets)

  DosepointNames <- c()
  OptionalDosepointNames <- c()
  for (PMLParametersSet in PMLParametersSets) {
    if (is.null(PMLParametersSet$MainDosepoint) &&
        PMLParametersSet$Type == "Custom" &&
        length(PMLParametersSet$CustomDosepoints) > 0) {
      PMLParametersSet$MainDosepoint <-
        PMLParametersSet$CustomDosepoints[1]
      PMLParametersSet$CustomDosepoints[[1]] <- NULL
    }

    if (length(PMLParametersSet$MainDosepoint) > 0) {
      DosepointName <- PMLParametersSet$MainDosepoint[[1]]$DosepointName

      if (PMLParametersSet$Type == "Custom") {
        if (length(PMLParametersSet$CustomDosepoints) > 0 &&
            DosepointName %in% names(PMLParametersSet$CustomDosepoints)) {
          DosepointName <- paste0(DosepointName, "_1")
        }

        DosepointNames <- c(DosepointNames,
                            DosepointName)

        if (length(PMLParametersSet$MainDosepoint[[1]]$rate) +
            length(PMLParametersSet$MainDosepoint[[1]]$duration) == 0) {
          OptionalDosepointNames <- c(OptionalDosepointNames,
                                      paste0(DosepointName, c("_Duration", "_Rate")))
        }

        # all other are optional
        for (CustomDosepoint in PMLParametersSet$CustomDosepoints) {
          DosepointName <- CustomDosepoint$DosepointName
          if (CustomDosepoint$DosepointName == PMLParametersSet$MainDosepoint[[1]]$DosepointName) {
            if (CustomDosepoint$DoseType == "dosepoint2") {
              DosepointName <- paste0(DosepointName, "_2")
            } else {
              next
            }
          }

          OptionalDosepointNames <-
            c(OptionalDosepointNames,
              DosepointName)

          if (length(CustomDosepoint$rate) +
              length(CustomDosepoint$duration) == 0) {
            OptionalDosepointNames <-
              c(OptionalDosepointNames,
                paste0(DosepointName,
                       c("_Duration", "_Rate")))
          }
        }
      } else {
        # PK
        DosepointNames <- c(DosepointNames,
                            DosepointName)

        if (!inherits(PMLParametersSet$MainDosepoint[[1]]$duration, "StParm") &&
            !inherits(PMLParametersSet$MainDosepoint[[1]]$rate, "StParm")) {
          # if not StParms and the space is not custom - they could be mapped
          OptionalDosepointNames <- c(OptionalDosepointNames,
                                      paste0(DosepointName, c("_Duration", "_Rate")))
        }
      }
    }
  }
  if (length(unique(DosepointNames)) > 1) {
    message(
      "For the mapping purposes, dosing Terms ",
      paste(DosepointNames, collapse = ", "),
      " could be substituted to `AMT`."
    )
  }

  # Observations
  ObservationResponsesNames <- c()
  BQLObsNames <- c()
  for (PMLParametersSet in PMLParametersSets) {
    DefaultObservationNames <-
      .gather_ClassProperties(PMLParametersSet, "Observation", "ObservationName", c())
    # includes custom observations and responses:
    ResponsesNames <- names(PMLParametersSet$Responses)
    CustomObservationNames <-
      ResponsesNames[sapply(PMLParametersSet$Responses, function(x) {
        x$Type == "observe"
      })]

    ObservationResponsesNames <-
      unique(c(ObservationResponsesNames,
               DefaultObservationNames,
               ResponsesNames))

    ObservationNames <-
      c(DefaultObservationNames,
        CustomObservationNames)


    for (ObsName in ObservationNames) {
      if (ObsName %in% DefaultObservationNames) {
        Obs <- PMLParametersSet$Observations[[ObsName]]
      } else {
        Obs <- PMLParametersSet$Responses[[ObsName]]
      }

      if (!Obs$BQL ||
          !is.na(Obs$BQLValue))
        next

      BQLObsNames <-
        c(BQLObsNames, paste0(Obs$ObservationName, "BQL"))
    }
  }


  list(Required = unique(
    c(
      IDTime,
      CovariateNames,
      DosepointNames,
      ObservationResponsesNames
    )
  ),
  Optional = unique(c(OptionalDosepointNames, BQLObsNames)))
}
