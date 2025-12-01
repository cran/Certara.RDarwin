#' Model Term = Column Term
#' Mapping <-
#'   list(PK2FOC = c(ID = "ID", TIME = "TIME", Aa = "Dose1", DV = "DV", RATE = "RATE"),
#'        PK2GC  = c(ID = "ID", TIME = "TIME", A1 = "Dose2", Aa = "Dose1", DV = "DV", RATE = "RATE"))
#' used if there's a special mapping is required for some spaces
#' @noRd
#' @keywords internal NLME
gen_MAPList <- function(MappingList,
                          PMLParametersSets,
                          DataFilePath) {
  stopifnot(is.list(MappingList))
  stopifnot(all(!is.na(unlist(MappingList))))
  stopifnot(all(nchar(unlist(MappingList)) > 0))

  stopifnot(length(MappingList) == length(PMLParametersSets))
  if (is.null(names(MappingList))) {
    stop("MappingList should be named list")
  }

  Data <- utils::read.csv(DataFilePath)
  DataNames <- colnames(Data)

  # we will create a separate map for each space
  SpaceNames <- names(PMLParametersSets)
  PMLMAPSet <- character(0)
  for (SpaceName in SpaceNames) {
    Mapping <- MappingList[[SpaceName]]
    # treat empty model terms
    MappingNames <- names(Mapping)

    EmptyModelNamesFlag <-
      is.null(MappingNames) |
      is.na(MappingNames) |
      MappingNames == ""

    names(Mapping)[EmptyModelNamesFlag] <-
      Mapping[EmptyModelNamesFlag]
    # restore Mapping model terms after assigning of empty model terms
    MappingNames <- names(Mapping)
    if (!all(Mapping %in% DataNames)) {
      # need to check if categories are presented
      if (!all(sapply(strsplit(Mapping, "\\("), function(x)
        x[1]) %in% DataNames)) {
        stop(
          "Following columns presented in mapping, but not in the dataset:",
          paste(Mapping[!Mapping %in% DataNames], collapse = ", "),
          " for a Space ", SpaceName
        )
      }
    }

    if (any(duplicated(Mapping))) {
      stop("Duplicated model terms in mapping: ",
           paste(Mapping[duplicated(Mapping)], collapse = ", "),
           " for a Space ", SpaceName)
    }

    if (any(duplicated(MappingNames))) {
      stop("Duplicated column names in mapping: ",
           paste(MappingNames[duplicated(MappingNames)], collapse = ", "),
           " for a Space ", SpaceName)
    }

    PMLParametersSet <- PMLParametersSets[[SpaceName]]
    PMLMAPSet[SpaceName] <- ""
    CurrentMapRowMapping <- Mapping
    CurrentMappingNames <- MappingNames

    # investigate the model for the covariates to be mapped
    CovariateNames <-
      unique(c(
        .gather_ClassProperties(PMLParametersSet, "Covariate", "Name", c()),
        unique(
          .gather_ClassProperties(PMLParametersSet, "CovariateCustom", "Name", c())
        )
      ))

    CovariateNamesNotMapped <-
      CovariateNames[is.na(match(CovariateNames, CurrentMappingNames))]
    if (length(CovariateNamesNotMapped) > 0) {
      warning(
        "Some covariate(s) are not mapped: ",
        paste(CovariateNamesNotMapped, collapse = ", "),
        " for a Space ", SpaceName
      )
      CovariateNamesNotMappedNoColumns <-
        CovariateNamesNotMapped[is.na(match(CovariateNamesNotMapped, DataNames))]
      if (length(CovariateNamesNotMappedNoColumns) > 0) {
        stop(
          "Cannot map covariate(s): ",
          paste(CovariateNamesNotMappedNoColumns, collapse = ", "),
          " for a Space ", SpaceName
        )
      } else {
        message(
          "Covariates are mapped from column names: ",
          paste(CovariateNamesNotMapped, collapse = ", "),
          " for a Space ", SpaceName
        )
      }

      names(CovariateNamesNotMapped) <- CovariateNamesNotMapped
      CurrentMapRowMapping <-
        c(CurrentMapRowMapping, CovariateNamesNotMapped)
      CurrentMappingNames <- names(CurrentMapRowMapping)
    }

    DataMappingCovariates <-
      Mapping[CurrentMappingNames %in% CovariateNames]

    PMLMAPSet[SpaceName] <-
      paste(PMLMAPSet[SpaceName],
            paste(
              names(DataMappingCovariates),
              DataMappingCovariates,
              sep = "=",
              collapse = " "
            ))

    ## covariates are not required anymore, removing from mapping
    CurrentMapRowMapping <-
      CurrentMapRowMapping[!CurrentMappingNames %in% CovariateNames]
    CurrentMappingNames <- names(CurrentMapRowMapping)

    # Dosing
    if (is.null(PMLParametersSet$MainDosepoint) &&
        PMLParametersSet$Type == "Custom" &&
        length(PMLParametersSet$CustomDosepoints) > 0) {
      message(
        "For a space named ",
        SpaceName,
        " the main dosepoint is set to ",
        names(PMLParametersSet$CustomDosepoints)[1]
      )
      PMLParametersSet$MainDosepoint <-
        PMLParametersSet$CustomDosepoints[1]
      PMLParametersSet$CustomDosepoints[[1]] <- NULL
    }

    if (!is.null(PMLParametersSet$MainDosepoint)) {
      MainDosepointName <- names(PMLParametersSet$MainDosepoint)
      if (length(PMLParametersSet$CustomDosepoints) > 0 &&
          MainDosepointName %in% names(PMLParametersSet$CustomDosepoints)) {
        message(
          "Since multiple dosing terms with the same name are used, they should be mapped as terms with indices: ",
          paste0(MainDosepointName, c("_1", "_2"), collapse = ", ")
        )
        MainDosepointName <- paste0(MainDosepointName, "_1")
      }
    }

    if (!is.null(PMLParametersSet$MainDosepoint) &&
        MainDosepointName %in% CurrentMappingNames) {
      # MainDosepoint is found in the model terms of Mapping
      PMLMAPSet[SpaceName] <-
        paste(PMLMAPSet[SpaceName],
              MainDosepointName,
              "=",
              CurrentMapRowMapping[CurrentMappingNames == MainDosepointName])

      if ("AMT" %in% CurrentMappingNames) {
        warning(
          "Cannot map ",
          MainDosepointName,
          " and reserved word AMT simultaneously. ",
          "AMT mapping term will be ignored",
          " for a Space ", SpaceName
        )

        CurrentMapRowMapping <-
          CurrentMapRowMapping[!"AMT" %in% CurrentMappingNames]
        CurrentMappingNames <- names(CurrentMapRowMapping)
      }

      if (length(PMLParametersSet$MainDosepoint[[1]]$rate) +
          length(PMLParametersSet$MainDosepoint[[1]]$duration) == 0) {
        # should not map rate or duration if it is given in PML
        # are there _Duration or _Rate for this dosepoint?
        PMLMAPSet[SpaceName] <-
          .map_RateDuration(
            DosepointName = MainDosepointName,
            Rate = paste0(MainDosepointName, "_Rate"),
            Duration = paste0(MainDosepointName, "_Duration"),
            Mapping = CurrentMapRowMapping,
            MappingNames = CurrentMappingNames,
            PMLMAPSet = PMLMAPSet[SpaceName],
            PMLParametersSetName = SpaceName
          )
      }
    } else if (!is.null(PMLParametersSet$MainDosepoint) &&
               "AMT" %in% CurrentMappingNames) {
      # AMT is found and should be mapped
      if (is.null(PMLParametersSet$MainDosepoint)) {
        stop(
          paste0(
            "AMT Model term is presented in mapping, but no dosepoint is found for a space named",
            SpaceName
          )
        )
      }

      PMLMAPSet[SpaceName] <-
        paste(PMLMAPSet[SpaceName],
              MainDosepointName,
              "=",
              CurrentMapRowMapping[CurrentMappingNames %in% "AMT"])

      if (length(PMLParametersSet$MainDosepoint[[1]]$rate) +
          length(PMLParametersSet$MainDosepoint[[1]]$duration) == 0) {
        # should not map rate or duration if it is given in PML
        if ((paste0(MainDosepointName, "_Rate") %in% CurrentMappingNames) ||
            (paste0(MainDosepointName, "_Duration") %in% CurrentMappingNames)) {
          message(
            paste0(MainDosepointName, "_Rate"),
            " or ",
            paste0(MainDosepointName, "_Duration"),
            " found in mapping for ",
            SpaceName
          )
          # are there _Duration or _Rate for this dosepoint?
          PMLMAPSet[SpaceName] <-
            .map_RateDuration(
              DosepointName = MainDosepointName,
              Rate = paste0(MainDosepointName, "_Rate"),
              Duration = paste0(MainDosepointName, "_Duration"),
              Mapping = CurrentMapRowMapping,
              MappingNames = CurrentMappingNames,
              PMLMAPSet = PMLMAPSet[SpaceName],
              PMLParametersSetName = SpaceName
            )

        } else {
          # are there _Duration or _Rate for this dosepoint?
          PMLMAPSet[SpaceName] <-
            .map_RateDuration(
              DosepointName = MainDosepointName,
              Rate = "Rate",
              Duration = "Duration",
              Mapping = CurrentMapRowMapping,
              MappingNames = CurrentMappingNames,
              PMLMAPSet = PMLMAPSet[SpaceName],
              PMLParametersSetName = SpaceName
            )
        }
      }
    } else if (!is.null(PMLParametersSet$MainDosepoint) &&
               !MainDosepointName %in% CurrentMappingNames) {
      message(
        "Cannot map ",
        names(PMLParametersSet$MainDosepoint),
        " for ",
        SpaceName,
        " since there's no corresponding mapping name given."
      )
    } else if (is.null(PMLParametersSet$MainDosepoint) &&
               PMLParametersSet$Type == "PK") {
      message("Main dosepoint for PML set ",
              SpaceName,
              " is empty. Please check if it is intended.")
    }

    if (length(PMLParametersSet$CustomDosepoints) > 0) {
      DosepointNames <- names(PMLParametersSet$CustomDosepoints)

      for (DosepointName in DosepointNames) {
        if (names(PMLParametersSet$MainDosepoint) == DosepointName) {
          # dosepoint2
          DosepointName <- paste0(DosepointName, "_2")
        }

        PMLMAPSet[SpaceName] <-
          paste(PMLMAPSet[SpaceName],
                DosepointName,
                "=",
                CurrentMapRowMapping[CurrentMappingNames == DosepointName])

        if (length(PMLParametersSet$CustomDosepoints[[DosepointName]]$rate) +
            length(PMLParametersSet$MainDosepoint[[DosepointName]]$duration) == 0) {
          PMLMAPSet[SpaceName] <-
            .map_RateDuration(
              DosepointName = DosepointName,
              Rate = paste0(DosepointName, "_Rate"),
              Duration = paste0(DosepointName, "_Duration"),
              Mapping = CurrentMapRowMapping,
              MappingNames = CurrentMappingNames,
              PMLMAPSet = PMLMAPSet[SpaceName],
              PMLParametersSetName = SpaceName
            )
        }
      }
    }

    DosepointNames <-
      unique(c(
        .gather_ClassProperties(PMLParametersSet, "Dosepoint", "DosepointName", c()),
        .gather_ClassProperties(PMLParametersSet, "DosepointCustom", "DosepointName", c())
      ))

    if (length(DosepointNames) > 0) {
      ## Dosepoints are mapped. We can remove it from mapping
      CurrentMapRowMapping <-
        CurrentMapRowMapping[!CurrentMappingNames %in% c(DosepointNames,
                                                         "AMT",
                                                         "Rate",
                                                         "Duration",
                                                         paste0(DosepointNames, c("_Rate", "_Duration", "_1", "_2")))]

      CurrentMappingNames <- names(CurrentMapRowMapping)
    }

    # Observations
    DefaultObservationNames <-
      .gather_ClassProperties(PMLParametersSet, "Observation", "ObservationName", c())
    # includes custom observations and responses:
    ResponsesNames <- names(PMLParametersSet$Responses)
    CustomObservationNames <-
      ResponsesNames[sapply(PMLParametersSet$Responses, function(x) {
        x$Type == "observe"
      })]

    ObservationResponsesNames <-
      unique(c(DefaultObservationNames,
               ResponsesNames))

    ObservationResponsesNamesFound <-
      CurrentMapRowMapping[!is.na(match(CurrentMappingNames, ObservationResponsesNames))]
    if (length(ObservationResponsesNamesFound) == 0) {
      stop("No observations/responses are mapped for a space ",
           SpaceName)
    } else if (length(ObservationResponsesNamesFound) <
               length(ObservationResponsesNames)) {
      warning(
        "Current observations/responses found in the space '",
        SpaceName,
        "' are not mapped: ",
        paste(ObservationResponsesNames[!ObservationResponsesNames %in% ObservationResponsesNamesFound],
              collapse = ", "),
        call. = FALSE
      )
    }

    PMLMAPSet[SpaceName] <-
      paste(PMLMAPSet[SpaceName],
            paste(
              names(ObservationResponsesNamesFound),
              "=",
              ObservationResponsesNamesFound,
              collapse = " "
            ))

    ObservationNames <-
      c(DefaultObservationNames,
        CustomObservationNames)

    ObservationNamesFound <-
      CurrentMappingNames[!is.na(match(CurrentMappingNames, ObservationNames))]
    BQLObsNames <- c()

    for (ObsName in ObservationNamesFound) {
      if (ObsName %in% DefaultObservationNames) {
        Obs <- PMLParametersSet$Observations[[ObsName]]
      } else {
        Obs <- PMLParametersSet$Responses[[ObsName]]
      }

      if (!Obs$BQL ||
          !is.na(Obs$BQLValue))
        next

      BQLObsName <- paste0(Obs$ObservationName, "BQL")
      if (BQLObsName %in% CurrentMappingNames) {
        BQLNameFound <-
          CurrentMapRowMapping[which(BQLObsName == CurrentMappingNames)]

        PMLMAPSet[SpaceName] <-
          paste(PMLMAPSet[SpaceName],
                paste(BQLObsName,
                      "=",
                      BQLNameFound,
                      collapse = " "))

        BQLObsNames <- c(BQLObsNames, BQLObsName)
      } else {
        warning(
          "Observation ",
          Obs$ObservationName,
          " has BQL flag set, but BQL column is not given ",
          " for a Space ", SpaceName
        )
      }
    }

    # Observations are mapped. We can remove it from mapping
    CurrentMapRowMapping <-
      CurrentMapRowMapping[!CurrentMappingNames %in% c(ObservationNamesFound, BQLObsNames)]
    CurrentMappingNames <- names(CurrentMapRowMapping)

    if (length(BQLObsNames) > 0) {
      CurrentMapRowMapping <-
        CurrentMapRowMapping[!CurrentMappingNames %in% BQLObsNames]
      CurrentMappingNames <- names(CurrentMapRowMapping)
    }

    SpecialMappingTerms <-
      c("ADDL", "SS", "II", "SSOFFSET", "RESET" , "MDV", "id\\d?")
    Pattern <-
      paste0("(^", SpecialMappingTerms, "$)", collapse = "|")

    for (MapTerm in CurrentMappingNames) {
      if (grepl(Pattern, MapTerm, ignore.case = TRUE)) {
        PMLMAPSet[SpaceName] <-
          paste(PMLMAPSet[SpaceName],
                paste(MapTerm,
                      "=",
                      CurrentMapRowMapping[which(MapTerm == CurrentMappingNames)],
                      collapse = " "))
      }
    }

    # Time
    if ((PMLParametersSet$Type == "Custom" &&
         PMLParametersSet$TimeBased) ||
        (PMLParametersSet$Type == "PK")) {
      if (!"time" %in% CurrentMappingNames) {
        warning("Time is not mapped for ", SpaceName, " Space.")
      } else {
        PMLMAPSet[SpaceName] <-
          paste(PMLMAPSet[SpaceName],
                paste("time =",
                      CurrentMapRowMapping[which("time" == CurrentMappingNames)],
                      collapse = " "))
      }
    }
  }

  if (length(PMLParametersSets) == 1) {
    # do not need tokens
    MapRow <- PMLMAPSet[1]
  } else {
    # create tokens
    MapRow <-
      add_TokensNLME(
        TokenName = "PML",
        ListElementName = "MAPText",
        TokenValues = PMLMAPSet,
        DoNotChangeTokenListMain = FALSE
      )
  }

  MapRow
}
