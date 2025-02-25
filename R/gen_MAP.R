#' Model Term = Column Term
#' Mapping <-
#'   c(ID = "ID", TIME = "TIME", AMT = "AMT", DV = "DV", RATE = "RATE")
#' @noRd
#' @keywords internal NLME
gen_MAP <- function(Mapping,
                    PMLParametersSets,
                    DataFilePath) {
  stopifnot(all(!is.null(Mapping)))
  stopifnot(all(!is.na(Mapping)))
  stopifnot(all(nchar(Mapping) > 0))

  Data <- utils::read.csv(DataFilePath)
  DataNames <- colnames(Data)

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
        paste(Mapping[!Mapping %in% DataNames], collapse = ", ")
      )
    }
  }

  MapRow <- ""
  # investigate the model for the model terms to be mapped
  CovariateNames <-
    unique(.gather_ClassProperties(PMLParametersSets, "Covariate", "Name", c()))
  CovariateNamesNotMapped <-
    CovariateNames[is.na(match(CovariateNames, MappingNames))]
  if (length(CovariateNamesNotMapped) > 0) {
    warning(
      "Some covariate(s) are not mapped: ",
      paste(CovariateNamesNotMapped, collapse = ", ")
    )
    CovariateNamesNotMappedNoColumns <-
      CovariateNamesNotMapped[is.na(match(CovariateNamesNotMapped, DataNames))]
    if (length(CovariateNamesNotMappedNoColumns) > 0) {
      stop("Cannot map covariate(s): ",
           paste(CovariateNamesNotMappedNoColumns, collapse = ", "))
    } else {
      message(
        "Covariates are mapped from column names: ",
        paste(CovariateNamesNotMapped, collapse = ", ")
      )
    }

    names(CovariateNamesNotMapped) <- CovariateNamesNotMapped
    Mapping <- c(Mapping, CovariateNamesNotMapped)
    MappingNames <- names(Mapping)
  }

  DataMappingCovariates <- Mapping[MappingNames %in% CovariateNames]

  MapRow <-
    paste(MapRow,
          paste(
            names(DataMappingCovariates),
            DataMappingCovariates,
            sep = "=",
            collapse = " "
          ))

  # covariates are not required anymore, removing from mapping
  Mapping <- Mapping[!MappingNames %in% CovariateNames]
  MappingNames <- names(Mapping)

  PMLMAPSet <- c()
  for (PMLParametersSetIndex in seq_along(PMLParametersSets)) {
    PMLParametersSet <- PMLParametersSets[[PMLParametersSetIndex]]
    PMLParametersSetName <-
      names(PMLParametersSets)[PMLParametersSetIndex]
    PMLMAPSet[PMLParametersSetName] <- ""
    if (!is.null(PMLParametersSet$MainDosepoint) &&
        names(PMLParametersSet$MainDosepoint) %in% MappingNames) {
      Dosepointname <- names(PMLParametersSet$MainDosepoint)
      PMLMAPSet[PMLParametersSetName] <-
        paste(PMLMAPSet[PMLParametersSetName],
              Dosepointname, "=", Mapping[MappingNames == Dosepointname])
      # MainDosepoint is found in model terms of Mapping
      if ("AMT" %in% MappingNames) {
        warning(
          "Cannot map ",
          Dosepointname,
          " and reserved word AMT simultaneously. ",
          "AMT mapping term will be ignored."
        )
        Mapping <- Mapping[!"AMT" %in% MappingNames]
        MappingNames <- names(Mapping)
      }

      # are there _Duration or _Rate for this dosepoint?
      PMLMAPSet <-
        .map_RateDuration(
          Dosepointname = Dosepointname,
          Rate = paste0(Dosepointname, "_Rate"),
          Duration = paste0(Dosepointname, "_Duration"),
          Mapping = Mapping,
          MappingNames = MappingNames,
          PMLMAPSet = PMLMAPSet,
          PMLParametersSetName = PMLParametersSetName
        )
    } else if ("AMT" %in% MappingNames) {
      if (is.null(PMLParametersSet$MainDosepoint)) {
        warning("AMT Model term is presented in mapping, but no dosepoint is found.")
        next
      }

      Dosepointname <- names(PMLParametersSet$MainDosepoint)
      PMLMAPSet[PMLParametersSetName] <-
        paste(PMLMAPSet[PMLParametersSetName],
              Dosepointname, "=", Mapping[MappingNames %in% "AMT"])

      if ((paste0(Dosepointname, "_Rate") %in% MappingNames) ||
          (paste0(Dosepointname, "_Duration") %in% MappingNames)) {
        message(
          paste0(Dosepointname, "_Rate"),
          " or ",
          paste0(Dosepointname, "_Duration"),
          " found in mapping for ",
          PMLParametersSetName
        )
        # are there _Duration or _Rate for this dosepoint?
        PMLMAPSet <-
          .map_RateDuration(
            Dosepointname = Dosepointname,
            Rate = paste0(Dosepointname, "_Rate"),
            Duration = paste0(Dosepointname, "_Duration"),
            Mapping = Mapping,
            MappingNames = MappingNames,
            PMLMAPSet = PMLMAPSet,
            PMLParametersSetName = PMLParametersSetName
          )

      } else {
        # are there _Duration or _Rate for this dosepoint?
        PMLMAPSet <-
          .map_RateDuration(
            Dosepointname = Dosepointname,
            Rate = "Rate",
            Duration = "Duration",
            Mapping = Mapping,
            MappingNames = MappingNames,
            PMLMAPSet = PMLMAPSet,
            PMLParametersSetName = PMLParametersSetName
          )
      }

    } else if (!is.null(PMLParametersSet$MainDosepoint) &&
               !names(PMLParametersSet$MainDosepoint) %in% MappingNames) {
      message(
        "Cannot map ",
        names(PMLParametersSet$MainDosepoint),
        " for ",
        PMLParametersSetName,
        " since there's no corresponding mapping name given."
      )
    } else if (is.null(PMLParametersSet$MainDosepoint) && PMLParametersSet$Type == "PK") {
      message(
        "Main dosepoint for PML set ",
        PMLParametersSetName,
        " is empty. Please check if it is intended."
      )
    }
  }

  DosepointNames <-
    unique(.gather_ClassProperties(PMLParametersSets, "Dosepoint", "DosepointName", c()))
  # Dosepoints are mapped. We can remove it from mapping
  Mapping <-
    Mapping[!MappingNames %in% c(DosepointNames,
                                 "AMT",
                                 "Rate",
                                 "Duration",
                                 paste0(DosepointNames, c("_Rate", "_Duration")))]
  MappingNames <- names(Mapping)

  # Observations
  ObservationNames <-
    unique(
      .gather_ClassProperties(PMLParametersSets, "Observation", "ObservationName", c())
    )

  ObservationNamesNotMapped <-
    ObservationNames[is.na(match(ObservationNames, MappingNames))]

  if (length(ObservationNames) == length(ObservationNamesNotMapped)) {
    stop("No observations are mapped: ",
         paste(ObservationNames, collapse = ", "))
  }

  BQLObsNames <- c()
  for (PMLParametersSetIndex in seq_along(PMLParametersSets)) {
    PMLParametersSet <- PMLParametersSets[[PMLParametersSetIndex]]
    PMLParametersSetName <-
      names(PMLParametersSets)[PMLParametersSetIndex]

    ObservationNamesFound <-
      Mapping[!is.na(match(MappingNames, names(PMLParametersSet$Observations)))]
    if (length(ObservationNamesFound) == 0) {
      stop(
        "No observations are mapped for a structure ",
        PMLParametersSetName,
        "\n",
        PMLParametersSet$PMLStructure
      )
    }

    PMLMAPSet[PMLParametersSetName] <-
      paste(PMLMAPSet[PMLParametersSetName],
            paste(names(ObservationNamesFound),
                  "=",
                  Mapping[match(names(ObservationNamesFound), MappingNames)],
                  collapse = " "))

    for (Obs in PMLParametersSet$Observations) {
      if (!Obs$ObservationName %in% MappingNames)
        next
      if (!Obs$BQL)
        next
      if (!is.na(Obs$BQLValue))
        next

      BQLObsName <- paste0(Obs$ObservationName, "BQL")
      if (BQLObsName %in% MappingNames) {
        BQLNameFound <-
          Mapping[which(BQLObsName == MappingNames)]

        PMLMAPSet[PMLParametersSetName] <-
          paste(PMLMAPSet[PMLParametersSetName],
                paste(BQLObsName,
                      "=",
                      BQLNameFound,
                      collapse = " "))

        BQLObsNames <- c(BQLObsNames, BQLObsName)
      } else {
        warning(
          "For observation ",
          Obs$ObservationName,
          " BQL flag is set, but BQL column is not given."
        )
      }
    }
  }

  # Observations are mapped. We can remove it from mapping
  Mapping <-  Mapping[!MappingNames %in% ObservationNames]
  MappingNames <- names(Mapping)

  if (!is.null(BQLObsNames)) {
    Mapping <- Mapping[!MappingNames %in% BQLObsNames]
    MappingNames <- names(Mapping)
  }

  MappingNotPresent <- Mapping[!Mapping %in% DataNames]
  if (length(MappingNotPresent) > 0) {
    warning(
      "Current mappings are not found in the dataset: ",
      paste(MappingNotPresent, collapse = ", ")
    )
  }


  if (length(PMLParametersSets) == 1) {
    # do not need tokens
    MapRow <-
      paste(MapRow,
            PMLMAPSet[1],
            paste(MappingNames, "=", Mapping, collapse = " "))
  } else {
    # create tokens
    TokenizedDoseObs <-
      add_TokensNLME(
        TokenName = "PML",
        ListElementName = "MAPText",
        TokenValues = PMLMAPSet,
        DoNotChangeTokenListMain = FALSE
      )

    MapRow <-
      paste(MapRow,
            TokenizedDoseObs,
            paste(MappingNames, "=", Mapping, collapse = " "))
  }

  MapRow
}

.gather_ClassProperties <-
  function(List,
           ClassName,
           ClassPropertyName,
           Collection) {
    if (!is.list(List)                   |
        length(List) == 0                |
        !.check_0nzchar(ClassName)       |
        !.check_0nzchar(ClassPropertyName))
      return(Collection)

    for (iElement in 1:length(List)) {
      if (inherits(List[[iElement]], ClassName)) {
        Collection <- c(Collection, List[[iElement]][[ClassPropertyName]])
      }

      if (is.list(List[[iElement]])) {
        Collection <-
          .gather_ClassProperties(List[[iElement]], ClassName, ClassPropertyName, Collection)
      }
    }

    Collection
  }


.map_RateDuration <- function(Dosepointname,
                              Rate,
                              Duration,
                              Mapping,
                              MappingNames,
                              PMLMAPSet,
                              PMLParametersSetName) {
  # are there _Duration or _Rate for this dosepoint?
  if (Rate %in% MappingNames) {
    if (Duration %in% MappingNames) {
      stop("Cannot map ", Rate, " and ",
           Duration, " simultaneously.")
    }

    PMLMAPSet[PMLParametersSetName] <-
      paste(PMLMAPSet[PMLParametersSetName],
            paste0(Dosepointname, "_Rate"), "=", Mapping[Rate == MappingNames])
  }

  if (Duration %in% MappingNames) {
    PMLMAPSet[PMLParametersSetName] <-
      paste(PMLMAPSet[PMLParametersSetName],
            paste0(Dosepointname, "_Duration"),
            "=",
            Mapping[Duration == MappingNames])
  }

  PMLMAPSet
}
