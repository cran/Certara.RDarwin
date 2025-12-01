get_ModelBlock <- function(PMLParametersSets, OmegaSearchBlocks) {
  # initial checks for OmegaSearchBlocks
  if (length(OmegaSearchBlocks) > 0) {
    OmegaNames <-
      .gather_ClassFields(
        ParmList = PMLParametersSets,
        ClassName = "Omega",
        FieldName = "Name",
        ClassFields = c()
      )


    OmegaCustomNames <- c()
    for (PMLParametersSet in PMLParametersSets) {
      if (PMLParametersSet$Type != "Custom")
        next

      OmegaCustomNames <- c(OmegaCustomNames,
                            sapply(PMLParametersSet$CustomRanefs, "[[", "RanefNames"))
    }

    OmegaNames <- unique(OmegaNames, OmegaCustomNames)

    for (OmegaSearchBlock in OmegaSearchBlocks) {
      if (!is.character(OmegaSearchBlock)) {
        stop("OmegaSearchBlock should be character vector, not ",
             typeof(OmegaSearchBlock))
      } else if (length(OmegaSearchBlock) < 2) {
        stop(
          "Cannot build the OmegaSearchBlock if only one Omega presented: ",
          OmegaSearchBlock
        )
      }

      if (any(is.na(match(OmegaSearchBlock, OmegaNames)))) {
        warning(
          "OmegaSearchBlock should be a vector of valid Omega names, ",
          "following Omega(s) are not presented: ",
          paste(OmegaSearchBlock[is.na(match(OmegaSearchBlock, OmegaNames))],
                collapse = ", "),
          "\nPlease review the space # ", which(PMLParametersSet == PMLParametersSets, arr.ind = TRUE)
        )
      }
    }

    if (any(duplicated(unlist(OmegaSearchBlocks)))) {
      stop("Omegas in the search_block() should not be duplcated between blocks.")
    }
  }

  OmegaSearchBlocksText <-
    vector(mode = "character", length = length(PMLParametersSets))
  for (PMLParametersSetIndex in seq_along(PMLParametersSets)) {
    if (length(OmegaSearchBlocks) == 0) {
      break
    }

    PMLParametersSet <- PMLParametersSets[[PMLParametersSetIndex]]
    if (PMLParametersSet$Type != "Custom") {
      OmegaNamesCurrentSet <-
        .gather_ClassFields(
          ParmList = PMLParametersSet,
          ClassName = "Omega",
          FieldName = "Name",
          ClassFields = c(),
          Unique = FALSE
        )
    } else {
      OmegaNamesCurrentSet <-
        unique(names(PMLParametersSet$CustomRanefs),
               .gather_ClassFields(
                 ParmList = PMLParametersSet,
                 ClassName = "Omega",
                 FieldName = "Name",
                 ClassFields = c(),
                 Unique = FALSE
               ))
    }

    for (OmegaSearchBlock in OmegaSearchBlocks) {
      OmegaSearchBlockPresented <-
        OmegaSearchBlock[OmegaSearchBlock %in% OmegaNamesCurrentSet]
      if (length(OmegaSearchBlockPresented) <= 1) {
        next
      }

      CurrentBlockText <-
        paste(OmegaSearchBlockPresented, collapse = ", ")
      CurrentBlockText <-
        paste0("\t#search_block(", CurrentBlockText, ")")
      OmegaSearchBlocksText[PMLParametersSetIndex] <-
        paste(OmegaSearchBlocksText[PMLParametersSetIndex],
              CurrentBlockText,
              sep = "\n")
    }
  }

  .check_InterpolatedCovariates(PMLParametersSets)

  if (length(PMLParametersSets) == 1) {
    ModelBlock <-
      get_ModelText(PMLParametersSet = PMLParametersSets[[1]],
                    OmegaSearchBlocksText = OmegaSearchBlocksText[1],
                    SpaceName = names(PMLParametersSets)[1])
  } else {
    ModelBlocks <- c()
    for (PMLParametersSetIndex in seq_along(PMLParametersSets)) {
      PMLParametersSet <- PMLParametersSets[[PMLParametersSetIndex]]
      ModelBlocks <-
        c(ModelBlocks,
          get_ModelText(PMLParametersSet = PMLParametersSet,
                        OmegaSearchBlocksText = OmegaSearchBlocksText[PMLParametersSetIndex],
                        SpaceName = names(PMLParametersSets)[PMLParametersSetIndex]))
    }

    ModelBlock <-
      add_TokensNLME(
        TokenName = "PML",
        ListElementName = "PMLText",
        TokenValues = ModelBlocks,
        DoNotChangeTokenListMain = FALSE
      )
  }

  ModelBlock
}

.check_InterpolatedCovariates <- function(PMLParametersSets) {
  for (PMLParametersSet in PMLParametersSets) {
    if (PMLParametersSet$Type != "PK" || !PMLParametersSet$ClosedForm)
      next
    Directions <-
      .gather_ClassFields(
        ParmList = PMLParametersSet,
        ClassName = "Covariate",
        FieldName = "Direction",
        ClassFields = c()
      )

    if ("Interpolate" %in% Directions) {
      CovariatesNames <-
        .gather_ClassFields(
          ParmList = PMLParametersSet,
          ClassName = "Covariate",
          FieldName = "Name",
          ClassFields = c()
        )

      InterpolatedCovariates <-
        CovariatesNames[Directions %in% "Interpolate"]
      warning(
        "Since interpolated covariates are used: ",
        paste(InterpolatedCovariates, collapse = ", "),
        ", Closed forms should be avoided. ",
        "Please use 'ClosedForm = FALSE' in create_ModelPK() call."
      )
      return(FALSE)
    }
  }

  TRUE
}
