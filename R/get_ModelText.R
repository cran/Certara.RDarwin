#' Get Model Text
#'
#' This function generates the model text based on the provided PML parameters
#' set.
#'
#' @param PMLParametersSet The PML parameters set.
#' @param OmegaSearchBlocksText String about Omega search to be included in the
#'   end of the model file.
#' @param SpaceName The name of PML parameters set.
#'
#' @return The generated model text as a character string.
#'
#' @examples
#' PMLParametersSets <-
#'   create_ModelPK(
#'     CompartmentsNumber = c(2),
#'     Absorption = c("First-Order"),
#'     Parameterization = c("Clearance"),
#'     Saturation = c(TRUE),
#'     EliminationCpt = c(FALSE),
#'     ResetObs = c(FALSE))
#'
#' modelText <- get_ModelText(PMLParametersSets[[1]])
#'
#' @noRd
#' @keywords internal NLME
get_ModelText <-
  function(PMLParametersSet,
           OmegaSearchBlocksText,
           SpaceName) {
    # check for StParms uniqueness
    StParmNames <-
      .gather_ClassProperties(PMLParametersSet,
                              "StParm",
                              "StParmName",
                              c())

    StParmStates <-
      .gather_ClassProperties(PMLParametersSet,
                              "StParm",
                              "State",
                              c())
    StParmNames <- StParmNames[StParmStates != "None"]
    if (any(duplicated(StParmNames))) {
      warning(
        "Please check the StParm names in the PMLParametersSet ",
        SpaceName,
        "\nThe following are duplicated:\n",
        paste(StParmNames[duplicated(StParmNames)], collapse = ", "),
        "\nThis may cause problems when generating the model text.",
        call. = FALSE
      )
    }

    ModelText <- "test() {"

    ModelText <- paste(ModelText,
                       PMLParametersSet$PMLCode,
                       sep = "\n\t")

    # Dosepoints
    for (DosepointInstance in PMLParametersSet$MainDosepoint) {
      ModelText <- paste(ModelText,
                         output(DosepointInstance),
                         sep = "\n\t")
    }

    for (DosepointInstance in PMLParametersSet$SecondaryDosepoints) {
      ModelText <- paste(ModelText,
                         output(DosepointInstance),
                         sep = "\n\t")
    }

    ExpressionsAvailable <-
      .gather_ClassProperties(PMLParametersSet,
                              "Expression",
                              "ExpressionText",
                              c())

    ExpressionsStates <-
      .gather_ClassProperties(PMLParametersSet,
                              "Expression",
                              "State",
                              c())

    ExpressionsAvailable <-
      ExpressionsAvailable[ExpressionsStates != "None"]

    if (length(ExpressionsAvailable) > 0) {
      .check_DosepointExpArgs(ModelText, StParmNames, SpaceName)
    }

    # Observations and error models
    for (ObservationInstance in PMLParametersSet$Observations) {
      ModelText <- paste(ModelText,
                         output(ObservationInstance),
                         sep = "\n\t")

    }

    CovariatesOutput <-
      .get_ClassOutput(PMLParametersSet, "Covariate", character(0))

    ModelText <- paste(ModelText,
                       paste(unique(CovariatesOutput), collapse = "\n\t"),
                       sep = "\n\t")

    # Structural parameters
    for (StParmInstance in PMLParametersSet$StParms) {
      ModelText <- paste(ModelText,
                         output(StParmInstance),
                         sep = "\n\t")

    }

    ModelText <- paste(ModelText,
                       OmegaSearchBlocksText,
                       "}",
                       sep = "\n")

    ModelText
  }

.get_ClassOutput <- function(List, ClassName, ClassOutput) {
  if (!is.list(List)            |
      length(List) == 0         |
      !.check_0nzchar(ClassName))
    return(ClassOutput)

  for (iElement in 1:length(List)) {
    if (inherits(List[[iElement]], ClassName)) {
      ClassOutput <- c(ClassOutput, output(List[[iElement]]))
    }

    if (is.list(List[[iElement]])) {
      ClassOutput <-
        .get_ClassOutput(List[[iElement]], ClassName, ClassOutput)
    }
  }

  ClassOutput
}


.check_DosepointExpArgs <- function(ModelText, StParmNames, SpaceName) {
  DosepointsLists <-
    .parse_CustomStatements(CustomCodeToSearch = ModelText, Statement = "dosepoint")
  NoWarnings <- TRUE
  for (DosepointParsedIndex in seq_along(DosepointsLists)) {
    DosepointParsed <- DosepointsLists[[DosepointParsedIndex]]
    # note that CustomDosepoint has the names of args in Cames case
    for (DosepointArg in c("Tlag", "Bioavail", "Duration", "Rate")) {
      if (is.null(DosepointParsed[[DosepointArg]]))
        next
      # need to cleanup from the tokens
      ExprText <- gsub("\\{.*?\\}", "", DosepointParsed[[DosepointArg]], perl = TRUE)
      VarPattern <- "\\b[a-zA-Z_][a-zA-Z0-9_]*\\b(?!\\s*\\()"
      matches <-
        regmatches(ExprText, gregexpr(VarPattern, ExprText, perl = TRUE))
      PotentialStParms <- unique(unlist(matches))
      NotStParms <-
        PotentialStParms[!PotentialStParms %in% StParmNames]
      if (length(NotStParms) > 0) {
        warning(
          "The following variables found in expression '",
          ExprText,
          "' for the Dosepoint '",
          names(DosepointsLists)[DosepointParsedIndex],
          "' are not StParms:\n",
          paste(NotStParms, collapse = ", "),
          "\nPlease check the dosepoint arguments in the PMLParametersSet ",
          SpaceName,
          "\nThis may cause problems when generating the model text.",
          call. = FALSE
        )
        NoWarnings <- FALSE
      }
    }

  }

  invisible(NoWarnings)
}
