#' Get Model Text
#'
#' This function generates the model text based on the provided PML parameters
#' set.
#'
#' @param PMLParametersSet The PML parameters set.
#' @param OmegaSearchBlocksText String about Omega search to be included in the
#'   end of the model file.
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
get_ModelText <- function(PMLParametersSet, OmegaSearchBlocksText) {
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
