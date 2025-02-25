#' prints NONMEM template file and token json file using given options,
#' filepaths and data
#'
#' @param TemplateFilePath NONMEM template file path to be written (usually txt)
#' @param TokensFilePath json file path to be written (usually json)
#' @param ModelDF a data frame with all thetas, initials, omegas and covariate
#'   relationships
#' @param SizesText an optional character string with Array sizes for NONMEM and PREDPP
#' @param ProblemName a problem name to be outputted in $PROBLEM section
#' @param DataMapping a named vector ModelTerm = DataTerm for the used data file
#' @param DataFilePath a data file path used by NONMEM
#' @param SigmasChosen a named list of sigmas to be used in the template with
#'   given names 'Additive', 'Proportional', 'Combined'. A numeric value greater
#'   than 0 is expected for 'Additive' and 'Proportional'. NA or a value <= 0
#'   means that that type of error should not be used. For 'Combined' type
#'   'PropPart' and 'AddPart' should be specified with the conditions as same as
#'   for 'Additive' and 'Proportional'.
#' @param EstimationRow a text to be inserted in $EST block
#' @param CovarianceRow a text to be inserted in $COV block
#' @param AppendixRows additional rows to be appended to the main template
#' @param OmegaSearch blocks of omegas to search bands. First block (list
#'   element) is for the omegas not to be searched.
#'
#' @noRd
#' @keywords internal NONMEM
print_TemplateTokens <- function(TemplateFilePath = "template.txt",
                                 TokensFilePath = "tokens.json",
                                 ModelDF,
                                 SizesText = character(),
                                 ProblemName,
                                 DataMapping,
                                 DataFilePath,
                                 SigmasChosen = list(
                                   Additive = 0,
                                   Proportional = 0.1,
                                   Combined = c(PropPart = 0.2, AddPart = 1)
                                 ),
                                 EstimationRow = "METHOD = COND INTER NOABORT MAX = 999999",
                                 CovarianceRow = "UNCOND PRINT=E",
                                 AppendixRows = "",
                                 OmegaSearch = list()) {
  results <- gen_TemplateTokens(
    ModelDF = ModelDF,
    SizesText = SizesText,
    ProblemName = ProblemName,
    DataMapping = DataMapping,
    DataFilePath = DataFilePath,
    SigmasChosen = SigmasChosen,
    EstimationRow = EstimationRow,
    CovarianceRow = CovarianceRow,
    AppendixRows = AppendixRows,
    OmegaSearch = OmegaSearch
  )

  cat(unlist(results$ModelTemplate),
      file = TemplateFilePath,
      sep = "\n")

  prettyJSON <-
    jsonlite::prettify(jsonlite::toJSON(
      results$TokensListMain,
      pretty = TRUE,
      digits = NA
    ))
  cat(prettyJSON,
      file = TokensFilePath, sep = "\n")
}
