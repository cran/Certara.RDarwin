#' @noRd
#' @keywords internal NONMEM
print_templateFromJson <- function(JSONtoParse) {
  if (!file.exists(JSONtoParse)) {
    stop("JSON file ",
         JSONtoParse,
         " with data for template building not found")
  }

  if (!requireNamespace("jsonlite")) {
    stop("jsonlite package is not found. R script won't run.")
  }

  parsedJSON <- jsonlite::fromJSON(JSONtoParse, simplifyMatrix = FALSE)

  expectedNames <-
    c(
      "appendix",
      "cov_text",
      "data_mapping",
      "data_path",
      "est_text",
      "mdf",
      "problem",
      "sigma",
      "template_path",
      "tokens_path"
    )
  if (length(setdiff(expectedNames, names(parsedJSON))) > 0) {
    stop("Current names are not presented in the JSON file: ",
         paste0(setdiff(expectedNames, names(parsedJSON)), collapse = ", "))
  }

  TemplateFilePath <- parsedJSON$template_path
  TokensFilePath <- parsedJSON$tokens_path

  ModelDFColumnNames <-
    utils::read.table(text = parsedJSON$mdf[1],
                      sep = ",",
                      stringsAsFactors = FALSE)
  if (length(parsedJSON$mdf) == 1) {
    stop("No ModelDF rows are found in JSON")
  }


  ModelDF <-
    utils::read.table(
      text = parsedJSON$mdf[2:length(parsedJSON$mdf)],
      sep = ",",
      na.strings = c("NA", ""),
      stringsAsFactors = FALSE
    )
  colnames(ModelDF) <- ModelDFColumnNames

  ProblemName <- parsedJSON$problem
  DataFilePath <- parsedJSON$data_path
  SigmasChosen <- list(
    Additive = parsedJSON$sigma$additive,
    Proportional = parsedJSON$sigma$proportional,
    Combined = c(
      PropPart = parsedJSON$sigma$combined$proportional,
      AddPart = parsedJSON$sigma$combined$additive
    )
  )

  if (length(parsedJSON$est_text) == 0) {
    EstimationRow <- ""
  } else {
    EstimationRow <- parsedJSON$est_text
  }

  if (length(parsedJSON$cov_text) == 0) {
    CovarianceRow <- ""
  } else {
    CovarianceRow <- parsedJSON$cov_text
  }

  if (length(parsedJSON$appendix) == 0) {
    AppendixRows <- ""
  } else {
    AppendixRows <- parsedJSON$appendix
  }

  # omega_search is optional
  if (length(parsedJSON$omega_search) == 0) {
    OmegaSearch <- list()
  } else {
    OmegaSearch <- parsedJSON$omega_search
  }

  if (length(parsedJSON$sizes_text) == 0) {
    SizesText <- character()
  } else {
    SizesText <- parsedJSON$sizes_text
  }

  DataMapping <-
    c(
      ID = parsedJSON$data_mapping$fields$ID,
      TIME = parsedJSON$data_mapping$fields$TIME,
      AMT = parsedJSON$data_mapping$fields$AMT,
      DV = parsedJSON$data_mapping$fields$DV,
      RATE = parsedJSON$data_mapping$fields$RATE
    )

  if (length(parsedJSON$data_mapping$covariates) > 0) {
    DataMapping <- c(DataMapping, parsedJSON$data_mapping$covariates)
  }

  if (length(parsedJSON$data_mapping$reserved) > 0) {
    DataMapping <- c(DataMapping, parsedJSON$data_mapping$reserved)
  }

  if (length(DataMapping) == 0) {
    stop("Data mapping is not given.")
  }

  emptyModelTerms <- names(DataMapping) == ""
  names(DataMapping)[emptyModelTerms] <-
    DataMapping[emptyModelTerms]
  columns <- parsedJSON$data_mapping$columns
  mapPositions <- match(DataMapping, columns)
  names(columns)[mapPositions] <- names(DataMapping)
  columns[is.na(names(columns))] <- "DROP"
  names(columns)[is.na(names(columns))] <- "DROP"

  print_TemplateTokens(
    TemplateFilePath = TemplateFilePath,
    TokensFilePath = TokensFilePath,
    ModelDF = ModelDF,
    SizesText = SizesText,
    ProblemName = ProblemName,
    DataMapping = columns,
    DataFilePath = DataFilePath,
    SigmasChosen = SigmasChosen,
    EstimationRow = EstimationRow,
    CovarianceRow = CovarianceRow,
    AppendixRows = AppendixRows,
    OmegaSearch = OmegaSearch
  )

}
