#' Prints NLME metamodel template file and token json file using given options,
#' filepaths and data
#'
#' This function generates and writes the model template and tokens files based
#' on the provided inputs.
#'
#' @param TemplateFilePath TemplateFilePath NLME template file path to be
#'   written (usually txt).
#' @param TokensFilePath json file path to be written (usually json).
#' @param Description A problem name to be outputted in Description section.
#' @param Author The author information for the model to be outputted in Author
#'   section.
#' @param DataFilePath A data file path used by NLME.
#' @param DataMapping Mapping of model terms to data column names, which can be:
#'   - **A named character vector**: Used when `PMLParametersSets` contains
#'   a single parameter set. Maps model terms to data columns.
#'   - **A named list of named character vectors**: Used when `PMLParametersSets`
#'   contains multiple parameter sets. Each element corresponds to a parameter
#'   set, with list names matching the names of `PMLParametersSets`.
#' @param ColDef A character string specifying additional column definitions in
#'   NLME column definition format. See Phoenix NLME documentation for details.
#'   \url{https://onlinehelp.certara.com/phoenix/8.6/index.html#t=Phoenix_UserDocs\%2FPML\%2FColumn_mappings.htm}
#' @param PMLParametersSets A list of PML parameters sets (`PMLModels` class
#'   instance).
#' @param EstArgs Estimation arguments for the model template. Please use
#'   \code{\link{specify_EngineParams}} to specify the arguments passed to NLME.
#' @param SimArgs Simulation arguments for the model template. Please use
#'   \code{\link{specify_SimParams}} to specify the arguments passed to NLME.
#' @param Tables A list of `Table` class instances specifying properties of the
#'   tables to be generated after fitting or during simulation.
#' @param AppendixRows Additional rows to include in the model template appendix
#'   in NLME column definition format. See Phoenix NLME documentation for details.
#'   \url{https://onlinehelp.certara.com/phoenix/8.6/index.html#t=Phoenix_UserDocs\%2FPML\%2FColumn_mappings.htm}
#' @param OmegaSearchBlocks A list of character vectors representing omega names
#'   to try to build block omegas.
#'
#' @details
#' **Mapping Details:**\cr
#'   *Basic Mapping:* Maps a model variable name (e.g., `CObs`) to a column name
#'   in your data file (e.g., `"Concentration"`). \cr
#'   *Shorthand Mapping:* If an element is unnamed, the Model Term is assumed
#'   to be the same as the Data Column Term.
#'   Example: `c(ID = "Subject", "Age", Weight = "WT")` is equivalent to
#'   `c(ID = "Subject", Age = "Age", Weight = "WT")`. \cr
#'   *Multiple ID/Grouping Levels:* Model terms matching the pattern `ID[0-9]?`
#'   (i.e., `ID`, `ID0`, `ID1`, `ID2`, `ID3`, `ID4`), case-insensitive, are
#'   automatically recognized by `Certara.RsNLME` as NLME sort keys/grouping
#'   levels. You can map up to 5 such levels. The function uses these to
#'   structure the model execution.  \cr
#'   *Covariates:* If not explicitly mapped, the function attempts to map them
#'   using data column names that match covariate names in the model.\cr
#'   *Mapping a List:* When using a list, each vector must map terms specific
#'   to its parameter set, and the list length must equal the number of parameter sets.\cr
#'   *Special Terms:*
#'   - Terms `<DosepointName>_Duration` or `<DosepointName>_Rate`
#'   could be used to map rate/duration columns for the corresponding dosepoints
#'   (e.g., `A1_Rate = "InfRate"`). Term `<ObservationName>BQL` could be used
#'   to map a BQL flag column for the corresponding observation
#'   (e.g., `CObsBQL = "ConcBQL"`). \cr
#'   - The generic `AMT` term can be used to map the dose amount column;
#'   the function will automatically associate it with the primary absorption
#'   compartment (e.g., `A1` for zero-order/bolus, `Aa` for first-order)
#'   for each parameter set. \cr
#'   - Generic `Duration` or `Rate` terms can be mapped (e.g., `Rate = "InfRate"`);
#'   the function will associate them with the dose mapped via `AMT`. If a specific
#'   mapping like `A1_Rate` exists, it overrides the generic `Rate` mapping
#'   for that dosepoint (`A1`).\cr
#'
#' @examples
#' # Write model template and tokens files
#' PMLParametersSets <- create_ModelPK(CompartmentsNumber = c(1,2))
#' # write test data frame
#' TempFolder <- tempdir()
#' TemplateFilePath <- file.path(TempFolder, "template.txt")
#' TokensFilePath <- file.path(TempFolder, "tokens.json")
#' DataFilePath <- file.path(TempFolder, "Data.csv")
#' # Ensure data file has columns matching the DataMapping values
#' write.csv(data.frame(Subject = 'id_1', # Column for ID
#'                      StudyDay = 1,      # Column for ID1
#'                      time = 0,         # Column for time
#'                      DoseAmt = 100,     # Column for AMT
#'                      Concentration = 10.5, # Column for CObs
#'                      SubjectAge = 45,   # Column for Age
#'                      Weight = 70,       # Column for Weight
#'                      ConcBQL = 0),      # Column for CObsBQL
#'                      DataFilePath, row.names = FALSE) # Use row.names=FALSE
#'
#' write_ModelTemplateTokens(
#'   TemplateFilePath = TemplateFilePath,
#'   TokensFilePath = TokensFilePath,
#'   Description = "1-2Cpts try with Multi-ID and Shorthand",
#'   Author = "Certara",
#'   DataFilePath = DataFilePath,
#'   DataMapping = c(ID = "Subject",   # Map ID model term to Subject column
#'                   ID1 = "StudyDay", # Map ID1 model term to StudyDay column
#'                   time = "time",    # Map time model term to time column
#'                   CObs = "Concentration", # Map CObs to Concentration
#'                   AMT = "DoseAmt",  # Map generic AMT to DoseAmt
#'                   "SubjectAge",     # Shorthand: Map Age model term to SubjectAge column
#'                   Weight = "Weight",# Map Weight model term to Weight column
#'                   CObsBQL = "ConcBQL"), # Map BQL flag
#'   ColDef = "",
#'   PMLParametersSets = PMLParametersSets,
#'   EstArgs = specify_EngineParams(method = "QRPEM"),
#'   SimArgs = specify_SimParams(numReplicates = 1000L),
#'   Tables = list(Table(Name = "simtable1.csv",
#'                       KeepSource = TRUE,
#'                       VariablesList = "C",
#'                       ForSimulation = TRUE)),
#'   OmegaSearchBlocks = list(c("nCl", "nV"), c("nCl2", "nV2")))
#'
#' # Multiple parameter sets
#' PMLParametersSets <- create_ModelPK(Absorption =c("Intravenous", "Weibull"))
#' DataMapping <- list(
#'   c(ID = "Subject", time = "time", Aa = "DoseAmt", CObs = "Concentration"),
#'   c(ID = "Subject", time = "time", A1 = "DoseAmt", CObs = "Concentration")
#' )
#'
#' names(DataMapping) <- names(PMLParametersSets)
#'
#' write_ModelTemplateTokens(
#'   TemplateFilePath = TemplateFilePath,
#'   TokensFilePath = TokensFilePath,
#'   Description = "1 Cpt Weibull and First-Order",
#'   Author = "Certara",
#'   DataFilePath = DataFilePath,
#'   DataMapping = DataMapping,
#'   PMLParametersSets = PMLParametersSets)
#'
#' @seealso [specify_EngineParams()], [specify_SimParams()], [Table()]
#' @return A list containing statements written to template and tokens files.
#' @export
write_ModelTemplateTokens <-
  function(TemplateFilePath = "template.txt",
           TokensFilePath = "tokens.json",
           Description = "",
           Author = "",
           DataFilePath,
           DataMapping = NULL,
           ColDef = "",
           PMLParametersSets,
           EstArgs = specify_EngineParams(),
           SimArgs = "",
           Tables = list(),
           AppendixRows = "",
           OmegaSearchBlocks = list()) {
    on.exit(clean_TokensEnv(e = TokensEnv))

    Template <- gen_TemplateNLME(
      Description = Description,
      Author = Author,
      DataFilePath = DataFilePath,
      DataMapping = DataMapping,
      ColDef = ColDef,
      PMLParametersSets = PMLParametersSets,
      EstArgs = EstArgs,
      SimArgs = SimArgs,
      Tables = Tables,
      AppendixRows = AppendixRows,
      OmegaSearchBlocks = OmegaSearchBlocks
    )

    cat(unlist(Template),
        file = TemplateFilePath,
        sep = "\n")

    if (exists("TokensList", envir = TokensEnv)) {
      TokensList <- get("TokensList", envir = TokensEnv)
    } else {
      TokensList <- NULL
    }

    prettyJSON <-
      jsonlite::prettify(jsonlite::toJSON(
        TokensList,
        pretty = TRUE,
        digits = NA
      ))

    cat(prettyJSON,
        file = TokensFilePath, sep = "\n")

    message("information stored in ",
            TemplateFilePath,
            " and ",
            TokensFilePath)

    list(TokensList = TokensList, Template = Template)
  }
