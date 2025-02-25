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
#' @param DataMapping A named vector ModelTerm = DataTerm for the used data
#'   file.
#' @param ColDef A character string specifying additional column definitions in
#'   NLME column definition format. See
#'   \url{https://onlinehelp.certara.com/phoenix/8.4/index.html#t=Phoenix_UserDocs\%2FPML\%2FColumn_mappings.htm}
#' @param PMLParametersSets A list of PML parameters sets (`PMLModels` class
#'   instance).
#' @param EstArgs Estimation arguments for the model template. Please use
#'   \code{\link{specify_EngineParams}} to specify the arguments passed to NLME.
#' @param SimArgs Simulation arguments for the model template. Please use
#'   \code{\link{specify_SimParams}} to specify the arguments passed to NLME.
#' @param Tables A list of `Table` class instances specifying properties of the
#'   tables to be generated after fitting or during simulation.
#' @param AppendixRows Additional rows to include in the model template appendix
#'   in NLME column definition format. See
#'   \url{https://onlinehelp.certara.com/phoenix/8.4/index.html#t=Phoenix_UserDocs\%2FPML\%2FColumn_mappings.htm}
#' @param OmegaSearchBlocks A list of character vectors representing omega names
#'   to try to build block omegas.
#'
#' @details Terms `<DosepointName>_Duration` or `<DosepointName>_Rate` could be
#' used to map rate/duration columns for the current dosepoints. Term
#' `<ObservationName>BQL` could be used to map BQL flag for the current
#' observation. `AMT` term could be used to map different main dosepoints (i.e.
#' A1 will be mapped for `Gamma`, Aa for `First-Order` absorption etc.) If `AMT`
#' term is used, additional terms `Duration` or `Rate` could be used; current
#' function will map it to the main dosepoint of each Parameter set. But it is
#' possible to map duration/rate for some dosepoint directly using terms
#' `<DosepointName>_Duration` or `<DosepointName>_Rate`, it will override
#' `Duration` or `Rate` terms mapping for current dosepoint.
#'
#' @examples
#' # Write model template and tokens files
#' PMLParametersSets <- create_ModelPK(CompartmentsNumber = c(1,2))
#' # write test data frame
#' TempFolder <- tempdir()
#' TemplateFilePath <- file.path(TempFolder, "template.txt")
#' TokensFilePath <- file.path(TempFolder, "tokens.json")
#' DataFilePath <- file.path(TempFolder, "Data.csv")
#' write.csv(data.frame(id = 'id',
#'                      time = 'time',
#'                      AMT = 'AMT',
#'                      Conc = 'Conc',
#'                      age = 'age',
#'                      Weight = 'Weight',
#'                      CObsBQL = 'CObsBQL'),
#'                      DataFilePath)
#' write_ModelTemplateTokens(TemplateFilePath = TemplateFilePath,
#'                           TokensFilePath = TokensFilePath,
#'                           Description = "1-2Cpts try",
#'                           Author = "Certara",
#'                           DataFilePath = DataFilePath,
#'                           DataMapping = c(ID = "id",
#'                                           time = "time",
#'                                           CObs = "Conc",
#'                                           AMT = "AMT",
#'                                           "age"),
#'                           ColDef = "",
#'                           PMLParametersSets = PMLParametersSets,
#'                           EstArgs = specify_EngineParams(method = "QRPEM"),
#'                           SimArgs = specify_SimParams(numReplicates = 1000L),
#'                           Tables = list(Table(Name = "simtable1.csv",
#'                                               KeepSource = TRUE,
#'                                               VariablesList = "C",
#'                                               ForSimulation = TRUE)),
#'                           OmegaSearchBlocks = list(c("nCl", "nV"), c("nCl2", "nV2")))
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
