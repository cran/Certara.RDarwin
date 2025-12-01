gen_TemplateNLME <- function(Description = "",
                             Author = "",
                             DataFilePath,
                             DataMapping = NULL,
                             ColDef = "",
                             PMLParametersSets,
                             EstArgs = "",
                             SimArgs = "",
                             Tables = list(),
                             AppendixRows = "",
                             OmegaSearchBlocks = list()) {
  stopifnot(length(Description) == 1 & is.character(Description))
  stopifnot(length(Author) == 1 & is.character(Author))
  stopifnot(length(ColDef) == 1 & is.character(ColDef))
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(EstArgs))
  stopifnot(is.character(SimArgs))
  stopifnot(length(AppendixRows) == 1 & is.character(AppendixRows))
  stopifnot(is.list(OmegaSearchBlocks))

  if (!file.exists(DataFilePath)) {
    stop("Data file is not found: ", DataFilePath)
  }

  if (is.Table(Tables)) {
    Tables <- list(Tables)
  } else if (!all(sapply(Tables, is.Table))) {
    stop("Tables argument should be a list of Table class instances.")
  }

  ModelTemplate <- list()
  TokensListMain <- list()

  ModelTemplate["Description"] <-
    paste("##Description:", Description)

  ModelTemplate["Author"] <- paste("##Author:", Author)

  ModelTemplate["DATA"] <-
    paste("##DATA", paste0("{data_dir}/", basename(DataFilePath)))

  if (is.list(DataMapping)) {
    MappingFunction <- "gen_MAPList"
  } else if (any(sapply(PMLParametersSets, function(x)
    x$Type == "Custom"))) {
    MappingFunction <- "gen_MAPCustom"
  } else {
    MappingFunction <- "gen_MAP"
  }

  ModelTemplate["MAP"] <- paste("##MAP",
                                do.call(
                                  MappingFunction,
                                  list(
                                    Mapping = DataMapping,
                                    PMLParametersSets = PMLParametersSets,
                                    DataFilePath = DataFilePath
                                  )
                                ))

  if (nchar(ColDef) > 0) {
    ModelTemplate["COLDEF"] <-
      paste("##COLDEF", ColDef)
  }

  ModelTemplate["MODEL"] <-
    paste("##MODEL",
          get_ModelBlock(PMLParametersSets, OmegaSearchBlocks))

  if (.check_0nzchar(EstArgs)) {
    ModelTemplate["ESTARGS"] <-
      paste("##ESTARGS", EstArgs, sep = "\n", collapse = "\n")
  } else {
    if (.check_0nzchar(SimArgs)) {
      # if something is given in simulation and EstArgs are not
      # specified, keep simulation part only
      ModelTemplate["ESTARGS"] <- NULL
    } else {
      # otherwise use default EstArgs only
      message("Both EstArgs and SimArgs are not specified. ",
              "Default EstArgs will be used.")
      ModelTemplate["ESTARGS"] <- "##ESTARGS"
    }
  }

  if (.check_0nzchar(SimArgs)) {
    ModelTemplate["SIMARGS"] <-
      paste("##SIMARGS", SimArgs, sep = "\n", collapse = "\n")
  } else {
    ModelTemplate["SIMARGS"] <- NULL
  }


  ModelTemplate["TABLES"] <- "##TABLES"
  if (length(Tables) > 0) {
    CovariateNames <-
      unique(.gather_ClassProperties(PMLParametersSets, "Covariate", "Name", c()))
    DosepointNames <-
      unique(.gather_ClassProperties(PMLParametersSets, "Dosepoint", "DosepointName", c()))
    ObservationNames <-
      unique(
        .gather_ClassProperties(PMLParametersSets, "Observation", "ObservationName", c())
      )

    for (TableInstance in Tables) {
      TableOutput <- generate.Table(
        TableInstance,
        PMLParametersSets = PMLParametersSets,
        Observations = ObservationNames,
        Covariates = CovariateNames,
        Dosepoints = DosepointNames,
        IsTimeBasedModel = TRUE
      )
      ModelTemplate["TABLES"] <-
        paste(ModelTemplate["TABLES"], TableOutput, sep = "\n")
    }
  }

  ModelTemplate["TABLES"] <-
    paste(ModelTemplate["TABLES"], AppendixRows, sep = "\n")

  ModelTemplate
}

.gather_ClassFields <-
  function(ParmList,
           ClassName,
           FieldName,
           ClassFields,
           Unique = TRUE) {
    if (!is.list(ParmList)            |
        length(ParmList) == 0)
      return(ClassFields)

    for (iElement in 1:length(ParmList)) {
      if (!is.list(ParmList[[iElement]]))
        next

      if (inherits(ParmList[[iElement]], ClassName)) {
        if (!is.null(ParmList[[iElement]][[FieldName]])) {
          ClassFields <-
            c(ClassFields, ParmList[[iElement]][[FieldName]])
        }

      } else {
        ClassFields <-
          .gather_ClassFields(ParmList[[iElement]], ClassName, FieldName, ClassFields, Unique = Unique)
      }
    }

    if (Unique) {
      unique(ClassFields)
    } else {
      ClassFields
    }
  }
