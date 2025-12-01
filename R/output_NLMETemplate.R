#' @title Generate NLME Model Template from JSON Specification
#' @description
#' This function serves as a command-line argument parser that calls an
#' internal function (`output_NLMETemplateInternal`) to generate NLME (Non-Linear
#' Mixed Effects) model templates. It takes a series of "key=value" pairs,
#' processes a detailed JSON model specification, and outputs a model template
#' file and a corresponding tokens file, which can be used for model fitting.
#'
#' @param args A character vector where each element is a string in the format
#'   "key=value". The function parses these strings to set up the model
#'   generation process. The following keys are expected:
#'   \itemize{
#'     \item \code{model_setup}: The file path to the input JSON file that defines the NLME model structure, parameters, covariates, and error models.
#'     \item \code{template_path}: The destination file path for the output model template.
#'     \item \code{tokens_path}: The destination file path for the output JSON tokens file.
#'     \item \code{data_path}: The file path to the dataset that will be used with the model.
#'     \item \code{author}: A string specifying the name of the author to be embedded in the output files.
#'     \item \code{description}: A string providing a description of the model to be embedded in the output files.
#'   }
#'
#' @return
#' This function does not return a value to the R environment. It is executed
#' for its side effect of writing two files:
#' \enumerate{
#'   \item A model template file to the location specified by \code{template_path}.
#'   \item A tokens JSON file to the location specified by \code{tokens_path}.
#' }
#'
#' @export
#' @seealso
#' The core logic is handled by the internal function `output_NLMETemplateInternal`.
#' The final file writing is performed by `write_ModelTemplateTokens`.
#'
#' @examples
#' \dontrun{
#' # Example of the arguments that would be passed to the function.
#' # In a real scenario, these might come from a command-line call.
#'
#' arguments <- c(
#'   "model_setup=./model_definition.json",
#'   "template_path=./template.txt",
#'   "tokens_path=./tokens.json",
#'   "data_path=./pk_data.csv",
#'   "author=Jane Doe",
#'   "description=2-compartment PK model with first-order absorption and linear elimination"
#' )
#'
#' # Execute the function with the defined arguments
#' output_NLMETemplate(args = arguments)
#'
#' }
#'
output_NLMETemplate <-
  function(args) {
    # Extract the values and assign the names
    key_value_pairs <- strsplit(args, "=", fixed = TRUE)
    key_value_pairs <-
      key_value_pairs[sapply(key_value_pairs, length) == 2]
    values <- sapply(key_value_pairs, `[`, 2)
    names(values) <- sapply(key_value_pairs, `[`, 1)
    args_list <- as.list(values)

    # Extract variables from the named list
    model_setup   <- args_list$model_setup
    template_path <- args_list$template_path
    tokens_path   <- args_list$tokens_path
    data_path     <- args_list$data_path
    author        <- args_list$author
    description   <- args_list$description

    if (!is.null(model_setup)) {
      model_setup <- gsub("\\\\", "/", model_setup)
    }
    if (!is.null(template_path)) {
      template_path <- gsub("\\\\", "/", template_path)
    }
    if (!is.null(tokens_path)) {
      tokens_path <- gsub("\\\\", "/", tokens_path)
    }
    if (!is.null(data_path)) {
      data_path <- gsub("\\\\", "/", data_path)
    }

    if (missing(description) ||
        length(description) == 0) {
      description = ""
    }

    if (missing(author) ||
        length(author) == 0) {
      author = ""
    }

    # Now proceed with the function logic
    message(paste("Model setup:", model_setup))
    message(paste("Template path:", template_path))
    message(paste("Tokens path:", tokens_path))
    message(paste("Data path:", data_path))
    message(paste("Author:", author))
    message(paste("Description:", description))
    output_NLMETemplateInternal(
      model_setup = model_setup,
      template_path = template_path,
      tokens_path = tokens_path,
      data_path = data_path,
      author = author,
      description = description
    )
  }

output_NLMETemplateInternal <- function(model_setup,
                                        template_path,
                                        tokens_path,
                                        data_path,
                                        author,
                                        description) {
  NLMEDarwinOptions <-
    jsonlite::fromJSON(model_setup)

  ExpectedModelSetupNames <-
    c(
      "Type",
      "Infusion",
      "ElimCptPresence",
      "ElimCptReset",
      "Covariates",
      "Mapping",
      "Parameterization",
      "Administration",
      "Compartments",
      "Observations",
      "StParms",
      "CovariateEffects",
      "FixedEffects",
      "RandomEffects"
    )

  ModelSetup <- NLMEDarwinOptions$ModelSetup
  ModelNames <- names(ModelSetup)
  MatchedModelSetupNames <-
    match(ExpectedModelSetupNames, ModelNames)
  if (any(is.na(MatchedModelSetupNames))) {
    stop(
      "Input JSON file does not contain the following expected ModelSetup names:\n",
      paste(ExpectedModelSetupNames[is.na(MatchedModelSetupNames)], collapse = ", "),
      "\nCurrent set of names:\n",
      paste(ModelNames, collapse = ", ")
    )
  }

  if (ModelSetup$Type != "PK") {
    stop("Only PK type of model spaces are supported.")
  }

  # need to build the arguments for create_ModelPK
  GuessCompartments <-
    as.numeric(substr(names(ModelSetup$Compartments), 1, 1))
  if (length(GuessCompartments) == 0 ||
      !all(GuessCompartments %in% c(1, 2, 3))) {
    stop(
      "Compartments should contain compartments 1, 2 or 3.",
      " Current compartments: ",
      paste(names(ModelSetup$Compartments), collapse = ", ")
    )
  }

  names(GuessCompartments) <- names(ModelSetup$Compartments)

  # merge Compartments with Absorption
  if (ModelSetup$Administration == "Intravenous") {
    ModelArgsDF <- data.frame(CompartmentsNumber = GuessCompartments,
                              Absorption = "Intravenous")
  } else {
    df_list <- lapply(names(GuessCompartments), function(CompName) {
      data.frame(
        CompartmentsNumber = GuessCompartments[[CompName]],
        Absorption = ModelSetup$Compartments[[CompName]]$Absorption
      )
    })
    ModelArgsDF <- do.call(rbind, df_list)
  }

  # merge with Parameterization
  ModelArgsDF$Parameterization <- ModelSetup$Parameterization

  # merge with Saturation
  ModelArgsDF <- .add_NewTermDF(
    ModelArgsDF = ModelArgsDF,
    GuessCompartments = GuessCompartments,
    ModelSetup = ModelSetup,
    SetupTerm = "Elimination",
    PackageTerm = "Elimination"
  )
  ModelArgsDF$Saturation <- ModelArgsDF$Elimination != "Linear"

  # merge with EliminationCpt
  ModelArgsDF$EliminationCpt <- ModelSetup$ElimCptPresence

  # merge with FractionExcreted
  # Split the data frame into a list by compartment number
  ModelArgsDF_list <-
    split(ModelArgsDF, ModelArgsDF$CompartmentsNumber)

  processed_list <-
    lapply(names(ModelArgsDF_list), function(comp_num_char) {
      CurrentDF <- ModelArgsDF_list[[comp_num_char]]
      CompName <-
        names(GuessCompartments)[GuessCompartments == as.numeric(comp_num_char)]

      FractionExcreteds <-
        ModelSetup$Compartments[[CompName]][["FractionExcreted"]]

      # If no FractionExcreted term, set to FALSE and return
      if (length(FractionExcreteds) == 0) {
        CurrentDF$FractionExcreted <- FALSE
        return(CurrentDF)
      }

      # validation
      FractionExcreteds <- unique(FractionExcreteds)
      if (length(FractionExcreteds) > 1) {
        stop("For ",
             CompName,
             " models FractionExcreted should be a single value...")
      }
      if (FractionExcreteds == "Present") {
        FractionExcreteds <- TRUE
      } else if (FractionExcreteds == "Searched") {
        FractionExcreteds <- c(TRUE, FALSE)
      } else {
        stop("FractionExcreted for ",
             CompName,
             " should be 'Present' or 'Searched'...")
      }

      # This replicates the existing rows and assigns the new values
      n_reps <- length(FractionExcreteds)
      expanded_df <-
        CurrentDF[rep(seq_len(nrow(CurrentDF)), each = n_reps),]
      expanded_df$FractionExcreted <- FractionExcreteds

      return(expanded_df)
    })

  # Combine the list of processed data frames back into one
  ModelArgsDF <- do.call(rbind, processed_list)
  rownames(ModelArgsDF) <- NULL

  # merge with ClosedForm
  FOIVOnly <-
    all(ModelArgsDF$Absorption %in% c("First-Order", "Intravenous"))
  EliminationCpt <- any(ModelArgsDF$EliminationCpt)
  InterpolatedCovs <-
    any(sapply(ModelSetup$Covariates$Continuous, function(x) {
      is.list(x) &&
        "Direction" %in% names(x) && x$Direction == "Interpolate"
    }))

  ClosedForm  <- FOIVOnly && !EliminationCpt && !InterpolatedCovs


  ModelSpaces <- create_ModelPK(
    CompartmentsNumber = ModelArgsDF$CompartmentsNumber,
    Absorption = ModelArgsDF$Absorption,
    Parameterization = ModelArgsDF$Parameterization,
    Saturation = ModelArgsDF$Saturation,
    EliminationCpt = ModelArgsDF$EliminationCpt,
    FractionExcreted = ModelArgsDF$FractionExcreted,
    ByVector = TRUE,
    ClosedForm = ClosedForm
  )

  SpacesNames <- names(ModelSpaces)
  # Tlag Bioavail
  for (CompName in names(GuessCompartments)) {
    CompNubmber <- GuessCompartments[[CompName]]
    SpacesNamesToModify <-
      SpacesNames[grepl(paste0("^", ModelSetup$Type, CompNubmber),
                        SpacesNames)]
    if (length(SpacesNamesToModify) == 0)
      next

    # Tlag
    if (!is.null(ModelSetup$Compartments[[CompName]]$Tlag)) {
      ModelSpaces <-
        add_StParm(
          ModelSpaces,
          StParmName = "Tlag",
          State = ModelSetup$Compartments[[CompName]]$Tlag,
          PMLStructures = SpacesNamesToModify,
          DosepointArgName = "tlag"
        )
    }

    if (!is.null(ModelSetup$Compartments[[CompName]]$Bioavailability)) {
      ModelSpaces <-
        add_StParm(
          ModelSpaces,
          StParmName = "F",
          State = ModelSetup$Compartments[[CompName]]$Bioavailability,
          PMLStructures = SpacesNamesToModify,
          DosepointArgName = "bioavail"
        )
    }
  }

  # Mapping (need it for BQL catching)
  # make time lower case
  names(ModelSetup$Mapping)[names(ModelSetup$Mapping) == "Time"] <-
    "time"
  Mapping <- unlist(ModelSetup$Mapping)

  # Observations
  for (ObservationName in names(ModelSetup$Observations)) {
    # BQL from mapping
    BQL <- (paste0(ObservationName, "BQL") %in% names(Mapping))

    # BQL from static
    BQLStatic <-
      !is.null(ModelSetup$Observations[[ObservationName]]$BQLStatic) &&
      ModelSetup$Observations[[ObservationName]]$BQLStatic
    if (BQLStatic)
      BQL <- TRUE

    # use a value only if BQLStatic == TRUE
    BQLValue <- NA
    if (BQLStatic) {
      if (is.null(ModelSetup$Observations[[ObservationName]]$BQLValue)) {
        warning(
          "BQLValue is not set for observation '",
          ObservationName,
          "', but BQLStatic == TRUE. Static BQL won't be used"
        )
      } else {
        BQLValue <- ModelSetup$Observations[[ObservationName]]$BQLValue
      }
    }

    if (ObservationName == "A0Obs" &&
        ModelSetup$ElimCptPresence &&
        ModelSetup$ElimCptReset) {
      ResetObs <- TRUE
    } else {
      ResetObs <- FALSE
    }

    Error <- ModelSetup$Observations[[ObservationName]]$Error
    SigmasArgs <- list()
    for (SigmaType in names(Error)) {
      if (is.null(Error[[SigmaType]]))
        next
      SigmaData <- Error[[SigmaType]]
      if (SigmaType %in% c("AdditiveMultiplicative", "MixRatio")) {
        SigmasArgs[[SigmaType]] <-
          list(PropPart = 1, AddPart = SigmaData$Initial)
      } else if (SigmaType == "Power") {
        SigmasArgs[[SigmaType]] <-
          list(PowerPart = SigmaData$Power,
               StdevPart = SigmaData$Initial)
      } else if (SigmaType == "Multiplicative") {
        SigmasArgs[["Proportional"]] <- SigmaData$Initial
      } else {
        SigmasArgs[[SigmaType]] <- SigmaData$Initial
      }
    }

    if (!"Proportional" %in% names(SigmasArgs)) {
      # reset it from default
      SigmasArgs[["Proportional"]] <- 0
    }

    SigmasArgs[["ObservationName"]] <- ObservationName
    SigmasChosen <- do.call("Sigmas", SigmasArgs)

    Frozen <- sapply(Error, function(x)
      isTRUE(x$Freeze))
    names(Frozen)[names(Frozen) == "Multiplicative"] <- "Proportional"

    ModelSpaces <-
      modify_Observation(
        ModelSpaces,
        ObservationName = ObservationName,
        SigmasChosen = SigmasChosen,
        BQL = BQL,
        BQLValue = BQLValue,
        Frozen = Frozen,
        ResetObs = ResetObs
      )
  }

  # StParm and obligatory thetas and omegas
  for (StParmName in names(ModelSetup$StParms)) {
    if (is.null(ModelSetup$StParms[[StParmName]]))
      next
    StParmProperties <- ModelSetup$StParms[[StParmName]]

    ThetaName <- paste0("tv", StParmName)
    if (length(ModelSetup$FixedEffects[[ThetaName]]$Freeze) == 0) {
      ThetaFrozen <- FALSE
    } else {
      ThetaFrozen <- ModelSetup$FixedEffects[[ThetaName]]$Freeze
    }

    InitialEstimates <-
      .build_InitialEstimate(ThetaInstance = ModelSetup$FixedEffects[[ThetaName]])

    OmegaName <- paste0("n", StParmName)
    if (length(ModelSetup$FixedEffects[[OmegaName]]$Freeze) == 0) {
      OmegaFrozen <- FALSE
    } else {
      OmegaFrozen <- ModelSetup$FixedEffects[[OmegaName]]$Freeze
    }

    # adding StParm to the ModelSpaces
    ModelSpaces <-
      .addmodify_StParm(
        PMLParametersSets = ModelSpaces,
        StParmName = StParmName,
        Type = ModelSetup$StParms[[StParmName]]$Type,
        # State = State, keep as is
        ThetaStParm = Theta(
          Name = ThetaName,
          InitialEstimates = InitialEstimates,
          Frozen = ThetaFrozen
        ),
        OmegaStParm = Omega(
          Name = OmegaName,
          InitialOmega = ModelSetup$RandomEffects[[OmegaName]]$Initial,
          Frozen = OmegaFrozen,
          State = ModelSetup$StParms[[StParmName]]$OmegaState
        ),
        DosepointArgName = NULL,
        # we don't need to modify it
        PMLStructures = NULL,
        Modify = TRUE
      )
  }

  # Covariates
  ## Continuous
  for (ContCovName in names(ModelSetup$Covariates$Continuous)) {
    Mapping[ContCovName] <- ContCovName
    ContCov <- ModelSetup$Covariates$Continuous[[ContCovName]]
    for (StParmName in names(ModelSetup$CovariateEffects)) {
      if (is.null(ModelSetup$CovariateEffects[[StParmName]]) ||
          is.null(ModelSetup$CovariateEffects[[StParmName]][[ContCovName]]) ||
          !ModelSetup$CovariateEffects[[StParmName]][[ContCovName]] %in% c("Present", "Searched")) {
        next
      }

      ModelSpaces <-
        add_Covariate(
          ModelSpaces,
          Name = ContCovName,
          Type = "Continuous",
          StParmNames = StParmName,
          State = ModelSetup$CovariateEffects[[StParmName]][[ContCovName]],
          Direction = ContCov$Direction,
          Center = ContCov$Center
        )
    }
  }

  ## Categorical
  for (CatCovName in names(ModelSetup$Covariates$Categorical)) {
    Mapping[CatCovName] <- CatCovName
    CatCov <- ModelSetup$Covariates$Categorical[[CatCovName]]
    for (StParmName in names(ModelSetup$CovariateEffects)) {
      if (is.null(ModelSetup$CovariateEffects[[StParmName]]) ||
          is.null(ModelSetup$CovariateEffects[[StParmName]][[CatCovName]]) ||
          !ModelSetup$CovariateEffects[[StParmName]][[CatCovName]] %in% c("Present", "Searched")) {
        next
      }

      RefCategory <- CatCov$Center

      ModelSpaces <-
        add_Covariate(
          ModelSpaces,
          Name = CatCovName,
          Type = "Categorical",
          StParmNames = StParmName,
          State = ModelSetup$CovariateEffects[[StParmName]][[CatCovName]],
          Direction = CatCov$Direction,
          Categories = c(RefCategory, CatCov$Categories[CatCov$Categories != RefCategory])
        )
    }
  }

  ## Occasion
  OccasionOmegasToModify <- c()
  for (OccCovName in names(ModelSetup$Covariates$Occasional)) {
    Mapping[OccCovName] <- OccCovName
    OccCov <- ModelSetup$Covariates$Occasional[[OccCovName]]
    for (StParmName in names(ModelSetup$CovariateEffects)) {
      if (is.null(ModelSetup$CovariateEffects[[StParmName]]) ||
          is.null(ModelSetup$CovariateEffects[[StParmName]][[OccCovName]]) ||
          !ModelSetup$CovariateEffects[[StParmName]][[OccCovName]] %in% c("Present", "Searched")) {
        next
      }

      ModelSpaces <-
        add_Covariate(
          ModelSpaces,
          Name = OccCovName,
          Type = "Occasion",
          StParmNames = StParmName,
          State = ModelSetup$CovariateEffects[[StParmName]][[OccCovName]],
          Direction = OccCov$Direction,
          Categories = OccCov$Categories
        )

      OccasionOmegasToModify <-
        c(
          OccasionOmegasToModify,
          paste0("n", StParmName, OccCovName, "x", OccCov$Categories[1])
        )
    }
  }

  # these thetas are already modified
  ThetaNamesChanged <- paste0("tv", names(ModelSetup$StParms))
  for (ThetaName in names(ModelSetup$FixedEffects)) {
    if (ThetaName %in% ThetaNamesChanged)
      next

    InitialEstimates <-
      .build_InitialEstimate(ThetaInstance = ModelSetup$FixedEffects[[ThetaName]])

    ModelSpaces <-
      modify_Theta(
        ModelSpaces,
        Name = ThetaName,
        InitialEstimates = InitialEstimates,
        Frozen = ModelSetup$FixedEffects[[ThetaName]]$Freeze
      )
  }

  # only the first occasion omegas are modified
  for (OmegaName in OccasionOmegasToModify) {
    ModelSpaces <-
      modify_Omega(
        ModelSpaces,
        Name = OmegaName,
        InitialOmega = ModelSetup$RandomEffects[[OmegaName]]$Initial,
        Frozen = ModelSetup$RandomEffects[[OmegaName]]$Freeze
      )
  }

  write_ModelTemplateTokens(
    TemplateFilePath = template_path,
    TokensFilePath = tokens_path,
    Description = description,
    Author = author,
    DataFilePath = data_path,
    DataMapping = Mapping,
    PMLParametersSets = ModelSpaces,
    EstArgs = NLMEDarwinOptions$EngineSetup$ESTARGS,
    SimArgs = NLMEDarwinOptions$EngineSetup$SIMARGS,
    ColDef = NLMEDarwinOptions$EngineSetup$COLDEFS
  )
}

.add_NewTermDF <-
  function(ModelArgsDF,
           GuessCompartments,
           ModelSetup,
           SetupTerm,
           PackageTerm) {
    ModelArgsDF_list <-
      split(ModelArgsDF, ModelArgsDF$CompartmentsNumber)

    # Use lapply to iterate over the list of data frames
    processed_list <-
      lapply(names(ModelArgsDF_list), function(comp_num_char) {
        CurrentCompartmentDF <- ModelArgsDF_list[[comp_num_char]]
        CompName <-
          names(GuessCompartments)[GuessCompartments == as.numeric(comp_num_char)]

        SetupCurCompTerms <-
          ModelSetup$Compartments[[CompName]][[SetupTerm]]

        if (length(SetupCurCompTerms) < 1) {
          stop("For the models ",
               CompName,
               " ",
               SetupTerm,
               " is not specified.")
        }

        # Efficiently expand the data frame for the current group by replicating rows
        # and assigning the new term values.
        expanded_df <-
          CurrentCompartmentDF[rep(seq_len(nrow(CurrentCompartmentDF)), each = length(SetupCurCompTerms)), ]
        expanded_df[[PackageTerm]] <- SetupCurCompTerms

        return(expanded_df)
      })

    # Combine the processed list of data frames back into a single one
    final_df <- do.call(rbind, processed_list)
    rownames(final_df) <- NULL # Clean up row names

    return(final_df)
  }

.build_InitialEstimate <- function(ThetaInstance) {
  if (length(ThetaInstance$Freeze) == 0) {
    ThetaFrozen <- FALSE
  } else {
    ThetaFrozen <- ThetaInstance$Freeze
  }

  if (is.null(ThetaInstance$LowerBound)) {
    Lower <- "-Inf"
  } else {
    Lower <- ThetaInstance$LowerBound
  }

  if (is.null(ThetaInstance$UpperBound)) {
    Upper <- "Inf"
  } else {
    Upper <- ThetaInstance$UpperBound
  }

  Initials <-
    data.frame(Lower = Lower,
               Initial = ThetaInstance$Initial,
               Upper = Upper)

  RowVectors <-
    lapply(split(Initials, seq(nrow(Initials))), function(row)
      as.numeric(row))

  InitialEstimates <-
    do.call(InitialEstimate,
            as.list(RowVectors))

  InitialEstimates
}
