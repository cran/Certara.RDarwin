#' Get the list of objects describing the PML models by set of PK parameters
#'
#' This function provides the PML (Pharmacometric Modelling Language) PK
#' parameter sets based on the specified options. They are available as a list
#' of specific S3 classes.
#'
#' @param CompartmentsNumber The number of compartments in the model. Supported
#'   embedded models are 1-, 2-, 3-compartments. Default is 1.
#' @param Absorption The absorption type of the model. Supported types are:
#' * `Intravenous` (Default) - Dose is given in the main compartment (A1) directly.
#' * `First-Order` - Dose is absorbed to the main compartment (A1) from the
#'   absorption compartment (Aa) by first-order kinetic.
#' * `Gamma` - Dose is absorbed to A1 by Gamma Distributed delay kinetic.
#' * `Inverse Gaussian` - Dose is absorbed to A1 by Inverse Gaussian Distributed delay kinetic.
#' * `Weibull` - Dose is absorbed to A1 by Weibull Distributed delay kinetic.
#'
#'
#' @param Parameterization The parameterization type. Possible options are
#'   `Clearance` - Clearance parameters: Cl, Cl2 to be used and `Micro` - Micro
#'   parameters: Ke, K12, K21 to be used. Default is `Clearance`.
#' @param Saturation Logical indicating whether saturation should be considered.
#'   Default is `FALSE`.
#' @param EliminationCpt Logical indicating whether elimination compartment
#'   should be included. Default is `FALSE`.
#' @param FractionExcreted Logical indicating whether fraction excreted
#'   structural parameter should be included in urinecpt statement: `urinecpt(A0
#'   = Cl * C, fe=Fe)`. Valid only if `EliminationCpt == TRUE`. Default is
#'   `FALSE`.
#' @param ByVector Logical indicating whether each element in vectorized
#'   argument should be treated as a separate PML structure (i.e. treated as
#'   data.frame vectors), `TRUE`, or as parameters to obtain a pool (i.e.
#'   expanded) of PML structures, `FALSE`. Default is `FALSE` (one value for a
#'   function call).
#' @param ClosedForm Logical indicating whether closed forms (cfMicro) should be
#'   used when possible. Note that closed forms are not available for the models
#'   with elimination compartment, models with saturation or absorption types
#'   other than `Intravenous` or `First-Order`. The models with interpolated
#'   covariates must use `ClosedForm == FALSE`. Default is `TRUE` (one value for
#'   a function call).
#' @param ... Additional named arguments, including Structural parameters
#'   (StParm), Covariates, Dosepoints (for PK models), Thetas and Omegas. See
#'   'Additional arguments' section.
#'
#' @return A list of PML models (`PMLModels` class instance) matching the
#'   specified options.
#'
#' @details
#'
#' The names of `PMLStructure` are constructed
#' by the following parts:
#' * Model type ('PK'),
#' * Compartments number
#' * Abbreviated absorption type:
#'     * 'IV' for Intravenous,
#'     * 'FO' for First-Order,
#'     * 'G' for Gamma,
#'     * 'W' for Weibull,
#'     * 'IG' for Inverse Gaussian,
#' * Abbreviated parameterization ('C' for Clearance and 'M' for Micro),
#' * Abbreviated saturation if presented ('S'),
#' * Abbreviated elimination if presented ('E'),
#' * Abbreviated fraction excreted if presented ('F').
#'
#' # Additional arguments
#'
#' Additional arguments (ellipsis) will be applied
#' sequentially. They can be used to add or modify Structural parameters
#' (StParm), Covariates, Observations, Dosepoints (for PK models); by the way it is advised to
#' use specific functions for it (see 'See Also' section for the references).
#' Also it is possible to modify Omegas and Thetas, but it is impossible to add
#' them (they are parts of other structures). If `PMLStructure` argument is not
#' specified, class instances will be modified or added in all PML structures.
#' If `PMLStructure` argument is specified, class instances in the specified PML
#' structure will be modified/added. Note that only one PML structure could be
#' added to the class instance. If multiple structures should be modified,
#' suggest to use specific functions.
#'
#' @family Dosepoints
#' @family StParms
#' @family Observations
#' @family Omegas
#' @family Thetas
#' @family Covariates
#'
#' @examples
#' # Get PK model set with default options
#' PMLParametersSets <- create_ModelPK()
#'
#' #' # Get PK Model search with custom options:
#' # will create 2 PML Parameters Sets with 2 and 3 compartments,
#' # with Absorption First-Order and Gamma accordingly:
#' ModelPKSearch <-
#'   create_ModelPK(CompartmentsNumber = c(2, 3),
#'                  Parameterization = "Micro",
#'                  Absorption = c("First-Order", "Gamma"),
#'                  ByVector = TRUE,
#'                  ClosedForm = TRUE)
#'
#'
#' # Next example will create a set of 4 PMLParametersSets:
#' # a combination of models with 2 and 3 compartments and First-Order and Gamma Absorption
#' PMLParametersSets <-
#'   create_ModelPK(CompartmentsNumber = c(2, 3),
#'                  Absorption = c("First-Order", "Gamma"),
#'                  ByVector = FALSE,
#'                  ClosedForm = FALSE)
#'
#' # Create 2 PML Parameters Sets with elimination compartment and fraction excreted
#' # and add zero order absorption to the main dosepoint of the PML Structure
#' # with infusion
#' PMLParametersSets <-
#'   create_ModelPK(CompartmentsNumber = 1,
#'                  Absorption = c("Intravenous", "Gamma"),
#'                  EliminationCpt = TRUE,
#'                  FractionExcreted = TRUE,
#'                  duration = StParm(StParmName = "Duration",
#'                                    OmegaStParm = Omega(State = "None")),
#'                                    PMLStructure = "PK1IVCEF")
#'
#' # Create 4 PML Parameters Sets, then modify `Cl` structural parameter for all sets,
#' # with 2 initial estimates sets to be searched,
#' # add `tlag` as a structural parameter `Tlag` to 1 compartment First-Order PML parameters set,
#' # change `tvKa` Theta initial estimate,
#' # change `nV` Omega initial estimate,
#' # change `CObs` Observation sigmas,
#' # add structural parameter `Rate` for 1 compartment Weibull Parameters set,
#' # add `Weight` covariate for all structural parameters to be searched.
#'
#' PMLParametersSets <-
#'    create_ModelPK(
#'      CompartmentsNumber = 1,
#'      Absorption = c("First-Order", "Weibull"),
#'      ByVector = FALSE,
#'      Cl = StParm(
#'        StParmName = "Cl",
#'        Type = "LogNormal2",
#'        ThetaStParm =
#'          Theta(Name = "tvCl",
#'                InitialEstimates =
#'                  InitialEstimate(c(-Inf, 0.2, Inf),
#'                                  c(0, 3, 10)))
#'      ),
#'      tlag = StParm(
#'        StParmName = "Tlag",
#'        State = "Searched",
#'        PMLStructure = "PK1FOC",
#'        Covariates = list(
#'          Age = Covariate(
#'            Name = "Age",
#'            Type = "Categorical",
#'            State = "Searched",
#'            Direction = "Backward",
#'            Center = "None",
#'            Categories = c(1, 2, 3)
#'          )
#'        )
#'      ),
#'      tvKa = Theta(Name = "tvKa", InitialEstimates = 10),
#'      nV = Omega(Name = "nV", InitialOmega = 0.1),
#'      CObs = Observation(
#'        ObservationName = "CObs",
#'        SigmasChosen = list(
#'          AdditiveMultiplicative = c(PropPart = 0.1, AddPart = 2),
#'          Proportional = 1
#'        )
#'      ),
#'      A1 = Dosepoint(
#'        DosepointName = "A1",
#'        rate = StParm(StParmName = "Rate"),
#'        PMLStructure = "PK1WC"
#'      ),
#'      Weight = Covariate(
#'        Name = "Weight",
#'        State = "Searched",
#'        Center = "Median"
#'      )
#'    )
#'
#' @export
create_ModelPK <- function(CompartmentsNumber = 1,
                           Absorption = "Intravenous",
                           Parameterization = "Clearance",
                           Saturation = FALSE,
                           EliminationCpt = FALSE,
                           FractionExcreted = FALSE,
                           ByVector = FALSE,
                           ClosedForm = TRUE,
                           ...) {
  .strictify_FunctionArgs()
  if (any(is.na(match(
    CompartmentsNumber,
    unique(NLMEFullStructuralOptionsDosesStParms$CompartmentsNumber)
  )))) {
    stop(
      "Number of compartments supported:",
      paste(
        unique(
          NLMEFullStructuralOptionsDosesStParms$CompartmentsNumber
        ),
        collapse = ", "
      ),
      "\nCurrent value(s): ",
      paste(CompartmentsNumber, collapse = ", ")
    )
  }

  if (any(is.na(match(
    Absorption,
    unique(NLMEFullStructuralOptionsDosesStParms$Absorption)
  )))) {
    stop(
      "Types of absorption supported:",
      paste(
        unique(NLMEFullStructuralOptionsDosesStParms$Absorption),
        collapse = ", "
      ),
      "\nCurrent type(s): ",
      paste(Absorption, collapse = ", ")
    )
  }

  if (any(is.na(match(
    Parameterization,
    unique(NLMEFullStructuralOptionsDosesStParms$Parameterization)
  )))) {
    stop(
      "Parameterization supported:",
      paste(
        unique(NLMEFullStructuralOptionsDosesStParms$Parameterization),
        collapse = ", "
      ),
      "\nCurrent Parameterization(s): ",
      paste(Parameterization, collapse = ", ")
    )
  }

  stopifnot(is.logical(Saturation))
  stopifnot(is.logical(EliminationCpt))
  stopifnot(is.logical(FractionExcreted))
  stopifnot(is.logical(ClosedForm))
  if (length(ClosedForm) != 1) {
    stop("ClosedForm argument could be FALSE or TRUE.")
  }

  if (!ByVector) {
    RequestedPMLDF <-
      expand.grid(
        CompartmentsNumber = CompartmentsNumber,
        Absorption = Absorption,
        Parameterization = Parameterization,
        Saturation = Saturation,
        EliminationCpt = EliminationCpt,
        FractionExcreted = FractionExcreted
      )
  } else {
    tryCatch({
      RequestedPMLDF <-
        data.frame(
          CompartmentsNumber = CompartmentsNumber,
          Absorption = Absorption,
          Parameterization = Parameterization,
          Saturation = Saturation,
          EliminationCpt = EliminationCpt,
          FractionExcreted = FractionExcreted
        )

      if (any(!RequestedPMLDF$EliminationCpt &
              RequestedPMLDF$FractionExcreted)) {
        warning(
          "When 'EliminationCpt == FALSE', ",
          "there could be no models with 'FractionExcreted == TRUE'"
        )
      }
    },
    error = function(e) {
      stop(
        "Please check the arguments for create_ModelPK: \n",
        " they should allow to build a data.frame when ByVector==TRUE\n",
        "Error reported:\n",
        e,
        call. = FALSE
      )
    })
  }

  RequestedPMLDF$Type <- "PK"
  RequestedPMLDF <- unique.data.frame(RequestedPMLDF)

  PMLStructuralCodes <-
    get_PMLStructuralCode(RequestedPMLDF, ClosedForm)

  PKTerms <- c(colnames(RequestedPMLDF), "ClosedForm", "PMLStructure")

  PMLModels <- list()
  for (PMLStructuralCode in PMLStructuralCodes) {
    PMLModel <-
      as.list(NLMEFullStructuralOptionsDosesStParms[NLMEFullStructuralOptionsDosesStParms$PMLStructure == PMLStructuralCode,])
    # removing not used PD arguments
    PMLModel <- PMLModel[names(PMLModel) %in% PKTerms]
    PMLModel$PMLCode <- PMLModel$PMLStructure
    PMLModel$PMLStructure <- NULL

    if (PMLModel$Absorption == "Intravenous") {
      AbsorptionAbbreviation <- "IV"
    } else if (PMLModel$Absorption == "First-Order") {
      AbsorptionAbbreviation <- "FO"
    } else if (PMLModel$Absorption == "Gamma") {
      AbsorptionAbbreviation <- "G"
    } else if (PMLModel$Absorption == "Weibull") {
      AbsorptionAbbreviation <- "W"
    } else if (PMLModel$Absorption == "Inverse Gaussian") {
      AbsorptionAbbreviation <- "IG"
    } else {
      AbsorptionAbbreviation <- "?"
    }

    ParameterizationAbbreviation <-
      substr(PMLModel$Parameterization, 1, 1)
    SaturationAbbreviation <- ifelse(PMLModel$Saturation, "S", "")
    EliminationAbbreviation <-
      ifelse(PMLModel$EliminationCpt, "E", "")
    FractionExcretedAbbreviation <-
      ifelse(PMLModel$FractionExcreted, "F", "")

    SpaceName <- paste0(
      PMLModel$Type,
      PMLModel$CompartmentsNumber,
      AbsorptionAbbreviation,
      ParameterizationAbbreviation,
      SaturationAbbreviation,
      EliminationAbbreviation,
      FractionExcretedAbbreviation
    )

    StParmsDosepointsObs <- get_StParmsDosepoints(PMLStructuralCode)
    PMLModel$StParms <- list()
    for (StParmString in StParmsDosepointsObs$StParms) {
      PMLModel$StParms[[StParmString]] <-
        StParm(StParmName = StParmString,
               PMLStructure = SpaceName)
    }

    if ("A0" %in% StParmsDosepointsObs$InputNames &
        !PMLModel$EliminationCpt) {
      warning(
        "For the current PML model A0 input name is found, but elmination is not requested:\n",
        PMLStructuralCode
      )
    }

    PMLModel$Observations <- list()
    for (InputName in StParmsDosepointsObs$InputNames) {
      ObservationName <- paste0(InputName, "Obs")
      PMLModel$Observations[[ObservationName]] <-
        Observation(ObservationName = ObservationName,
                    PMLStructure = SpaceName)
    }

    PMLModel$MainDosepoint[[StParmsDosepointsObs$MainDosepoint]] <-
      Dosepoint(DosepointName = StParmsDosepointsObs$MainDosepoint,
                PMLStructure = SpaceName)
    PMLModels[[SpaceName]] <- PMLModel
  }

  PMLModels <-
    structure(PMLModels,
              class = "PMLModels")

  if (...length() != 0) {
    #DotsNames <- names(match.call(expand.dots = FALSE)$...)
    Dots <- eval(substitute(alist(...), env = environment()))
    DotsNames <- names(Dots)

    #  there could be additional arguments added by mistake
    # from other create_ function. Need to report it
    ExpectedArgs <- names(formals(create_ModelPK))
    UnexpectedArgs <- names(formals(create_ModelEmax))
    UnexpectedArgs <- UnexpectedArgs[!UnexpectedArgs %in% ExpectedArgs]
    if (length(UnexpectedArgs[UnexpectedArgs %in% DotsNames]) > 0) {
      warning("The following arguments are invalid for this function ",
              "and will be ignored: ",
              paste(UnexpectedArgs[UnexpectedArgs %in% DotsNames], collapse = ", "))
    }

    PMLModels <-
      .incorporate_EllipsisArgs(PMLModels, Dots, DotsNames)
  }

  PMLModels
}

#' @rdname create_ModelPK
#' @export
get_PMLParametersSets <- create_ModelPK

#' @export
print.PMLModels <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  PMLNames <- names(x)
  for (ModelTextBlockIndex in seq_along(x)) {
    ModelTextBlock <- x[[ModelTextBlockIndex]]
    cat(PMLNames[ModelTextBlockIndex], "\n",
        get_ModelText(ModelTextBlock, c()), "\n")
  }
}

.incorporate_EllipsisArgs <- function(PMLModels, Dots, DotsNames) {
  StParmNames <-
    unique(.gather_ClassProperties(PMLModels, "StParm", "StParmName", c()))

  ObservationNames <-
    unique(.gather_ClassProperties(PMLModels, "Observation", "ObservationName", c()))

  DosepointNames <-
    unique(.gather_ClassProperties(PMLModels, "Dosepoint", "DosepointName", c()))

  CovariateNames <-
    unique(.gather_ClassProperties(PMLModels, "Covariate", "Name", c()))

  ThetaNames <-
    unique(.gather_ClassProperties(PMLModels, "Theta", "Name", c()))

  OmegaNames <-
    unique(.gather_ClassProperties(PMLModels, "Omega", "Name", c()))

  for (DotIndex in seq_along(Dots)) {
    Dot <- eval(Dots[[DotIndex]])
    DotName <- DotsNames[DotIndex]
    if (is.list(Dot) &&
        .check_0nzchar(Dot$PMLStructure) &&
        !Dot$PMLStructure %in% names(PMLModels)) {
      # if PML structure is specified, but not presented in the list of models
      stop(
        "Please check the PML Structure specified for ",
        DotName,
        ". Current PML Structure is not found: ",
        Dot$PMLStructure
      )
    }

    if (inherits(Dot, "StParm")) {
      PMLModels <-
        .incorporate_StParm(PMLModels, Dot, DotName, StParmNames)

    } else if (inherits(Dot, "Theta")) {
      PMLModels <- .incorporate_Theta(PMLModels, Dot, DotName, ThetaNames)

    } else if (inherits(Dot, "Omega")) {
      PMLModels <- .incorporate_Omega(PMLModels, Dot, DotName, OmegaNames)


    } else if (inherits(Dot, "Dosepoint")) {
      PMLModels <-
        .incorporate_Dosepoint(PMLModels, Dot, DotName, DosepointNames)

    } else if (inherits(Dot, "Observation")) {
      PMLModels <-
        .incorporate_Observation(PMLModels, Dot, DotName, ObservationNames)

    } else if (inherits(Dot, "Covariate")) {
      PMLModels <-
        .incorporate_Covariate(PMLModels, Dot, DotName, CovariateNames)
    } else if (inherits(Dot, "character")) {
      PMLModels <- .assign_TextToVariable(
        ParmList = PMLModels,
        VariableName = DotName,
        VariableText = Dot
      )
    } else {
      warning("Cannot apply current structure to PMLParametersSet")
    }
  }

  PMLModels
}

.gather_ListNames <-
  function(ParmList, ListNamesVector) {
    if (!is.list(ParmList)            |
        length(ParmList) == 0)
      return(ListNamesVector)

    ListNamesVector <- c(ListNamesVector, names(ParmList))
    for (iElement in 1:length(ParmList)) {
      if (is.list(ParmList[[iElement]])) {
        ListNamesVector <- c(ListNamesVector,
                             .gather_ListNames(ParmList[[iElement]], ListNamesVector))
      }
    }

    unique(ListNamesVector)
  }
