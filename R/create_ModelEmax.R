#' Get the list of objects describing the PML models by set of PD parameters
#'
#' This function provides the PML (Pharmacometric Modelling Language) PD
#' parameter sets based on the specified options. They are available as a list
#' of specific S3 classes.
#'
#' @inheritParams create_ModelPK
#' @param Type Pharmacodynamic model type. Currently, only Emax is supported.
#' @param Baseline Logical indicating whether the Emax model contains a baseline
#'   response. If it is set to TRUE,  the new parameter, E0, for baseline
#'   response is added to the model. Default is `FALSE`.
#' @param Fractional Logical indicating whether the Emax model with baseline
#'   response is fractional. Applicable only for the Emax models with baseline
#'   response, otherwise a warning is given and current parameter is ignored.
#'   Default is `FALSE`.
#' @param Inhibitory Logical indicating whether the model is inhibitory. If it
#'   is set to TRUE, the structural parameters 'EC50' and 'Emax' change to
#'   'IC50' (concentration producing 50% of maximal inhibition) and 'Imax'.
#'   Default is `FALSE`.
#' @param Sigmoid Logical indicating whether the model is sigmoidal. If it is
#'   set to TRUE, the Hill coefficient, 'Gam', is added to the model. Default is
#'   `FALSE`.
#'
#' @inherit create_ModelPK return
#'
#' @inheritSection create_ModelPK Additional arguments
#'
#' @details The names of `PMLStructure` are constructed by the following parts:
#' * Baseline if presented (abbreviated as 'E0'),
#' * Fractional if presented (abbreviated as '1+'),
#' * Inhibitory (abbreviated as 'Imax' if the model is inhibitory and 'Emax' otherwise),
#' * Sigmoid  if presented (abbreviated as 'Gam').
#'
#' @family StParms
#' @family Observations
#' @family Omegas
#' @family Thetas
#' @family Covariates
#'
#' @examples
#' # Get PD model set with default options
#' PDParametersSets <- create_ModelPD(Type = "Emax")
#'
#' # Create PD model set with all possible combinations
#' # will give a warning since When 'Baseline == FALSE',
#' # there could be no model with 'Fractional == TRUE'
#' PDParametersSets <-
#'   create_ModelPD(Type = "Emax",
#'                  Baseline = FALSE,
#'                  Inhibitory = c(FALSE, TRUE),
#'                  Sigmoid = c(FALSE, TRUE),
#'                  ByVector = FALSE)
#'
#' @export
create_ModelPD <- function(Type = "Emax",
                           Baseline = FALSE,
                           Fractional = FALSE,
                           Inhibitory = FALSE,
                           Sigmoid = FALSE,
                           ByVector = FALSE,
                           ...) {

  match.arg(Type)
  if (Type == "Emax") {
    create_ModelEmax(Baseline = Baseline,
                     Fractional = Fractional,
                     Inhibitory = Inhibitory,
                     Sigmoid = Sigmoid,
                     ByVector = ByVector,
                     ...)
  }
}


#' Get the list of objects describing the PML models by set of Emax parameters
#'
#' This function provides the PML (Pharmacometric Modelling Language) Emax
#' parameter sets based on the specified options. They are available as a list
#' of specific S3 classes.
#'
#' @inheritParams create_ModelPD
#'
#' @inherit create_ModelPK return
#'
#' @examples
#' # Get Emax model set with default options
#' PDParametersSets <- create_ModelEmax()
#'
#' # Create Emax model set with all possible combinations
#' # will give a warning since When 'Baseline == FALSE',
#' # there could be no model with 'Fractional == TRUE'
#' PDParametersSets <-
#'   create_ModelEmax(Baseline = TRUE,
#'                    Fractional = c(FALSE, TRUE),
#'                    Inhibitory = c(FALSE, TRUE),
#'                    Sigmoid = c(FALSE, TRUE),
#'                    ByVector = FALSE)
#'
#' @export
create_ModelEmax <- function(Baseline = FALSE,
                             Fractional = FALSE,
                             Inhibitory = FALSE,
                             Sigmoid = FALSE,
                             ByVector = FALSE,
                             ...) {
 stopifnot(is.logical(Baseline))
  stopifnot(is.logical(Fractional))
  stopifnot(is.logical(Inhibitory))
  stopifnot(is.logical(Sigmoid))

  if (!ByVector) {
    RequestedPMLDF <-
      expand.grid(
        Baseline = Baseline,
        Fractional = Fractional,
        Inhibitory = Inhibitory,
        Sigmoid = Sigmoid
      )
  } else {
    tryCatch({
      RequestedPMLDF <-
        data.frame(
          Baseline = Baseline,
          Fractional = Fractional,
          Inhibitory = Inhibitory,
          Sigmoid = Sigmoid
        )
    },
    error = function(e) {
      stop(
        "Please check the arguments for create_ModelEmax(): \n",
        " they should allow to build a data.frame when ByVector==TRUE\n",
        "Error reported:\n",
        e,
        call. = FALSE
      )
    })
  }

  if (any(!RequestedPMLDF$Baseline &
          RequestedPMLDF$Fractional)) {
    if (!ByVector &&
        length(Baseline) == 2 &&
        length(Fractional) == 2) {
      # was expanded and there's a clone
      # removing it
      RequestedPMLDF <- subset(RequestedPMLDF, RequestedPMLDF$Baseline | !RequestedPMLDF$Fractional)
      warning(
        "A model with 'Baseline == FALSE' and 'Fractional == TRUE' ",
        "is removed from the search since it duplicates the model with ",
        "'Baseline == FALSE' and 'Fractional == FALSE'"
      )
    } else {
      warning(
        "When 'Baseline == FALSE', ",
        "there could be no model with 'Fractional == TRUE'. Such model will be interpreted as ",
        "'Baseline == FALSE' and 'Fractional == FALSE'"
      )

      RequestedPMLDF[!RequestedPMLDF$Baseline & RequestedPMLDF$Fractional, "Fractional"] <- FALSE
    }
  }

  RequestedPMLDF <- unique.data.frame(RequestedPMLDF)
  RequestedPMLDF$Type <- "Emax"

  PMLStructuralCodes <-
    get_PMLStructuralCode(RequestedPMLDF, ClosedForm = FALSE)

  PDTerms <- c(colnames(RequestedPMLDF), "PMLStructure", "PMLCode")

  PMLModels <- list()
  for (PMLStructuralCode in PMLStructuralCodes) {
    PMLModel <-
      as.list(NLMEFullStructuralOptionsDosesStParms[NLMEFullStructuralOptionsDosesStParms$PMLStructure == PMLStructuralCode,])
    PMLModel$PMLCode <- PMLModel$PMLStructure
    PMLModel$PMLStructure <- NULL

    # removing not used PK arguments
    PMLModel <- PMLModel[names(PMLModel) %in% PDTerms]

    BaselineAbbreviation <- ifelse(PMLModel$Baseline, "E0", "")
    FractionalAbbreviation <-
      ifelse(PMLModel$Fractional, "1+", "")
    InhibitoryAbbreviation <-
      ifelse(PMLModel$Inhibitory, "Imax", "Emax")
    SigmoidAbbreviation <-
      ifelse(PMLModel$Sigmoid, "Gam", "")

    PMLStructure <- paste0(
      InhibitoryAbbreviation,
      BaselineAbbreviation,
      FractionalAbbreviation,
      SigmoidAbbreviation
    )

    StParmsDosepointsObs <- get_StParmsDosepoints(PMLStructuralCode)
    PMLModel$StParms <- list()
    for (StParmString in StParmsDosepointsObs$StParms) {
      PMLModel$StParms[[StParmString]] <-
        StParm(StParmName = StParmString,
               PMLStructure = PMLStructure)
    }

    PMLModel$Observations <- list()
    # we know that Emax is a special case with C as a covariate
    if (length(StParmsDosepointsObs$InputNames) == 2 &&
        "C" %in% StParmsDosepointsObs$InputNames) {
      StParmsDosepointsObs$InputNames <-
        StParmsDosepointsObs$InputNames[StParmsDosepointsObs$InputNames != "C"]
      CCovariate <- TRUE
    } else {
      CCovariate <- FALSE
    }

    for (InputName in StParmsDosepointsObs$InputNames) {
      ObservationName <- paste0(InputName, "Obs")

      if (CCovariate) {
        CCovariate <- FALSE
        PMLModel$Observations[[ObservationName]] <-
          Observation(ObservationName = ObservationName,
                      Covariates = list(Covariate(Name = "C",
                                                  PMLStructure = PMLStructure)),
                      PMLStructure = PMLStructure)

      } else {
        PMLModel$Observations[[ObservationName]] <-
          Observation(ObservationName = ObservationName,
                      PMLStructure = PMLStructure)
      }
    }

    PMLModels[[PMLStructure]] <- PMLModel
  }

  PMLModels <-
    structure(PMLModels,
              class = "PMLModels")

  if (...length() != 0) {
    Dots <- eval(substitute(alist(...), env = environment()))
    DotsNames <- names(Dots)

    #  there could be additional arguments added by mistake
    # from other create_ function. Need to report it
    ExpectedArgs <- names(formals(create_ModelEmax))
    UnexpectedArgs <- names(formals(create_ModelPK))
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
