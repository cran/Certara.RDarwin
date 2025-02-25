#' Add Covariate into PML models
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams Covariate
#' @param Name A character string representing the name of the covariate to be
#'   added.
#' @param StParmNames Character or character vector specifying names of
#'   structural parameters to which covariates should be added. Can be set to
#'   `NULL` or not specified, for such case, covariate will be added to all
#'   structural parameters.
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures to which the covariate will be added. For the naming covention
#'   of PMLStructures, see Details section of [create_ModelPK()] for PK models
#'   and [create_ModelPD()] for PD models.
#'
#' @details
#' * If Covariate already exists, it will be substituted with a new
#' instance with given properties. New covariate will have default bound
#' omegas/thetas. The user can change thetas with [modify_Theta()] and omegas
#' with [modify_Omega()].
#'
#' * The current functionality does not support adding or modifying custom
#' covariates that are defined within the PML code of custom model spaces.
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @family Covariates
#' @seealso [list_Covariates()] [modify_Theta()] [modify_Omega()]
#'
#' @examples
#' PMLParametersSets <- create_ModelPK()
#'
#' PMLParametersSetsWT <-
#'  add_Covariate(PMLParametersSets,
#'                Name = "WT",
#'                Type = "Continuous",
#'                State = "Present",
#'                Direction = "Forward",
#'                Center = 70)
#'
#' PMLParametersSetsWTCL <-
#'  add_Covariate(PMLParametersSets = PMLParametersSetsWT,
#'                Name = "Race",
#'                Type = "Categorical",
#'                State = "Searched",
#'                Direction = "Backward",
#'                Categories = c(1,2,3),
#'                StParmNames = "Cl",
#'                PMLStructure = "PK1IVC")
#'
#' @export
add_Covariate <- function(PMLParametersSets,
                          Name,
                          Type = "Continuous",
                          StParmNames = NULL,
                          State = "Present",
                          Direction = "Forward",
                          Center = "None",
                          Categories = c(),
                          PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))

  StParmNames <- .check_StParmNames(PMLParametersSets, StParmNames)

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  StParmModified <- FALSE
  for (PMLStructure in PMLStructures) {

    StParmsCurrentPML <-
      unique(.gather_ClassProperties(PMLParametersSets[[PMLStructure]], "StParm", "StParmName", c()))
    StParmNamesAvailable <-
      StParmNames[StParmNames %in% StParmsCurrentPML]

    for (StParmName in StParmNamesAvailable) {
      # reinitialize to get related thetas/omegas if not given
      CovariateToPaste <-
        Covariate(
          Name = Name,
          Type = Type,
          StParmName = StParmName,
          State = State,
          Direction = Direction,
          Center = Center,
          Categories = Categories,
          PMLStructure = PMLStructure
        )

      StParmInstance <-
        .get_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          Name = StParmName,
          InstanceNameElement = "StParmName"
        )

      CovariateNamesCurrentStParm <-
        unique(.gather_ClassProperties(StParmInstance, "Covariate", "Name", c()))

      if (Name %in% CovariateNamesCurrentStParm &&
          StParmInstance$State != "None") {
        message(
          "For PMLStructure == ",
          PMLStructure,
          ", StParmName == ",
          StParmName,
          " covariate ",
          Name,
          " already exists and ",
          "will be substituted."
        )
      }

      PMLParametersSets[[PMLStructure]] <-
        .modify_CovariateInstance(
          PMLParametersSets[[PMLStructure]],
          DotName = Name,
          Dot = CovariateToPaste,
          PMLStructure = PMLStructure
        )

      StParmModified <- TRUE
    }
  }

  if (!StParmModified) {
    message("No covariates were added. Please check StParmNames and PMLStructures arguments.")
  }

  PMLParametersSets
}

.check_StParmNames <- function(PMLParametersSets, StParmNames) {
  StParmNamesInPML <-
    unique(.gather_ClassProperties(PMLParametersSets, "StParm", "StParmName", c()))

  # prepare StParmNames
  if (length(StParmNames) == 0) {
    StParmNames <- StParmNamesInPML
  } else {
    stopifnot(is.character(StParmNames))
    if (any(is.na(match(StParmNames, StParmNamesInPML)))) {
      stop(
        "Current StParmNames are not presented in non-custom StParm structures: ",
        paste(StParmNames[is.na(match(StParmNames, StParmNamesInPML))], collapse = ", ")
      )
    }
  }

  StParmNames
}

.check_PMLStructures <- function(PMLParametersSets, PMLStructures) {
  if (length(PMLStructures) == 0) {
    PMLStructures <- names(PMLParametersSets)

  } else {
    stopifnot(is.character(PMLStructures))
    if (any(is.na(match(
      PMLStructures, names(PMLParametersSets)
    )))) {
      stop(
        "Current PMLStructures are not found in PML parameters set: ",
        paste(PMLStructures[is.na(match(PMLStructures, names(PMLParametersSets)))], collapse = ", "),
        ". PMLStructures presented: ",
        paste(names(PMLParametersSets), collapse = ", ")
      )
    }
  }

  PMLStructures
}

#' Remove Covariate from PML models
#'
#' @inheritParams add_Covariate
#' @param Name Character specifying the name of the covariate to be removed.
#' @param StParmNames Character or character vector specifying names of
#'   structural parameters from which the covariate will be removed. Can be set
#'   to `NULL` or not specified, for such case the covariate will be removed from
#'   all structural parameters.
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures from which the covariate will be removed. For the naming
#'   convention of PMLStructures, see Details section of  see details section of
#'   [get_PMLParametersSets()].
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @details The current functionality does not support removing custom
#' covariates that are defined within the PML code of custom model spaces.
#'
#'
#' @family Covariates
#' @seealso [list_Covariates()]
#'
#' @examples
#' PMLParametersSets <- get_PMLParametersSets()
#'
#' PMLParametersSetsWT <-
#'  add_Covariate(PMLParametersSets,
#'                Name = "WT",
#'                Type = "Continuous",
#'                State = "Present",
#'                Direction = "Forward",
#'                Center = 70)
#'
#' PMLParametersSetsVonly <-
#'  remove_Covariate(PMLParametersSets = PMLParametersSetsWT,
#'                Name = "WT",
#'                StParmNames = "Cl")
#'
#' @export
remove_Covariate <- function(PMLParametersSets,
                             Name,
                             StParmNames = NULL,
                             PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))

  StParmNames <- .check_StParmNames(PMLParametersSets, StParmNames)

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  for (PMLStructure in PMLStructures) {
    StParmsCurrentPML <-
      unique(.gather_ClassProperties(PMLParametersSets, "StParm", "StParmName", c()))
    StParmNamesAvailable <-
      StParmNames[StParmNames %in% StParmsCurrentPML]

    for (StParmName in StParmNamesAvailable) {
      CovariateNamesCurrentStParm <-
        sapply(PMLParametersSets[[PMLStructure]]$StParms[[StParmName]]$Covariates,
               function(x)
                 x$Name)

      if (!Name %in% CovariateNamesCurrentStParm)
        next

      if (length(CovariateNamesCurrentStParm) > 0) {
        # if Covariates element does not have names - name it
        names(PMLParametersSets[[PMLStructure]]$StParms[[StParmName]]$Covariates) <-
          CovariateNamesCurrentStParm
      }

      PMLParametersSets[[PMLStructure]]$StParms[[StParmName]]$Covariates[[Name]] <-
        NULL
    }
  }

  PMLParametersSets
}

#' List Covariates in the currrent PML set
#'
#' This function lists the names of covariates in a given set of
#' PMLParametersSets.
#'
#' @inheritParams add_StParm
#' @param IncludeAll Logical. Should the names of covariates with `None` state
#'   or covariates inside structural parameters with `None` state be included or
#'   not.
#' @param IncludeCustom Logical. Should the names of `covariate`, `fcovariate`
#'   and `interpolate` statements (from the PML code of custom spaces) be
#'   included or not. Default is `TRUE`.
#'
#' @return A character vector containing the names of covariates.
#'
#' @seealso [add_Covariate()] [remove_Covariate()] [Covariate()]
#'
#' @examples
#' PMLParametersSets <- get_PMLParametersSets()
#' PMLParametersSets <- add_Covariate(PMLParametersSets,
#'                                    Name  = "WT")
#' list_Covariates(PMLParametersSets)
#'
#' @export
list_Covariates <- function(PMLParametersSets,
                            IncludeAll = FALSE,
                            IncludeCustom = TRUE) {
  if (!is.list(PMLParametersSets)) {
    return(c())
  }

  stopifnot(is.logical(IncludeAll))
  stopifnot(is.logical(IncludeCustom))

  if (inherits(PMLParametersSets, "StParm")) {
    PMLParametersSets <- list(PMLParametersSets)
  }

  StParmsAvailable <-
    list_StParms(PMLParametersSets,
                 IncludeAll = IncludeAll,
                 IncludeCustom = FALSE)

  CovariatesList <- c()
  for (StParmName in StParmsAvailable) {
    StParmToLook <-
      .get_ClassInstance(
        ParmList = PMLParametersSets,
        Name = StParmName,
        InstanceNameElement = "StParmName"
      )

    if (StParmToLook$State == "None" ||
        length(StParmToLook$Covariates) == 0)
      next

    CovariatesNames <-
      .gather_ClassProperties(StParmToLook, "Covariate", "Name", c())

    if (!IncludeAll) {
      CovariateStates <-
        .gather_ClassProperties(StParmToLook, "Covariate", "State", c())
      CovariatesNames <- CovariatesNames[CovariateStates != "None"]
    }

    if (length(CovariatesNames) == 0)
      next

    CovariatesList <- c(CovariatesList, CovariatesNames)
  }

  if (IncludeCustom) {
    CovariatesList <-
      c(CovariatesList,
        unlist(lapply(PMLParametersSets, function(x) {
          names(x$CustomCovariates)
        })))
  }

  unique(CovariatesList)
}


#' adds or modifies covariate inside StParm
#' @noRd
#' @keywords internal NLME
.modify_CovariateInstance <-
  function(ParmList,
           DotName,
           Dot,
           PMLStructure) {
    if (!is.list(ParmList)       |
        length(ParmList) == 0    |
        !.check_0nzchar(DotName) |
        is.null(Dot))
      return(ParmList)

    for (iElement in 1:length(ParmList)) {
      if (!is.list(ParmList[[iElement]]))
        next

      if (!.check_0nzchar(PMLStructure)) {
        # assign PMLStructure and go deeper
        # PMLStructure is given as a name vector of upper structure
        # use it internally for current PML and reset after
        PMLStructure <- names(ParmList)[[iElement]]

        PMLStructureEqualToRequested <-
          .check_0nzchar(Dot$PMLStructure) &&
          PMLStructure == Dot$PMLStructure

        PMLStructureNotPresent <- !.check_0nzchar(Dot$PMLStructure)

        if (PMLStructureEqualToRequested ||
            PMLStructureNotPresent) {
          ParmList[[iElement]] <-
            .modify_CovariateInstance(ParmList[[iElement]], DotName, Dot, PMLStructure)
        }

        PMLStructure <- ""
      } else if (inherits(ParmList[[iElement]], "StParm")) {
        # check if covariate should be added into current StParm
        StParmNameEqualToRequested <-
          .check_0nzchar(Dot$StParmName) &&
          Dot$StParmName == ParmList[[iElement]]$StParmName

        StParmNameNotPresent <- !.check_0nzchar(Dot$StParmName)
        if (StParmNameEqualToRequested ||
            StParmNameNotPresent) {
          CovariateToPaste <- Dot
          if (StParmNameNotPresent) {
            CovariateToPaste$StParmName <- ParmList[[iElement]]$StParmName
          }

          CovariateToPaste$PMLStructure <- PMLStructure

          ParmList[[iElement]]$Covariates[[DotName]] <-
            CovariateToPaste

          if (StParmNameNotPresent) {
            ParmList[[iElement]] <-
              .harmonize_StParmCovariateThetaOmega(StParmInstance = ParmList[[iElement]],
                                                   CovariateInstance = CovariateToPaste)
          }
        }
      } else {
        ParmList[[iElement]] <-
          .modify_CovariateInstance(ParmList = ParmList[[iElement]],
                                    DotName,
                                    Dot,
                                    PMLStructure = PMLStructure)
      }
    }

    ParmList
  }
