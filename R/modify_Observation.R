#' Modify Observation class in PML models
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams Observation
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures in which the observation will be modified. For the naming
#'   convention of PMLStructures, see Details section of
#'   [create_ModelPK()] for PK models and [create_ModelPD()] for PD models.
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @details This function can only be used to modify the structural parameters
#'   in the built-in models (i.e., created using either `create_ModelEmax()` or
#'   `create_ModelPK()`).
#'
#' @family Observations
#' @seealso [list_Observations()]
#'
#' @examples
#' PMLParametersSets <-
#'   create_ModelPK(CompartmentsNumber = c(1, 2, 3))
#' # update structural paramter type
#' PMLParametersSetsVMod <-
#'  modify_Observation(
#'    PMLParametersSets,
#'    ObservationName = "CObs",
#'    SigmasChosen = Sigmas(Proportional = 0,
#'                          AdditiveMultiplicative =
#'                            list(PropPart = 0.1, AddPart = 10)))
#'
#' print(PMLParametersSetsVMod)
#'
#' @export
modify_Observation <- function(PMLParametersSets,
                               ObservationName,
                               SigmasChosen,
                               BQL,
                               BQLValue,
                               Frozen,
                               ResetObs,
                               Covariates,
                               PMLStructures = NULL) {
  .addmodify_Observation(
    PMLParametersSets = PMLParametersSets,
    ObservationName = ObservationName,
    SigmasChosen = SigmasChosen,
    BQL = BQL,
    BQLValue = BQLValue,
    Frozen = Frozen,
    ResetObs = ResetObs,
    Covariates = Covariates,
    PMLStructures = PMLStructures,
    Modify = TRUE
  )
}

.addmodify_Observation <- function(PMLParametersSets,
                                   ObservationName,
                                   SigmasChosen,
                                   BQL,
                                   BQLValue,
                                   Frozen,
                                   ResetObs,
                                   Covariates,
                                   PMLStructures,
                                   Modify) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(ObservationName) &&
              length(ObservationName) == 1)

  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  if (Modify) {
    ObservationsAvailable <-
      list_Observations(PMLParametersSets,
                        IncludeCustom = FALSE)

    if (!ObservationName %in% ObservationsAvailable) {
      warning(
        ObservationName,
        " was not found in the list of Observations.\n",
        "Observations available: ",
        paste(ObservationsAvailable, collapse = ", ")
      )

      return(PMLParametersSets)
    }
  }

  Modified <- FALSE
  for (PMLStructure in PMLStructures) {
    ObservationsCurrentPML <-
      list_Observations(PMLParametersSets[[PMLStructure]],
                        IncludeCustom = FALSE)

    if (Modify &&
        !ObservationName %in% ObservationsCurrentPML) {
      next
    }

    if (Modify ||
        ObservationName %in% ObservationsCurrentPML) {
      # Observation already exists and will be modified
      if (!Modify) {
        # trying to add Observation already added
        message(
          "For PMLStructure == ",
          PMLStructure,
          ", Observation == ",
          ObservationName,
          " already exists and ",
          "will be substituted."
        )
      } else {
        Modified <- TRUE
      }

      ObservationToModify <-
        .get_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          Name = ObservationName,
          InstanceNameElement = "ObservationName"
        )

      if (all(is.na(ObservationToModify)))
        next

      if (!missing(SigmasChosen)) {
        ObservationToModify$SigmasChosen <- SigmasChosen
      }

      if (!missing(BQL)) {
        ObservationToModify$BQL <- BQL
      }

      if (!missing(BQLValue)) {
        ObservationToModify$BQLValue <- BQLValue
      }

      if (!missing(Frozen)) {
        ObservationToModify$Frozen  <- Frozen
      }

      if (!missing(ResetObs)) {
        ObservationToModify$ResetObs  <- ResetObs
      }

      if (!missing(Covariates)) {
        ObservationToModify$Covariates  <- Covariates
      }

      # reinitialize
      ObservationToModify <-
        Observation(
          ObservationName  = ObservationToModify$ObservationName,
          SigmasChosen  = ObservationToModify$SigmasChosen,
          BQL = ObservationToModify$BQL,
          BQLValue = ObservationToModify$BQLValue,
          Frozen = ObservationToModify$Frozen,
          ResetObs = ObservationToModify$ResetObs,
          Covariates = ObservationToModify$Covariates,
          PMLStructure = PMLStructure
        )

      PMLParametersSets[[PMLStructure]] <-
        .subst_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          DotName = ObservationName,
          Dot = ObservationToModify,
          PMLStructure = PMLStructure,
          InstanceName = "ObservationName"
        )
    } else {
      # add_Observation used and current dosepoint is not presented
      ObservationToAdd <-
        Observation(
          ObservationName = ObservationName ,
          SigmasChosen = SigmasChosen ,
          BQL = BQL,
          BQLValue = BQLValue,
          Frozen = Frozen,
          ResetObs = ResetObs,
          Covariates = Covariates,
          PMLStructure = PMLStructure
        )

      PMLParametersSets[[PMLStructure]]$Observations[[ObservationName]] <-
        ObservationToAdd
    }
  }

  if (Modify && !Modified) {
    warning("No Observations were modified. Please check 'PMLStructures' argument")
  }

  PMLParametersSets
}

#' Remove Observation from PML models
#'
#' @inheritParams modify_Observation
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures from which the observation will be removed. For the naming
#'   convention of PMLStructures, see Details section of
#'   [create_ModelPK()] for PK models and [create_ModelPD()] for PD models.
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @details The current functionality does not support modifying custom
#'   observations that are defined within the PML code of custom model spaces.
#'
#' @family Observations
#' @seealso [list_Observations()]
#'
#' @examples
#' PMLParametersSets <-
#'   create_ModelPK(
#'     CompartmentsNumber = c(2, 3),
#'     Parameterization = "Micro",
#'     Absorption = c("First-Order", "Gamma"),
#'     ByVector = TRUE,
#'     ClosedForm = TRUE,
#'     EliminationCpt = TRUE)
#'
#' remove_Observation(PMLParametersSets,
#'                    ObservationName = "A0Obs",
#'                    PMLStructures = "PK3GME")
#'
#' @export
remove_Observation <- function(PMLParametersSets,
                               ObservationName,
                               PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(ObservationName) &&
              length(ObservationName) == 1)

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)
  ObservationsAvailable <-
    list_Observations(PMLParametersSets,
                      IncludeCustom = FALSE)

  if (!ObservationName %in% ObservationsAvailable) {
    stop(
      "Observation ",
      ObservationName,
      " not found in PMLModels set.\n",
      "Observation(s) available: ",
      paste(ObservationsAvailable, collapse = ", ")
    )
  }

  Removed <- FALSE
  for (PMLStructure in PMLStructures) {
    ObservationsCurrentPML <-
      list_Observations(PMLParametersSets[[PMLStructure]],
                        IncludeCustom = FALSE)

    if (!ObservationName %in% ObservationsCurrentPML)
      next

    PMLParametersSets[[PMLStructure]]$Observations[[ObservationName]] <-
      NULL

    Removed <- TRUE
  }

  if (!Removed) {
    warning("No Observations were removed. Please check 'PMLStructures' argument")
  }

  PMLParametersSets
}

#' List Observations in the current PML set
#'
#' This function lists the names of Observations in a given `PMLModels` class
#' instance.
#'
#' @inheritParams modify_Observation
#' @param IncludeCustom Logical. Should the names of responses (`observe`,
#'   `multi`, `ordinal`, `count`, `event` and `LL`) from the PML code of custom
#'   spaces be included or not. Default is `TRUE`.
#' @param ObservationsOnly Logical. If `TRUE` (default), only the names of
#'   `observe` responses are included in the PML code generated for custom
#'   spaces.  Non-observed response names (such as `multi`, `ordinal`, `count`,
#'   `event`, and `LL`) are not included. Ignored if `IncludeCustom == FALSE`.
#'
#' @seealso [Observation()] [modify_Observation()] [remove_Observation()]
#'
#' @return A character vector containing the names of Observations
#'
#' @examples
#' PMLParametersSets <-
#'   create_ModelPK(
#'     Absorption = c("First-Order", "Gamma"),
#'     EliminationCpt = c(TRUE, FALSE))
#' list_Observations(PMLParametersSets)
#'
#' @export
list_Observations <- function(PMLParametersSets,
                              IncludeCustom = TRUE,
                              ObservationsOnly = TRUE) {
  if (!is.list(PMLParametersSets)) {
    return(c())
  }

  stopifnot(is.logical(IncludeCustom))
  stopifnot(is.logical(ObservationsOnly))

  ObservationsNames <-
    .gather_ClassProperties(PMLParametersSets,
                            "Observation",
                            "ObservationName",
                            c())

  if (IncludeCustom) {
    CustomObservationsNames <-
      .gather_ClassProperties(PMLParametersSets,
                              "ObservationCustom",
                              "ObservationName",
                              c())

    if (ObservationsOnly) {
      CustomObservationsTypes <-
        .gather_ClassProperties(PMLParametersSets,
                                "ObservationCustom",
                                "Type",
                                c())

      CustomObservationsNames <-
        CustomObservationsNames[CustomObservationsTypes == "observe"]
    }

    ObservationsNames <-
      c(ObservationsNames, CustomObservationsNames)
  }

  unique(ObservationsNames)
}
