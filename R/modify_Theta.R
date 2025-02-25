#' Modify Theta Parameters in PML Models
#'
#' This function allows to modify Theta parameter in a list of PML models
#' (`PMLModels` class instance created by [create_ModelPK()] or [create_ModelPD]).
#'
#' @inheritParams Theta
#' @param PMLParametersSets A list of PML parameters sets (`PMLModels` class
#'   instance).
#' @param Name Character specifying the name of the Theta to be modified.
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures in which the `Theta` parameter will be modified. For the naming
#'   convention of `PMLStructures`, see Details section of
#'   [create_ModelPK()] for PK models and [create_ModelPD()] for PD models..
#'
#' @details
#' * If the specified Theta does not exist in the PML models, a warning
#' will be issued, and no modifications will be made. Thetas associated with
#' structural parameters in the proportional part of MixRatio and
#' Additive+Proportional error models can also be modified.
#' * The current functionality does not support modifying custom
#' thetas (fixefs) that are defined within the PML code of custom model spaces.
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @family Thetas
#' @seealso [InitialEstimate()]
#'
#' @examples
#' PMLParametersSets <- create_ModelPK(CompartmentsNumber = c(1, 2))
#' # Modify a Theta parameter named "tvV" with new Initial Estimates and
#' # Frozen flag
#' PMLParametersSetsMod1 <-
#'   modify_Theta(PMLParametersSets,
#'                Name = "tvV",
#'                Frozen = TRUE,
#'                InitialEstimates = 0.3)
#'
#' print(PMLParametersSetsMod1)
#'
#' PMLParametersSetsMod2 <-
#'   add_StParm(PMLParametersSets = PMLParametersSetsMod1,
#'              StParmName = "Duration",
#'              State = "Searched",
#'              PMLStructures = "PK2IVC",
#'              DosepointArgName = "duration")
#'
#' PMLParametersSetsMod3 <-
#'   modify_Theta(PMLParametersSets = PMLParametersSetsMod2,
#'                Name = "tvDuration",
#'                InitialEstimates = c(2, 4, Inf))
#'
#' print(PMLParametersSetsMod3)
#'
#' @export
modify_Theta <- function(PMLParametersSets,
                         Name,
                         InitialEstimates,
                         Frozen,
                         PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(Name) && length(Name) == 1)

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  ThetasNamesAvailable <-
    list_Thetas(PMLParametersSets,
                IncludeCustom = FALSE)

  if (!Name %in% ThetasNamesAvailable) {
    warning(
      Name,
      " was not found in the Theta names list. ",
      "The available Thetas are:\n",
      paste(ThetasNamesAvailable, collapse = ", ")
    )

    return(PMLParametersSets)
  }

  Modified <- FALSE
  for (PMLStructure in PMLStructures) {
    ThetasCurrentPML <-
      list_Thetas(PMLParametersSets[[PMLStructure]],
                  IncludeCustom = FALSE)

    ThetaToModify <-
      .get_ClassInstance(
        ParmList = PMLParametersSets[[PMLStructure]],
        Name = Name,
        InstanceNameElement = "Name"
      )

    if (all(is.na(ThetaToModify)))
      next

    if (!missing(Name)) {
      ThetaToModify$Name <- Name
    }

    if (!missing(InitialEstimates)) {
      if (!inherits(InitialEstimates, "InitialEstimate")) {
        InitialEstimates <- InitialEstimate(InitialEstimates)
      }

      ThetaToModify$InitialEstimates <- InitialEstimates
    }

    if (!missing(Frozen)) {
      ThetaToModify$Frozen <- Frozen
    }

    ThetaToModify <- Theta(
      Name = Name,
      InitialEstimates = ThetaToModify$InitialEstimates,
      Frozen = ThetaToModify$Frozen,
      PMLStructure = PMLStructure
    )

    PMLParametersSets[[PMLStructure]] <-
      .subst_ClassInstance(
        ParmList = PMLParametersSets[[PMLStructure]],
        DotName = Name,
        Dot = ThetaToModify,
        PMLStructure = PMLStructure,
        InstanceName = "Name"
      )

    Modified <- TRUE
  }

  if (!Modified) {
    warning("No Thetas were modified. Please check 'PMLStructures' argument")
  }

  PMLParametersSets
}

#' List Unique Theta Names
#'
#' This function lists the unique names of Theta parameters in a given set.
#'
#' @param PMLParametersSets `PMLModels` class instance or an element (one PML
#'   structure) of this class or `StParm` class.
#' @param IncludeAll Logical. Whether should the Theta names to be inlcuded from
#'   structural parameters, covariates or thetas with a `State == 'None'`.
#' @param IncludeCustom Logical. Should the names of custom `theta` statements
#'   (from the PML code of custom spaces) be included or not. Default is `TRUE`.
#'
#' @return A character vector containing the unique names of Theta parameters.
#'
#' @seealso [Theta()] [modify_Theta()]
#'
#' @examples
#' PMLParametersSets <- create_ModelPD()
#' list_Thetas(PMLParametersSets)
#'
#' @export
list_Thetas <- function(PMLParametersSets,
                        IncludeAll = FALSE,
                        IncludeCustom = TRUE) {
  if (!is.list(PMLParametersSets)) {
    return(c())
  }

  stopifnot(is.logical(IncludeAll))
  stopifnot(is.logical(IncludeCustom))

  if (!inherits(PMLParametersSets, "PMLModels")) {
    PMLParametersSets <- list(PMLParametersSets)
  }

  if (inherits(PMLParametersSets, "StParm")) {
    PMLParametersSets <- list(PMLParametersSets)
  }

  ThetasList <- c()

  for (PMLParameterSet in PMLParametersSets) {
    StParmsAvailable <-
      list_StParms(PMLParameterSet,
                   IncludeAll = IncludeAll,
                   IncludeCustom = FALSE)

    for (StParmName in StParmsAvailable) {
      StParmToLook <-
        .get_ClassInstance(
          ParmList = PMLParameterSet,
          Name = StParmName,
          InstanceNameElement = "StParmName"
        )

      if (StParmToLook$State == "None")
        next

      if (StParmToLook$ThetaStParm$State != "None") {
        ThetasList <- unique(c(ThetasList,
                               StParmToLook$ThetaStParm$Name))
      }

      CovariatesList <- list_Covariates(StParmToLook,
                                        IncludeAll = IncludeAll)

      if (length(CovariatesList) == 0)
        next

      for (CovariateName in CovariatesList) {
        if (StParmToLook$Covariates[[CovariateName]]$Type != "Occasion") {
          ThetaNames <-
            .gather_ClassProperties(StParmToLook$Covariates[[CovariateName]],
                                    "Theta",
                                    "Name",
                                    c())
          if (!IncludeAll) {
            ThetaStates <-
              .gather_ClassProperties(StParmToLook$Covariates[[CovariateName]],
                                      "Theta",
                                      "State",
                                      c())
            ThetaNames <- ThetaNames[ThetaStates != "None"]
          }

          if (length(ThetaNames) == 0)
            next

          ThetasList <- unique(c(ThetasList, ThetaNames))
        }
      }
    }

    if (IncludeCustom) {
      ThetasList <-
        c(ThetasList,
          names(PMLParameterSet$CustomFixefs))
    }
  }

  unique(ThetasList)
}
