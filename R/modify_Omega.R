#' Modify Omega Parameters in PML Models
#'
#' This function allows to modify Omega parameters in a list of PML models
#' (`PMLModels` class instance created by [get_PMLParametersSets()]).
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams Omega
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures in which the Omega will be modified. For the naming convention
#'   of PMLStructures, see Details section of [create_ModelPK()] for PK models
#'   and [create_ModelPD()] for PD models.
#'
#' @details
#' * If the specified Omega does not exist in the PML models, a warning
#' will be issued, and no modifications will be made.
#'
#'
#' * The current functionality does not support modifying custom
#' omegas (ranefs) that are defined within the PML code of custom model spaces.
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @family Omegas
#' @seealso [list_Omegas()]
#'
#' @examples
#' PMLParametersSets12 <- create_ModelPK(CompartmentsNumber = c(1, 2))
#' # Modify an Omega parameter named "nV" with new Initial Estimate and
#' # Frozen flag
#' PMLParametersSets12Mod1 <-
#'   modify_Omega(PMLParametersSets12,
#'                Name = "nV",
#'                InitialOmega = 0.3,
#'                State = "Present",
#'                Frozen = TRUE,
#'                PMLStructures = "PK1IVC")
#'
#' print(PMLParametersSets12Mod1)
#'
#' @export
modify_Omega <- function(PMLParametersSets,
                         Name,
                         InitialOmega,
                         State,
                         Frozen,
                         PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(Name) && length(Name) == 1)

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  OmegasNamesAvailable <-
    list_Omegas(PMLParametersSets,
                IncludeAll = TRUE,
                IncludeCustom = FALSE)

  if (!Name %in% OmegasNamesAvailable) {
    warning(
      Name,
      " was not found in the Omega names list. ",
      "Omegas available:\n",
      paste(OmegasNamesAvailable, collapse = ", ")
    )

    return(PMLParametersSets)
  }

  CurrentOccasionLikeOmegas <-
    OmegasNamesAvailable[gsub("\\d+$", "", OmegasNamesAvailable) == gsub("\\d+$", "", Name)]

  if (grepl("x\\d+$", Name) &&
      length(CurrentOccasionLikeOmegas) > 1 &&
      CurrentOccasionLikeOmegas[1] != Name) {
    warning(
      Name,
      " Omega looks like an occasion omega ",
      "for the category rather than the 1st one. ",
      "If so, any changes made won't be visible ",
      "until current omega becomes the first in occasion sequence."
    )
  }

  Modified <- FALSE
  for (PMLStructure in PMLStructures) {
    OmegasCurrentPML <-
      list_Omegas(PMLParametersSets[[PMLStructure]],
                  IncludeAll = TRUE,
                  IncludeCustom = FALSE)

    OmegaToModify <-
      .get_ClassInstance(
        ParmList = PMLParametersSets[[PMLStructure]],
        Name = Name,
        InstanceNameElement = "Name"
      )

    if (all(is.na(OmegaToModify)))
      next

    if (!missing(Name)) {
      OmegaToModify$Name <- Name
    }

    if (!missing(InitialOmega)) {
      OmegaToModify$InitialOmega <- InitialOmega
    }

    if (!missing(State)) {
      OmegaToModify$State <- State
    }

    if (!missing(Frozen)) {
      OmegaToModify$Frozen <- Frozen
    }

    OmegaToModify <- Omega(
      Name = Name,
      InitialOmega = OmegaToModify$InitialOmega,
      State = OmegaToModify$State,
      Frozen = OmegaToModify$Frozen,
      StParmName =  OmegaToModify$StParmName,
      PMLStructure = PMLStructure
    )

    PMLParametersSets[[PMLStructure]] <-
      .subst_ClassInstance(
        ParmList = PMLParametersSets[[PMLStructure]],
        DotName = Name,
        Dot = OmegaToModify,
        PMLStructure = PMLStructure,
        InstanceName = "Name"
      )

    Modified <- TRUE
  }

  if (!Modified) {
    warning("No omegas were modified. Please check 'PMLStructures' argument")
  }

  PMLParametersSets
}

#' List Unique Omega Names
#'
#' This function lists the unique names of Omega parameters in a given set.
#'
#' @param PMLParametersSets `PMLModels` class instance or an element (one PML
#'   structure) of this class or `StParm` class.
#' @param IncludeAll Logical. Whether should the omega names to be inlcuded from
#'   structural parameters, covariates or omegas with a `State == 'None'`.
#' @param IncludeCustom Logical. Should the names of custom `ranef` statements
#'   (from the PML code of custom spaces) be included or not. Default is `TRUE`.
#'
#' @return A character vector containing the unique names of Omega parameters.
#'
#' @seealso [Omega()] [modify_Omega()]
#'
#' @examples
#' PMLParametersSets <- create_ModelPK()
#' list_Omegas(PMLParametersSets)
#'
#' @export
list_Omegas <- function(PMLParametersSets,
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

  OmegasList <- c()

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

      if (StParmToLook$OmegaStParm$State != "None") {
        OmegasList <- c(OmegasList,
                        StParmToLook$OmegaStParm$Name)
      }

      CovariatesList <- list_Covariates(StParmToLook,
                                        IncludeAll = IncludeAll)

      if (length(CovariatesList) == 0)
        next

      for (CovariateName in CovariatesList) {
        if (StParmToLook$Covariates[[CovariateName]]$Type == "Occasion") {
          OmegaNames <-
            .gather_ClassProperties(StParmToLook$Covariates[[CovariateName]],
                                    "Omega",
                                    "Name",
                                    c())
          if (!IncludeAll) {
            OmegaStates <-
              .gather_ClassProperties(StParmToLook$Covariates[[CovariateName]],
                                      "Omega",
                                      "State",
                                      c())
            OmegaNames <- OmegaNames[OmegaStates != "None"]
          }

          if (length(OmegaNames) == 0)
            next

          OmegasList <- c(OmegasList, OmegaNames)
        }
      }
    }

    if (IncludeCustom) {
      CustomRanefsNames <-
        unlist(sapply(PMLParameterSet$CustomRanefs, "[[", "RanefNames"))
      OmegasList <- c(OmegasList,
                      CustomRanefsNames)
    }
  }

  unique(OmegasList)
}
