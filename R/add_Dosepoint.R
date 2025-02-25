#' Add Dosepoint in PML models
#'
#' @inheritParams Dosepoint
#' @inheritParams modify_Dosepoint
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @family Dosepoints
#' @noRd
add_Dosepoint <- function(PMLParametersSets,
                          DosepointName = "A1",
                          State = "Present",
                          tlag = c(),
                          bioavail = c(),
                          duration = c(),
                          rate = c(),
                          PMLStructures = NULL) {
  .addmodify_Dosepoint(
    PMLParametersSets = PMLParametersSets,
    DosepointName = DosepointName,
    State = State,
    tlag = tlag,
    bioavail = bioavail,
    duration = duration,
    rate = rate,
    PMLStructures = PMLStructures,
    Modify = FALSE
  )
}

#' Modify Dosepoint in PML models
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams Dosepoint
#' @inheritParams add_Covariate
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures in which the dosepoint statement will be modified. For the
#'   naming convention of PMLStructures, see Details section of
#'   [get_PMLParametersSets()].
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @details This function can only be used to modify the structural parameters
#'   in the built-in models (i.e., created using either `create_ModelEmax()` or
#'   `create_ModelPK()`).
#'
#' @family Dosepoints
#' @seealso [list_Dosepoints()]
#'
#' @examples
#' PMLParametersSets <-
#'   get_PMLParametersSets(CompartmentsNumber = c(1, 2, 3))
#' # update structural paramter type
#' PMLParametersSetsVMod <-
#'  modify_Dosepoint(PMLParametersSets,
#'             DosepointName = "A1",
#'             tlag = StParm(StParmName = "Tlag",
#'                           State = "Searched"))
#'
#' @export
modify_Dosepoint <- function(PMLParametersSets,
                             DosepointName,
                             tlag,
                             bioavail,
                             duration,
                             rate,
                             PMLStructures = NULL) {
  .addmodify_Dosepoint(
    PMLParametersSets = PMLParametersSets,
    DosepointName = DosepointName,
    tlag = tlag,
    bioavail = bioavail,
    duration = duration,
    rate = rate,
    PMLStructures = PMLStructures,
    Modify = TRUE
  )
}

.addmodify_Dosepoint <- function(PMLParametersSets,
                                 DosepointName,
                                 State,
                                 tlag,
                                 bioavail,
                                 duration,
                                 rate,
                                 PMLStructures,
                                 Modify) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(DosepointName) &&
              length(DosepointName) == 1)

  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  if (Modify) {
    DosepointsAvailable <-
      list_Dosepoints(PMLParametersSets,
                      IncludeAll = TRUE,
                      IncludeCustom = FALSE)

    if (!DosepointName %in% DosepointsAvailable) {
      warning(
        DosepointName,
        " was not found in the list of non-custom Dosepoints.\n",
        "Dosepoints available: ",
        paste(DosepointsAvailable, collapse = ", ")
      )

      return(PMLParametersSets)
    }
  }

  Modified <- FALSE
  for (PMLStructure in PMLStructures) {
    DosepointsCurrentPML <-
      list_Dosepoints(PMLParametersSets[[PMLStructure]],
                      IncludeAll = TRUE,
                      IncludeCustom = FALSE)

    if (Modify &&
        !DosepointName %in% DosepointsCurrentPML) {
      next
    }

    if (Modify ||
        DosepointName %in% DosepointsCurrentPML) {
      # Dosepoint already exists and will be modified
      if (!Modify) {
        # trying to add Dosepoint already added
        message(
          "For PMLStructure == ",
          PMLStructure,
          ", Dosepoint == ",
          DosepointName,
          " already exists and ",
          "will be substituted."
        )
      } else {
        Modified <- TRUE
      }

      DosepointToModify <-
        .get_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          Name = DosepointName,
          InstanceNameElement = "DosepointName"
        )

      if (all(is.na(DosepointToModify)))
        next

      if (!missing(State)) {
        DosepointToModify$State <- State
      }

      if (!missing(tlag)) {
        DosepointToModify$tlag <- tlag
      }

      if (!missing(bioavail)) {
        DosepointToModify$bioavail <- bioavail
      }

      if (!missing(duration)) {
        DosepointToModify$duration  <- duration
      }

      if (!missing(rate)) {
        DosepointToModify$rate  <- rate
      }

      # reinitialize
      DosepointToModify <-
        Dosepoint(
          DosepointName = DosepointToModify$DosepointName,
          State = DosepointToModify$State,
          tlag = DosepointToModify$tlag,
          bioavail = DosepointToModify$bioavail,
          duration = DosepointToModify$duration,
          rate = DosepointToModify$rate,
          PMLStructure = PMLStructure
        )

      PMLParametersSets[[PMLStructure]] <-
        .subst_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          DotName = DosepointName,
          Dot = DosepointToModify,
          PMLStructure = PMLStructure,
          InstanceName = "DosepointName"
        )
    } else {
      # add_Dosepoint used and current dosepoint is not presented
      DosepointToAdd <-
        Dosepoint(
          DosepointName = DosepointName,
          State = State,
          tlag = tlag,
          bioavail = bioavail,
          duration = duration,
          rate = rate,
          PMLStructure = PMLStructure
        )

      if (length(PMLParametersSets[[PMLStructure]]$MainDosepoint) == 0) {
        PMLParametersSets[[PMLStructure]]$MainDosepoint[[DosepointName]] <-
          DosepointToAdd
      } else {
        PMLParametersSets[[PMLStructure]]$SecondaryDosepoints[[DosepointName]] <-
          DosepointToAdd
      }
    }
  }

  if (Modify && !Modified) {
    warning("No dosepoints were modified. Please check 'PMLStructures' argument")
  }

  PMLParametersSets
}

#' List Dosepoints in the current PML set
#'
#' This function lists the names of dosepoints in a given set of
#' PMLParametersSets.
#'
#' @inheritParams modify_Dosepoint
#' @param IncludeAll Logical. Should the names of dosepoints with `None` state
#'   be included or not. Default is `FALSE`.
#' @param IncludeCustom Logical. Should the names of custom `dosepoint` and
#'   `dosepoint2` statements (from the PML code of custom spaces) be included or
#'   not. Default is `TRUE`.
#'
#' @return A character vector containing the names of dosepoints
#'
#' @seealso [modify_Dosepoint()]
#'
#' @examples
#' PMLParametersSets <-
#'   get_PMLParametersSets(
#'     Absorption = c("First-Order", "Gamma"))
#' list_Dosepoints(PMLParametersSets)
#'
#' @export
list_Dosepoints <- function(PMLParametersSets,
                            IncludeAll = FALSE,
                            IncludeCustom = TRUE) {
  if (!is.list(PMLParametersSets)) {
    return(c())
  }
  stopifnot(is.logical(IncludeAll))
  stopifnot(is.logical(IncludeCustom))

  DosepointsAvailable <-
    .gather_ClassProperties(PMLParametersSets,
                            "Dosepoint",
                            "DosepointName",
                            c())

  if (!IncludeAll) {
    DosepointsStates <-
      .gather_ClassProperties(PMLParametersSets,
                              "Dosepoint",
                              "State",
                              c())

    DosepointsAvailable <-
      DosepointsAvailable[DosepointsStates != "None"]
  }


  if (IncludeCustom) {
    unique(c(
      DosepointsAvailable,
      .gather_ClassProperties(PMLParametersSets,
                              "DosepointCustom",
                              "DosepointName",
                              c())
    ))
  } else {
    DosepointsAvailable
  }

}
