#' Add Structural parameter into PML models Dosepoints
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams StParm
#' @inheritParams add_Covariate
#' @param StParmName Character specifying the name of the structural parameter
#'   to be added.
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures to which the structural parameter will be added. For the naming
#'   convention of PMLStructures, see Details section of
#'   [get_PMLParametersSets()].
#' @param DosepointArgName Character specifying the name of the argument in the
#'   [Dosepoint()] instance to add/update the associated structural parameter.
#'   Options are `bioavail`, `rate`, `duration`, `tlag`. Not applicable for
#'   custom models
#'
#' @details
#' * only special [Dosepoint()] related structural parameters could be
#' added to built-in models (i.e. created using either `create_ModelPD()` or
#' `create_modelPK()`. Due to ambiguity of situation when a structural parameter
#' is added with `State == 'None'`, a warning is given for such cases.
#' * A structural parameter could be added to the custom model if it not presented in the model yet
#' (as a custom or built-in structural parameter).
#'
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @family StParms
#' @seealso [Dosepoint()] [list_StParms()]
#'
#' @examples
#' PMLParametersSets <-
#'   get_PMLParametersSets(CompartmentsNumber = c(1, 2, 3))
#'
#' # add Rate structural parameter for all PMLModels
#' PMLParametersSetsVModDuration <-
#'  add_StParm(PMLParametersSets,
#'             StParmName = "Duration",
#'             ThetaStParm = Theta("tvD",
#'                           InitialEstimates = 2),
#'             OmegaStParm = Omega(Name = "nD",
#'                                 State = "Searched"),
#'             DosepointArgName = "duration")
#'
#' @export
add_StParm <- function(PMLParametersSets,
                       StParmName,
                       Type = "LogNormal",
                       State = "Present",
                       ThetaStParm = list(),
                       OmegaStParm = list(),
                       Covariates = list(),
                       PMLStructures = NULL,
                       DosepointArgName = character()) {
  .addmodify_StParm(
    PMLParametersSets = PMLParametersSets,
    StParmName = StParmName,
    Type = Type,
    State = State,
    ThetaStParm = ThetaStParm,
    OmegaStParm = OmegaStParm,
    Covariates = Covariates,
    PMLStructures = PMLStructures,
    DosepointArgName = DosepointArgName,
    Modify = FALSE
  )
}

#' Modify structural parameter in PML models set
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams add_StParm
#' @param StParmName Character specifying the name of the structural parameter
#'   to be modified.
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @details This function can only be used to modify the structural parameters
#'   in the built-in models (i.e., created using either `create_ModelEmax()` or
#'   `create_ModelPK()`) or in the custom models if they are added with
#'   `add_StParm()`.
#'
#' @examples
#' PMLParametersSets <-
#'   get_PMLParametersSets(CompartmentsNumber = c(1, 2, 3))
#' # update structural parameter type
#' PMLParametersSetsVMod <-
#'  modify_StParm(PMLParametersSets,
#'             StParmName = "V",
#'             Type = "LogitNormal")
#'
#' @family StParms
#' @seealso [Dosepoint()] [list_StParms()]
#' @export
modify_StParm <- function(PMLParametersSets,
                          StParmName,
                          Type = "LogNormal",
                          State = "Present",
                          ThetaStParm,
                          OmegaStParm,
                          Covariates,
                          PMLStructures = NULL) {
  .addmodify_StParm(
    PMLParametersSets = PMLParametersSets,
    StParmName = StParmName,
    Type = Type,
    State = State,
    ThetaStParm = ThetaStParm,
    OmegaStParm = OmegaStParm,
    Covariates = Covariates,
    PMLStructures = PMLStructures,
    DosepointArgName = NULL,
    Modify = TRUE
  )
}

.addmodify_StParm <- function(PMLParametersSets,
                              StParmName,
                              Type,
                              State,
                              ThetaStParm,
                              OmegaStParm,
                              Covariates,
                              PMLStructures,
                              DosepointArgName,
                              Modify) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(StParmName) && length(StParmName) == 1)
  CustomModelsPresented <-
    sapply(PMLParametersSets, "[[", "Type") == "Custom"

  DosepointArgNames <- c("bioavail", "rate", "duration", "tlag")
  if (length(DosepointArgName) > 0 &&
      !DosepointArgName %in% DosepointArgNames) {
    stop("DosepointArgName should be one of: ",
         paste(DosepointArgNames, collapse = ", "))
  }

  if (!Modify &&
      !any(CustomModelsPresented) &&
      length(DosepointArgName) == 0) {
    stop("DosepointArgName should be provided when add_StParm is used.")
  } else if (all(CustomModelsPresented) &&
             length(DosepointArgName) > 0) {
    stop("DosepointArgName should be used only with non-custom StParms.")
  }

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  ObligatoryStParmNames <-
    get_StParmsDosepoints(NLMEFullStructuralOptionsDosesStParms$PMLStructure)$StParms

  if (Modify) {
    StParmsAvailable <-
      list_StParms(PMLParametersSets,
                   IncludeAll = TRUE,
                   IncludeCustom = FALSE)

    if (!StParmName %in% StParmsAvailable) {
      warning(
        StParmName,
        " was not found in the list of non-custom Structural Parameters.\n",
        "Structural parameters available: ",
        paste(StParmsAvailable, collapse = ", "),
        call. = FALSE
      )

      return(PMLParametersSets)
    }
  }

  # for non-custom models warn that some parameters are essential
  if (!all(CustomModelsPresented) &&
      StParmName %in% ObligatoryStParmNames &&
      !missing(State) &&
      State != "Present") {
    warning(
      "The structural parameter ",
      StParmName,
      " is essential one and it is strongly advised to retain its `State` as Present.",
      call. = FALSE
    )
  }

  # warn about State==None for newly added parameters
  if (!Modify &&
      State == "None") {
    warning(
      "State == `None` means structural parameter ",
      StParmName,
      " to be added won't be used.\n",
      "Please make sure that it is intended.",
      call. = FALSE
    )
  }

  if (length(DosepointArgName) == 0 &&
      tolower(StParmName) %in% DosepointArgNames &&
      !Modify) {
    warning(
      "The StParm name to be added looks like Dosepoint special argument: ",
      StParmName,
      "\nPlease specify `DosepointArgName` argument if Dosepoint should be",
      " modified",
      call. = FALSE
    )
  }

  for (PMLStructure in PMLStructures) {
    StParmsCurrentPML <-
      list_StParms(PMLParametersSets[[PMLStructure]],
                   IncludeAll = TRUE,
                   IncludeCustom = FALSE)

    if (PMLParametersSets[[PMLStructure]]$Type == "Custom" &&
        length(DosepointArgName) > 0) {
      message("Cannot use a dosepoint argument with the custom Model: ",
              PMLStructure)
      next
    }

    if (Modify) {
      # modify_StParm used

      if (PMLParametersSets[[PMLStructure]]$Type == "Custom" &&
          StParmName %in% names(PMLParametersSets[[PMLStructure]]$CustomStParms)) {
        # it is custom StParm:
        message(
          "`modify_StParm()` does not modify the custom structural paramters.",
          "Use `modify_StParmCustom()` to change custom StParms there."
        )

        next
      }

      StParmToModify <-
        .get_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          Name = StParmName,
          InstanceNameElement = "StParmName"
        )

      if (all(is.na(StParmToModify)))
        next

      if (!missing(Type)) {
        StParmToModify$Type <- Type
      }

      if (!missing(State)) {
        StParmToModify$State <- State
      }

      if (!missing(ThetaStParm)) {
        StParmToModify$ThetaStParm <- ThetaStParm
      }

      if (!missing(OmegaStParm)) {
        StParmToModify$OmegaStParm <- OmegaStParm
      }

      if (!missing(Covariates)) {
        StParmToModify$Covariates <- Covariates
      }

      # reinitialize
      StParmToModify <-
        StParm(
          StParmName = StParmToModify$StParmName,
          Type = StParmToModify$Type,
          State = StParmToModify$State,
          ThetaStParm = StParmToModify$ThetaStParm,
          OmegaStParm = StParmToModify$OmegaStParm,
          Covariates = StParmToModify$Covariates,
          PMLStructure = PMLStructure
        )

      PMLParametersSets[[PMLStructure]] <-
        .subst_ClassInstance(
          ParmList = PMLParametersSets[[PMLStructure]],
          DotName = StParmName,
          Dot = StParmToModify,
          PMLStructure = PMLStructure,
          InstanceName = "StParmName"
        )
    } else {
      # add_StParm used
      if (length(DosepointArgName) > 0) {
        DosepointsPresentedInCurrentSpace <-
          list_Dosepoints(PMLParametersSets[[PMLStructure]])
        if (length(DosepointsPresentedInCurrentSpace) == 0) {
          message("There are no Dosepoints to modify in ",
                  PMLStructure)

          next
        }

        if (length(DosepointsPresentedInCurrentSpace) > 1) {
          if (length(PMLParametersSets[[PMLStructure]]$MainDosepoint) == 0) {
            # move first Dosepoint to main
            PMLParametersSets[[PMLStructure]]$MainDosepoint[DosepointsPresentedInCurrentSpace[1]] <-
              PMLParametersSets[[PMLStructure]]$SecondaryDosepoints[DosepointsPresentedInCurrentSpace[1]]
            PMLParametersSets[[PMLStructure]]$SecondaryDosepoints[DosepointsPresentedInCurrentSpace[1]] <-
              NULL
          }

          message(
            "Multiple dosepoints exist within the ",
            PMLStructure,
            " space; only the primary dosepoint (",
            names(PMLParametersSets[[PMLStructure]]$MainDosepoint),
            ") will be updated. ",
            "To adjust any other dosepoints, please use modify_Dosepoint()."
          )
        }

        StParmsCurrentPMLSection <-
          list_StParms(PMLParametersSets[[PMLStructure]]$MainDosepoint,
                       IncludeAll = TRUE)

      } else {
      StParmsCurrentPMLSection <-
        list_StParms(PMLParametersSets[[PMLStructure]]$StParms,
                     IncludeAll = TRUE)
    }

    if (StParmName %in% names(PMLParametersSets[[PMLStructure]]$CustomStParms)) {
      message(
        StParmName,
        " is a custom StParm in ",
        PMLStructure,
        " space amd cannot be added. Use `modify_StParmCustom()` to change it."
      )

      next
    }

    if (StParmName %in% StParmsCurrentPMLSection) {
      # we want to add StParm already added
      message(
        "For PMLStructure == ",
        PMLStructure,
        ", StParmName == ",
        StParmName,
        " already exists and ",
        "will be substituted."
      )

    } else if (StParmName %in% StParmsCurrentPML) {
      # it is presented in PML, but not in the Dosepoint if
      # DosepointArgName is not NULL or vice versa
      if (length(DosepointArgName) > 0) {
        warning(
          "For PMLStructure == ",
          PMLStructure,
          ", StParmName == ",
          StParmName,
          " has been found in StParm section, not Dosepoint section",
          " and won't be substituted.",
          call. = FALSE
        )
      } else {
        warning(
          "For PMLStructure == ",
          PMLStructure,
          ", StParmName == ",
          StParmName,
          " has been found in Dosepoint section, not StParm section",
          " and won't be substituted.",
          call. = FALSE
        )
      }

      next
    }

    # reinitialize
    StParmToPaste <-
      StParm(
        StParmName = StParmName,
        Type = Type,
        State = State,
        ThetaStParm = ThetaStParm,
        OmegaStParm = OmegaStParm,
        Covariates = Covariates,
        PMLStructure = PMLStructure
      )

    if (length(DosepointArgName) > 0) {
      # StParm inside Dosepoint
      PMLParametersSets[[PMLStructure]]$MainDosepoint[[1]][[DosepointArgName]] <-
        StParmToPaste
    } else {
      PMLParametersSets[[PMLStructure]]$StParms[[StParmName]] <-
        StParmToPaste

    }
  }
}

PMLParametersSets
}

.subst_ClassInstance <-
  function(ParmList,
           DotName,
           Dot,
           PMLStructure,
           InstanceName) {

  }

.get_ClassInstance <- function(ParmList,
                               Name,
                               InstanceNameElement) {
  ReturnedValue <- NA

  if (!is.list(ParmList)    |
      length(ParmList) == 0 |
      !.check_0nzchar(Name)) {
    return(ReturnedValue)
  }

  for (iElement in 1:length(ParmList)) {
    if (is.list(ParmList[[iElement]])) {
      if (length(ParmList[[iElement]][[InstanceNameElement]]) > 0 &&
          ParmList[[iElement]][[InstanceNameElement]] == Name) {
        ReturnedValue <- ParmList[[iElement]]
      } else {
        ReturnedValue <-
          .get_ClassInstance(ParmList[[iElement]], Name, InstanceNameElement)
      }

      if (!all(is.na(ReturnedValue)))
        break
    }
  }

  ReturnedValue
}

#' Remove structural parameter from PML models
#'
#' @inheritParams write_ModelTemplateTokens
#' @param StParmName character specifying the name for the structural parameter
#'   to be removed.
#' @param PMLStructures Character or character vector specifying names of PML
#'   structures from which the structural parameter will be removed. For the
#'   naming convention of PMLStructures, see Details section of
#'   [get_PMLParametersSets()].
#'
#' @details Please make sure that structural parameter to be removed is not
#'   essential for the model. Usually the user does not need to remove
#'   any structural parameter. The only case is related to structural parameters
#'   in [Dosepoint()].
#'
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @family StParms
#' @seealso [Dosepoint()] [list_StParms()]
#'
#' @examples
#' PMLParametersSets <- get_PMLParametersSets(CompartmentsNumber = c(1, 2))
#'
#' PMLParametersSetsDuration <-
#'  add_StParm(PMLParametersSets,
#'             StParmName = "Duration",
#'             State = "Searched",
#'             DosepointArgName = "duration")
#'
#' PMLParametersSetsDuration1CptOnly <-
#'  remove_StParm(PMLParametersSetsDuration,
#'                StParmName = "Duration",
#'                PMLStructures = "PK2IVC")
#'
#' @export
remove_StParm <- function(PMLParametersSets,
                          StParmName,
                          PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(StParmName) && length(StParmName) == 1)

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)
  StParmsAvailable <-
    unique(.gather_ClassProperties(PMLParametersSets, "StParm", "StParmName", c()))

  if (!StParmName %in% StParmsAvailable) {
    stop(
      "Structural parameter ",
      StParmName,
      " not found in PMLModels set.\n",
      "Structural parameters available: ",
      paste(StParmsAvailable, collapse = ", ")
    )
  }

  ObligatoryStParmNames <-
    get_StParmsDosepoints(NLMEFullStructuralOptionsDosesStParms$PMLStructure)$StParms

  if (StParmName %in% ObligatoryStParmNames) {
    warning(
      "The structural parameter ",
      StParmName,
      " to be removed is essential one and it strongly advised to retain it in the model.",
      call. = FALSE
    )
  }

  for (PMLStructure in PMLStructures) {
    StParmsCurrentPML <-
      unique(.gather_ClassProperties(PMLParametersSets, "StParm", "StParmName", c()))
    if (!StParmName %in% StParmsCurrentPML)
      next

    StParmsCurrentPMLDosepoint <-
      sapply(PMLParametersSets[[PMLStructure]]$MainDosepoint[[1]],
             function(x, StParmName) {
               StParmDetected <- inherits(x, "StParm")
               if (StParmDetected && x$StParmName == StParmName) {
                 StParmDetected
               } else {
                 FALSE
               }
             },
             StParmName)

    if (any(StParmsCurrentPMLDosepoint)) {
      PMLParametersSets[[PMLStructure]]$MainDosepoint[[1]][StParmsCurrentPMLDosepoint][[1]] <-
        character()
    } else {
      PMLParametersSets[[PMLStructure]]$StParms[[StParmName]] <-
        NULL
    }
  }

  PMLParametersSets
}

#' List Structural Parameters in the currrent PML set
#'
#' This function lists the names of structural parameters in a given set of
#' PMLParametersSets.
#'
#' @inheritParams add_StParm
#' @param IncludeAll Logical. Should the names of structural parameters with
#'   `None` state be included or not. Default is `FALSE`.
#' @param IncludeCustom Logical. Should the names of custom `stparm` statements
#'   (from the PML code of custom spaces) be included or not. Default is `TRUE`.
#'
#' @return A character vector containing the names of structural parameters.
#'
#' @seealso [add_StParm()] [modify_StParm()]
#'
#' @examples
#' PMLParametersSets <- get_PMLParametersSets()
#' list_StParms(PMLParametersSets)
#'
#' @export
list_StParms <- function(PMLParametersSets,
                         IncludeAll = FALSE,
                         IncludeCustom = TRUE) {
  stopifnot(is.logical(IncludeAll))
  stopifnot(is.logical(IncludeCustom))

  if (!is.list(PMLParametersSets)) {
    return(c())
  }


  StParmsAvailable <-
    .gather_ClassProperties(PMLParametersSets,
                            "StParm",
                            "StParmName",
                            c())

  if (!IncludeAll) {
    StParmsState <-
      .gather_ClassProperties(PMLParametersSets,
                              "StParm",
                              "State",
                              c())

    StParmsAvailable <-
      unique(StParmsAvailable[StParmsState != "None"])
  }

  if (IncludeCustom) {
    unique(c(
      StParmsAvailable,
      .gather_ClassProperties(PMLParametersSets,
                              "StParmCustom",
                              "StParmName",
                              c())
    ))
  } else {
    StParmsAvailable
  }
}
