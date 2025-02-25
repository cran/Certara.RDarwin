
.incorporate_StParm <-
  function(PMLModels, Dot, DotName, StParmNames) {
    if (.check_0nzchar(DotName)) {
      if (!DotName %in% c("tlag", "bioavail", "rate", "duration") &&
          DotName != Dot$StParmName) {
        # need to catch the case when the argument name is not the same as StParmName
        # it is OK only when special dosepoint arguments are used
        warning(
          DotName,
          " argument is not equal to StParmName ",
          Dot$StParmName,
          ". StParmName will be used.",
          immediate. = TRUE
        )
        DotName <- Dot$StParmName
      }
    } else {
      DotName <- Dot$StParmName
    }

    if (DotName %in% StParmNames) {
      PMLModels <-
        .subst_ClassInstance(
          ParmList = PMLModels,
          DotName = DotName,
          Dot = Dot,
          PMLStructure = "",
          InstanceName = "StParmName"
        )
    } else if (DotName %in% c("tlag", "bioavail", "rate", "duration")) {
      PMLModels <- .subst_ClassInstanceDoseSpecial(
        ParmList = PMLModels,
        DotName = DotName,
        Dot = Dot,
        PMLStructure = ""
      )
    } else {
      PMLModels <-
        .add_ClassInstance(
          ParmList = PMLModels,
          DotName = DotName,
          Dot = Dot,
          PMLStructure = "",
          ListName = "StParms"
        )
    }

    PMLModels
  }

.incorporate_Theta <-
  function(PMLModels, Dot, DotName, ThetaNames) {
    if (.check_0nzchar(DotName) &&
        DotName != Dot$Name) {
      warning(
        DotName,
        " argument is not equal to Theta Name `",
        Dot$Name,
        "`. Theta's Name will be used.",
        immediate. = TRUE
      )
    }

    DotName <- Dot$Name

    if (DotName %in% ThetaNames) {
      PMLModels <-
        .subst_ClassInstance(
          ParmList = PMLModels,
          DotName = DotName,
          Dot = Dot,
          PMLStructure = "",
          InstanceName = "Name"
        )
    } else {
      stop(
        "Cannot create new Theta ",
        DotName,
        " without associated structural parameter.",
        " please create new structural parameter."
      )
    }

    PMLModels
  }

.incorporate_Omega <-
  function(PMLModels, Dot, DotName, OmegaNames) {
    if (.check_0nzchar(DotName) &&
        DotName != Dot$Name) {
      warning(
        DotName,
        " argument is not equal to Omega Name `",
        Dot$Name,
        "`. Omega's Name will be used.",
        immediate. = TRUE
      )
    }

    DotName <- Dot$Name

    if (DotName %in% OmegaNames) {
      PMLModels <-
        .subst_ClassInstance(
          ParmList = PMLModels,
          DotName = DotName,
          Dot = Dot,
          PMLStructure = "",
          InstanceName = "Name"
        )
    } else {
      stop("Cannot create new Omega ",
           DotName,
           " without associated structural parameter.")
    }

    PMLModels
  }

.incorporate_Dosepoint <-
  function(PMLModels, Dot, DotName, DosepointNames) {
    if (.check_0nzchar(DotName) &&
        DotName != Dot$DosepointName) {
      warning(
        DotName,
        " argument is not equal to DosepointName",
        Dot$DosepointName,
        ". DosepointName will be used.",
        immediate. = TRUE
      )
    }

    DotName <- Dot$DosepointName

    if (DotName %in% DosepointNames) {
      PMLModels <-
        .subst_ClassInstance(
          ParmList = PMLModels,
          DotName = DotName,
          Dot = Dot,
          PMLStructure = "",
          InstanceName = "DosepointName"
        )
    } else {
      for (PMLModelIndex in seq_along(PMLModels)) {
        if (.check_0nzchar(Dot$PMLStructure) &&
            names(PMLModels)[PMLModelIndex] != Dot$PMLStructure) {
          next
        }

        DosepointToAdd <- Dot
        if (!.check_0nzchar(Dot$PMLStructure)) {
          DosepointToAdd$PMLStructure <- names(PMLModels)[PMLModelIndex]
        }

        if (length(PMLModels[[PMLModelIndex]]$MainDosepoint) == 0) {
          PMLModels[[PMLModelIndex]]$MainDosepoint[[DotName]] <-
            DosepointToAdd
        } else {
          PMLModels[[PMLModelIndex]]$SecondaryDosepoints[[DotName]] <-
            DosepointToAdd
        }
      }
    }

    PMLModels
  }

.incorporate_Observation <-
  function(PMLModels, Dot, DotName, ObservationNames) {
    if (.check_0nzchar(DotName) &&
        DotName != Dot$ObservationName) {
      warning(
        DotName,
        " argument is not equal to ObservationName",
        Dot$ObservationName,
        ". ObservationName will be used.",
        immediate. = TRUE
      )
    }

    DotName <- Dot$ObservationName

    if (DotName %in% ObservationNames) {

      PMLModels <- .addmodify_Observation(PMLParametersSets = PMLModels,
                                         ObservationName = DotName,
                                         SigmasChosen = Dot$SigmasChosen,
                                         BQL = Dot$BQL,
                                         BQLValue = Dot$BQLValue,
                                         Frozen = Dot$Frozen,
                                         ResetObs = Dot$ResetObs,
                                         PMLStructures = names(PMLModels),
                                         Modify = TRUE)

    } else {
      for (PMLModelIndex in seq_along(PMLModels)) {
        if (.check_0nzchar(Dot$PMLStructure) &&
            names(PMLModels)[PMLModelIndex] != Dot$PMLStructure) {
          next
        }

        ObservationToAdd <- Dot
        if (!.check_0nzchar(Dot$PMLStructure)) {
          ObservationToAdd$PMLStructure <- names(PMLModels)[PMLModelIndex]
        }

        PMLModels[[PMLModelIndex]]$Observations[[DotName]] <-
          ObservationToAdd

      }
    }

    PMLModels
  }

.incorporate_Covariate <-
  function(PMLModels, Dot, DotName, CovariateNames) {
    if (.check_0nzchar(DotName) &&
        DotName != Dot$Name) {
      warning(
        DotName,
        " argument is not equal to Covariate Name",
        Dot$Name,
        ". Covariate Name will be used.",
        immediate. = TRUE
      )
    }

    DotName <- Dot$Name
    PMLModels <-
      .modify_CovariateInstance(
        ParmList = PMLModels,
        DotName = DotName,
        Dot = Dot,
        PMLStructure = ""
      )

    PMLModels
  }
