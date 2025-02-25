
.subst_ClassInstance <-
  function(ParmList,
           DotName,
           Dot,
           PMLStructure,
           InstanceName) {
    if (!is.list(ParmList)       |
        length(ParmList) == 0    |
        !.check_0nzchar(DotName) |
        is.null(Dot))
      return(ParmList)

    for (iElement in 1:length(ParmList)) {
      if (is.list(ParmList[[iElement]])) {
        if (length(ParmList[[iElement]][[InstanceName]]) > 0 &&
            ParmList[[iElement]][[InstanceName]] == DotName) {
          # if PMLStructure is given inside the instance to substitute
          # and PMLStructure is specified in upper level then proceed
          # otherwise not
          if (length(PMLStructure) != 0 &&
              is.list(Dot) &&
              "PMLStructure" %in% names(Dot)) {
            if (!.check_0nzchar(Dot$PMLStructure)) {
              Dot$PMLStructure <- PMLStructure
            } else if (Dot$PMLStructure != PMLStructure) {
              break
            }
          }

          ParmList[[iElement]] <- Dot
          break
        }

        if (!.check_0nzchar(PMLStructure)) {
          # PMLStructure is given as a name vector of upper structure
          # use it internally for current PML and reset after
          PMLStructure <- names(ParmList)[[iElement]]

          PMLStructureEqualToRequested <-
            .check_0nzchar(Dot$PMLStructure) &&
            PMLStructure == Dot$PMLStructure

          PMLStructureNotPresent <-
            !.check_0nzchar(Dot$PMLStructure)

          if (PMLStructureEqualToRequested ||
              PMLStructureNotPresent) {
            ParmList[[iElement]] <-
              .subst_ClassInstance(ParmList[[iElement]], DotName, Dot, PMLStructure, InstanceName)
          }

          PMLStructure <- ""
        } else {
          ParmList[[iElement]] <-
            .subst_ClassInstance(ParmList[[iElement]], DotName, Dot, PMLStructure, InstanceName)
        }

      }
    }

    ParmList
  }

.subst_ClassInstanceDoseSpecial <-
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
      if (is.list(ParmList[[iElement]])) {
        if (DotName %in% names(ParmList[[iElement]])) {
          if (length(PMLStructure) != 0 &&
              is.list(Dot) &&
              "PMLStructure" %in% names(Dot)) {
            if (!.check_0nzchar(Dot$PMLStructure)) {
              Dot$PMLStructure <- PMLStructure
            } else if (Dot$PMLStructure != PMLStructure) {
              break
            }
          }

          ParmList[[iElement]][[DotName]] <- Dot
          break
        }

        if (!.check_0nzchar(PMLStructure)) {
          # PMLStructure is given as a name vector of upper structure
          # use it internally for current PML and reset after
          PMLStructure <- names(ParmList)[[iElement]]

          PMLStructureEqualToRequested <-
            .check_0nzchar(Dot$PMLStructure) &&
            PMLStructure == Dot$PMLStructure

          PMLStructureNotPresent <-
            !.check_0nzchar(Dot$PMLStructure)

          if (PMLStructureEqualToRequested ||
              PMLStructureNotPresent) {
            ParmList[[iElement]] <-
              .subst_ClassInstanceDoseSpecial(ParmList[[iElement]], DotName, Dot, PMLStructure)
          }

          PMLStructure <- ""
        } else {
          ParmList[[iElement]] <-
            .subst_ClassInstanceDoseSpecial(ParmList[[iElement]], DotName, Dot, PMLStructure)
        }

      }
    }

    ParmList
  }

.add_ClassInstance <-
  function(ParmList,
           DotName,
           Dot,
           PMLStructure,
           ListName) {
    if (!is.list(ParmList)        |
        length(ParmList) == 0     |
        !.check_0nzchar(DotName)  |
        !.check_0nzchar(ListName) |
        is.null(Dot))
      return(ParmList)

    for (iElement in 1:length(ParmList)) {
      if (names(ParmList)[[iElement]] == ListName) {
        # if PMLStructure is given inside the instance to substitute
        # and PMLStructure is specified in upper level then proceed
        # otherwise not
        if (length(PMLStructure) != 0 &&
            is.list(Dot) &&
            "PMLStructure" %in% names(Dot)) {
          if (!.check_0nzchar(Dot$PMLStructure)) {
            Dot$PMLStructure <- PMLStructure
          } else if (Dot$PMLStructure != PMLStructure) {
            break
          }
        }

        ParmList[[iElement]][[DotName]] <- Dot
        break
      }

      if (is.list(ParmList[[iElement]])) {
        if (!.check_0nzchar(PMLStructure)) {
          # PMLStructure is given as a name vector of upper structure
          # use it internally for current PML and reset after
          PMLStructure <- names(ParmList)[[iElement]]
          ParmList[[iElement]] <-
            .add_ClassInstance(ParmList[[iElement]], DotName, Dot, PMLStructure, ListName)
          PMLStructure <- ""
        } else {
          ParmList[[iElement]] <-
            .add_ClassInstance(ParmList[[iElement]], DotName, Dot, PMLStructure, ListName)
        }
      }
    }

    ParmList
  }

.unclass_List <-
  function(ParmList) {
    if (!is.list(ParmList)       |
        length(ParmList) == 0)
      return(ParmList)

    ParmList <- unclass(ParmList)
    for (iElement in 1:length(ParmList)) {
      if (!is.list(ParmList[[iElement]])) next
      ParmList[[iElement]] <- .unclass_List(ParmList[[iElement]])
    }

    ParmList
  }
