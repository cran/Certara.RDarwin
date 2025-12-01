#' Create a Structural parameter instance
#'
#' Creates a Structural parameter instance
#'  with the given parameters
#'
#' @inheritParams StParm
#'
#' @return A StParm instance.
#'
#' @noRd
#' @keywords internal NLME
new_StParm <- function(StParmName = character(),
                       Type = "LogNormal",
                       State = c("Present", "None", "Searched"),
                       ThetaStParm = list(),
                       OmegaStParm = list(),
                       Covariates = list(),
                       PMLStructure = character()) {
  stopifnot(length(StParmName) == 1 &
              length(Type) > 0 &
              length(State) == 1)

  stopifnot(is.character(StParmName) & .check_0nzchar(StParmName))

  Type <-
    match.arg(
      Type,
      c(
        "LogNormal",
        "LogNormal1",
        "LogNormal2",
        "LogitNormal",
        "Normal"
      ),
      several.ok = TRUE
    )

  State <- match.arg(State)

  if (length(ThetaStParm) > 0) {
    stopifnot(inherits(ThetaStParm, "Theta"))
  }

  if (length(OmegaStParm) > 0) {
    stopifnot(inherits(OmegaStParm, "Omega"))
  }

  if (!missing(Covariates)) {
    if (inherits(Covariates, "Covariate")) {
      Covariates <- list(Covariates)
    } else if (is.list(Covariates)) {
      stopifnot(all(sapply(Covariates, function(x)
        inherits(x, "Covariate"))))
    }
  } else {
    Covariates <- list()
  }

  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for StParm ", StParmName)
  }

  structure(
    list(
      StParmName = StParmName,
      Type = Type,
      State = State,
      ThetaStParm = ThetaStParm,
      OmegaStParm = OmegaStParm,
      Covariates = Covariates,
      PMLStructure = PMLStructure
    ),
    class = "StParm"
  )
}

validate_StParm <- function(StParmInstance) {
  stopifnot(StParmInstance$ThetaStParm$State == "Present")

  StParmInstance
}

#' Create an instance of a Structural parameter.
#'
#' This function creates a new instance of a Structural parameter.
#'
#' @param StParmName Character specifying the name of the structural parameter.
#'
#' @param Type Character specifying the type of the structural parameter. Options are
#' * `LogNormal` The PML statement of the structural parameter will look like the following:
#'
#' `stparm(V = tvV * wt^dVdwt * exp(nV + nVx0*(Occasion==0) + nVx1*(Occasion==1)))`
#' * `LogNormal1` The PML statement of the structural parameter will look like the following:
#'
#' `stparm(V = (tvV + wt*dVdwt) * exp(nV + nVx0*( Occasion==0) + nVx1*( Occasion==1)))`
#' * `LogNormal2` The PML statement of the structural parameter will look like the following:
#'
#' `stparm(V = exp(tvV + wt*dVdwt + nV + nVx0*(Occasion==0) + nVx1*(Occason==1)))`
#' * `LogitNormal` The PML statement of the structural parameter will look like the following:
#'
#' `stparm(V = ilogit(tvV + wt*dVdwt + nV + nVx0*(Occasion==0) + nVx1*(Occasion==1)))`
#' * `Normal` The PML statement of the structural parameter will look like the following:
#'
#' `stparm(V = tvV + wt*dVdwt + nV + nVx0*(Occasion==0) + nVx1*(Occasion==1))`
#'
#' @param State character string that indicates the presence of the structural
#'   parameter. Options are:
#' * `None` The structural parameter does not exist in the specified `PMLStructures`.
#' * `Present` The structural parameter exists in the specified `PMLStructures` (the default).
#' * `Searched` The presence of the structural parameter  is searched.
#'
#' @param ThetaStParm A Theta class instance inside the structural parameter. If
#'   not given, the associated Theta will be automatically created with its name
#'   set to "tv" + `StParmName`.
#'
#' @param OmegaStParm An Omega class instance inside the structural parameter.
#'   If not given, the associated Omega will be automatically created with its
#'   name set to "n" + `StParmName`
#'
#' @param Covariates A list of covariates (`Covariate` instances) that should be
#'   included in the structural parameter statement.
#' @param PMLStructure Character  specifying the name of PML
#'   structure for which current parameter should be attributed. For the
#'   naming convention of PMLStructures, see Details section of
#'   [create_ModelPK()] for PK models and [create_ModelPD()] for PD models.
#'
#' @return An instance of a structural parameter.
#'
#' @examples
#'
#' # Create a Structural parameter instance with default values
#' V <- StParm(StParmName = "V")
#'
#' # Create a Structural parameter with Normal type:
#' V2 <- StParm("V2",
#'        Type = "Normal",
#'        ThetaStParm = Theta(Name = "tvV2", InitialEstimates = 0.1))
#'
#' # Create a Structural parameter instance with covariates:
#' Cl <- StParm(
#'   StParmName = "Cl",
#'   Covariates = Covariate(
#'     Name = "Period",
#'     Type = "Occasion",
#'     State = "Searched",
#'     Categories = c(1,2),
#'     Omegas = list(Omega(Name = "nPeriodx1", 2),
#'                   Omega(Name = "nPeriodx2", 3))),
#'   PMLStructure = "1CFOE")
#'
#' @family StParms
#' @export
StParm <- function(StParmName = character(),
                   Type = "LogNormal",
                   State = "Present",
                   ThetaStParm = list(),
                   OmegaStParm = list(),
                   Covariates = list(),
                   PMLStructure = character()) {
  StParmInstance <- new_StParm(
    StParmName = StParmName,
    Type = Type,
    State = State,
    ThetaStParm = ThetaStParm,
    OmegaStParm = OmegaStParm,
    Covariates = Covariates,
    PMLStructure = PMLStructure
  )

  if (length(StParmInstance$ThetaStParm) == 0) {
    StParmInstance$ThetaStParm <-
      Theta(StParmName = StParmInstance$StParmName)
  }


  if (length(StParmInstance$OmegaStParm) == 0) {
    StParmInstance$OmegaStParm <-
      Omega(StParmName = StParmInstance$StParmName)
  }


  if (length(Covariates) > 0) {
    CovariatesNames <-
      sapply(StParmInstance$Covariates, function(x)
        x$Name)
    names(StParmInstance$Covariates) <- CovariatesNames
    for (CovariateInstance in StParmInstance$Covariates) {
      if (CovariateInstance$Type == "Occasion") {
        for (OmegaInstanceIndex in seq_along(CovariateInstance$Omegas)) {
          OmegaName <- CovariateInstance$Omegas[[OmegaInstanceIndex]]$Name
          if (nchar(OmegaName) > 3 &
              substr(OmegaName, 1, 3) == "n__") {
            NewOmegaName <- paste0(
              "n",
              StParmInstance$StParmName,
              CovariateInstance$Name,
              substring(OmegaName, 4)
            )
            StParmInstance$Covariates[[CovariateInstance$Name]]$Omegas[[OmegaInstanceIndex]]$Name <-
              NewOmegaName
            names(StParmInstance$Covariates[[CovariateInstance$Name]]$Omegas[OmegaInstanceIndex]) <-
              NewOmegaName
          }
        }
      } else {
        for (ThetaInstanceIndex in seq_along(CovariateInstance$Thetas)) {
          ThetaName <- CovariateInstance$Thetas[[ThetaInstanceIndex]]$Name
          if (nchar(ThetaName) > 3 &
              substr(ThetaName, 1, 3) == "d__") {
            NewThetaName <- paste0("d",
                                   StParmInstance$StParmName,
                                   substring(ThetaName, 4))
            StParmInstance$Covariates[[CovariateInstance$Name]]$Thetas[[ThetaInstanceIndex]]$Name <-
              NewThetaName
            names(StParmInstance$Covariates[[CovariateInstance$Name]]$Thetas[ThetaInstanceIndex]) <-
              NewThetaName

          }
        }
      }
    }
  }

  if (.check_0nzchar(StParmInstance$PMLStructure)) {
    StParmInstance <-
      .assign_TextToVariable(ParmList = StParmInstance, "PMLStructure", StParmInstance$PMLStructure)
  }

  if (.check_0nzchar(StParmInstance$StParmName)) {
    StParmInstance <-
      .assign_TextToVariable(ParmList = StParmInstance, "StParmName", StParmInstance$StParmName)
  }

  validate_StParm(StParmInstance)
}

.harmonize_StParmCovariateThetaOmega <-
  function(StParmInstance, CovariateInstance) {
    if (CovariateInstance$Type == "Occasion") {
      for (OmegaInstanceIndex in seq_along(CovariateInstance$Omegas)) {
        OmegaName <- CovariateInstance$Omegas[[OmegaInstanceIndex]]$Name
        if (nchar(OmegaName) > 3 &
            substr(OmegaName, 1, 3) == "n__") {
          NewOmegaName <- paste0("n",
                                 StParmInstance$StParmName,
                                 substring(OmegaName, 4))
          StParmInstance$Covariates[[CovariateInstance$Name]]$Omegas[[OmegaInstanceIndex]]$Name <-
            NewOmegaName
          names(StParmInstance$Covariates[[CovariateInstance$Name]]$Omegas[OmegaInstanceIndex]) <-
            NewOmegaName
        }
      }
    } else {
      for (ThetaInstanceIndex in seq_along(CovariateInstance$Thetas)) {
        ThetaName <- CovariateInstance$Thetas[[ThetaInstanceIndex]]$Name
        if (nchar(ThetaName) > 3 &
            substr(ThetaName, 1, 3) == "d__") {
          NewThetaName <- paste0("d",
                                 StParmInstance$StParmName,
                                 substring(ThetaName, 4))
          StParmInstance$Covariates[[CovariateInstance$Name]]$Thetas[[ThetaInstanceIndex]]$Name <-
            NewThetaName
          names(StParmInstance$Covariates[[CovariateInstance$Name]]$Thetas[ThetaInstanceIndex]) <-
            NewThetaName

        }
      }
    }

    StParmInstance
  }

.assign_TextToVariable <-
  function(ParmList, VariableName, VariableText) {
    if (!is.list(ParmList)            |
        length(ParmList) == 0         |
        !.check_0nzchar(VariableName) |
        !.check_0nzchar(VariableText))
      return(ParmList)

    for (iElement in 1:length(ParmList)) {
      if (is.list(ParmList[[iElement]])) {
        ParmList[[iElement]] <-
          .assign_TextToVariable(ParmList[[iElement]], VariableName, VariableText)
      }

      if (length(names(ParmList[iElement])) == 0)
        next

      if (names(ParmList[iElement]) == VariableName) {
        ParmList[iElement] <- VariableText
      }
    }

    ParmList
  }

.modify_OmegaExpression <-
  function(x, FullOmegaExpression, OmegaText) {
    if (length(OmegaText) == 0) {
      return("")
    }

    # blank should remain blank
    FullOmegaExpression[OmegaText == ""] <- ""

    if (length(OmegaText) > 1) {
      FullOmegaExpression <-
        add_TokensNLME(
          TokenName = paste("", #x$PMLStructure, x$StParmName,
                            x$OmegaStParm$Name,
                            sep = "_"),
          ListElementName = "Ranefs",
          TokenValues = FullOmegaExpression,
          DoNotChangeTokenListMain = FALSE
        )
    }

    FullOmegaExpression
  }

#' @noRd
#' @keywords internal NLME
output.StParm <- function(x, ...) {
  if (x$State == "None") {
    return("")
  }

  x <-
    .assign_TextToVariable(ParmList = x, "PMLStructure", x$PMLStructure)
  x <-
    .assign_TextToVariable(ParmList = x, "StParmName", x$StParmName)

  StParmTexts <- c()

  for (Type in x$Type) {
    StParmOneType <- x
    StParmOneType$Type <- Type
    StParmText <- ouptut_StParmOneType(StParmOneType)
    StParmTexts <- c(StParmTexts, StParmText)
  }

  if (x$State == "Searched") {
    StParmTexts <- c("", StParmTexts)
  }

  if (length(StParmTexts) > 1) {
    StParmTexts <-
      add_TokensNLME(
        TokenName = paste("", #x$PMLStructure,
                          x$StParmName,
                          sep = "_"),
        ListElementName = "StParm",
        TokenValues = StParmTexts,
        DoNotChangeTokenListMain = FALSE
      )
  }

  StParmTexts
}

#' @export
print.StParm <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}

output <- function(x) {
  UseMethod("output")
}

#' @export
output.NULL <- function(x, ...) {
  return("")
}

ouptut_StParmOneType <- function(x) {
  OmegaCovariateRelation <- " + "
  if (x$Type == "LogNormal") {
    CovariateCovThetaRelation <- "^"
    CovariateCentering <- "/"
    ThetaCovariateRelation <- " * "
  } else {
    CovariateCovThetaRelation <- "*"
    CovariateCentering <- "-"
    ThetaCovariateRelation <- " + "
  }

  ThetaSum <- x$ThetaStParm$Name
  if (x$OmegaStParm$State == "None") {
    OmegaText <- ""
    Ranefs <- ""
  } else if (x$OmegaStParm$State == "Present") {
    OmegaText <-  x$OmegaStParm$Name
    Ranefs <- output(x$OmegaStParm)
  } else {
    # searched
    OmegaText <- c("", x$OmegaStParm$Name)
    OmegaPresented <- x$OmegaStParm
    OmegaPresented$State <- "Present"
    Ranefs <- c("", output(OmegaPresented))
  }

  Fixefs <- output(x$ThetaStParm)

  for (CovariateInstance in x$Covariates) {
    CurrentFixef <- c()
    # CurrentRanef <- c()

    if (CovariateInstance$State == "None")
      next

    if (CovariateInstance$Type == "Continuous") {
      if (CovariateInstance$Center != "None") {
        CenteredCovariate <-
          paste0(
            "(",
            CovariateInstance$Name,
            CovariateCentering,
            tolower(CovariateInstance$Center)
          )

        if (CovariateInstance$Center %in% c("Median", "Mean")) {
          CenteredCovariate <-
            paste0(CenteredCovariate, "(", CovariateInstance$Name, ")")
        }

        CenteredCovariate <-
          paste0(CenteredCovariate, ")")
      } else {
        CenteredCovariate <- CovariateInstance$Name
      }

      CovariateText <-
        paste0(
          ThetaCovariateRelation,
          CenteredCovariate,
          CovariateCovThetaRelation,
          CovariateInstance$Thetas[[1]]$Name
        )

      CurrentFixef <- output(CovariateInstance$Thetas[[1]])
    } else if (CovariateInstance$Type == "Categorical") {
      CovariateText <- ""
      CurrentFixef < c()
      for (CategoryIndex in seq_along(CovariateInstance$Categories)) {
        if (CategoryIndex == 1)
          next

        CategoryText <-
          paste0(
            "(",
            CovariateInstance$Name,
            "==",
            CovariateInstance$Categories[CategoryIndex],
            ")*",
            CovariateInstance$Thetas[[CategoryIndex - 1]]$Name
          )
        if (x$Type == "LogNormal") {
          CategoryText <- paste0("exp(", CategoryText, ")")
        }

        CovariateText <-
          paste0(CovariateText, ThetaCovariateRelation, CategoryText)

        CurrentFixef <-
          paste(CurrentFixef,
                output(CovariateInstance$Thetas[[CategoryIndex - 1]]),
                sep = "\n\t")
      }

    } else if (CovariateInstance$Type == "Occasion") {
      if (CovariateInstance$State == "Searched") {
        # need to double records
        OmegaText <- c(OmegaText, OmegaText)
        Ranefs <- c(Ranefs, Ranefs)
        OmegaTokenIndicesToAdd <-
          (length(OmegaText) / 2 + 1):length(OmegaText)
      } else {
        OmegaTokenIndicesToAdd <- 1:length(OmegaText)
      }

      for (OmegaTokenIndex in OmegaTokenIndicesToAdd) {
        for (CategoryIndex in seq_along(CovariateInstance$Categories)) {
          CategoryText <-
            paste0(
              "(",
              CovariateInstance$Name,
              "==",
              CovariateInstance$Categories[CategoryIndex],
              ")*",
              CovariateInstance$Omegas[[CategoryIndex]]$Name
            )

          if (OmegaText[OmegaTokenIndex] == "" &&
              CategoryIndex == 1) {
            # no omegas, but occasion
            OmegaText[OmegaTokenIndex] <- CategoryText
          } else {
            OmegaText[OmegaTokenIndex] <-
              paste0(OmegaText[OmegaTokenIndex], OmegaCovariateRelation, CategoryText)
          }

          if (CategoryIndex == 1) {
            Ranefs[OmegaTokenIndex] <-
              paste(Ranefs[OmegaTokenIndex],
                    output(CovariateInstance$Omegas[[CategoryIndex]]),
                    sep = "\n\t")
            # need to concatenate with same()
            Ranefs[OmegaTokenIndex] <-
              substr(Ranefs[OmegaTokenIndex], 1, nchar(Ranefs[OmegaTokenIndex]) - 1)
          } else {
            Ranefs[OmegaTokenIndex] <- paste(Ranefs[OmegaTokenIndex],
                                             paste0(", same(", CovariateInstance$Omegas[[CategoryIndex]]$Name, ")"))
          }
        }

        Ranefs[OmegaTokenIndex] <-
          paste0(Ranefs[OmegaTokenIndex], ")")
      }


    } else {
      stop("Unknown ", CovariateInstance$Type)
    }

    if (CovariateInstance$Type != "Occasion") {
      if (CovariateInstance$State == "Searched") {
        CovariateText <-
          add_TokensNLME(
            TokenName = paste(#CovariateInstance$PMLStructure,
              x$StParmName,
              CovariateInstance$Name,
              sep = "_"),
            ListElementName = "CovThetaInStParm",
            TokenValues = c("", CovariateText),
            DoNotChangeTokenListMain = FALSE
          )

        CurrentFixef <-
          add_TokensNLME(
            TokenName = paste(#CovariateInstance$PMLStructure,
              x$StParmName,
              CovariateInstance$Name,
              sep = "_"),
            ListElementName = "CovThetaInFixef",
            TokenValues = c("", CurrentFixef),
            DoNotChangeTokenListMain = FALSE
          )
      }

      ThetaSum <- paste(ThetaSum, CovariateText)
      Fixefs <- paste(Fixefs, CurrentFixef, sep = "\n\t")
    }
    #   if (CovariateInstance$State == "Searched") {
    #     CovariateText <-
    #       add_TokensNLME(
    #         TokenName = paste(
    #           #CovariateInstance$PMLStructure,
    #           x$StParmName,
    #           CovariateInstance$Name,
    #           sep = "_"
    #         ),
    #         ListElementName = "CovOmegaInStParm",
    #         TokenValues = c("", CovariateText),
    #         DoNotChangeTokenListMain = FALSE
    #       )
    #
    #     CurrentRanef <-
    #       add_TokensNLME(
    #         TokenName = paste(
    #           #CovariateInstance$PMLStructure,
    #           x$StParmName,
    #           CovariateInstance$Name,
    #           sep = "_"
    #         ),
    #         ListElementName = "CovOmegaInRanef",
    #         TokenValues = c("", CurrentRanef),
    #         DoNotChangeTokenListMain = FALSE
    #       )
    #   }
    #
    #   OmegaText <- paste(OmegaText, CovariateText)
    #   Ranefs <- paste(Ranefs, CurrentRanef, sep = "\n\t")
    # }
  }

  if (x$Type == "LogNormal") {
    # stparm(V = tvV * wt^dVdwt * exp(nV + nVx0*(sex==0) + nVx1*(sex==1)))
    FullOmegaExpression <- paste("* exp(", OmegaText, ")")

    FullOmegaExpression <-
      .modify_OmegaExpression(x, FullOmegaExpression, OmegaText)

    StParmText <- paste(ThetaSum, FullOmegaExpression)
  } else if (x$Type == "Normal") {
    # stparm(V = tvV + wt*dVdwt + nV + nVx0*(sex==0) + nVx1*(sex==1))
    FullOmegaExpression <- paste("+", OmegaText)
    FullOmegaExpression[OmegaText == ""] <- ""
    FullOmegaExpression <-
      .modify_OmegaExpression(x, FullOmegaExpression, OmegaText)

    StParmText <- paste(ThetaSum, FullOmegaExpression)
  } else if (x$Type == "LogNormal1") {
    # stparm(V = (tvV + wt*dVdwt) * exp(nV + nVx0*(sex==0) + nVx1*(sex==1)))
    FullOmegaExpression <- paste("* exp(", OmegaText, ")")

    FullOmegaExpression <-
      .modify_OmegaExpression(x, FullOmegaExpression, OmegaText)

    StParmText <- paste("(", ThetaSum, ")", FullOmegaExpression)
  } else if (x$Type == "LogNormal2") {
    # stparm(V = exp(tvV + wt*dVdwt + nV + nVx0*(sex==0) + nVx1*(sex==1)))
    FullOmegaExpression <- paste("+", OmegaText)

    FullOmegaExpression <-
      .modify_OmegaExpression(x, FullOmegaExpression, OmegaText)

    StParmText <- paste("exp(", ThetaSum, FullOmegaExpression, ")")
  } else if (x$Type == "LogitNormal") {
    # stparm(V = ilogit(tvV + wt*dVdwt + nV + nVx0*(sex==0) + nVx1*(sex==1)))
    FullOmegaExpression <- paste("+", OmegaText)

    FullOmegaExpression <-
      .modify_OmegaExpression(x, FullOmegaExpression, OmegaText)

    StParmText <-
      paste("ilogit(", ThetaSum, FullOmegaExpression, ")")
  }

  StParmText <-
    paste0("stparm(", x$StParmName, " = ", StParmText, ")")

  if (length(Ranefs) > 1) {
    Ranefs <- add_TokensNLME(
      TokenName = paste("", #x$PMLStructure, x$StParmName,
                        x$OmegaStParm$Name,
                        sep = "_"),
      ListElementName = "Ranefs",
      TokenValues = Ranefs,
      DoNotChangeTokenListMain = FALSE
    )
  }

  StParmText <- paste(StParmText,
                      Fixefs,
                      Ranefs,
                      sep = "\n\t")

  StParmText
}
