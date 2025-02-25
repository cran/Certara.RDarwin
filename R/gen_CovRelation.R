check_Presence <- function(CurrentCovDF) {
  Presence <- unique(CurrentCovDF$Presence)
  if (length(Presence) > 1) {
    stop(
      "For covariate ",
      unique(CurrentCovDF$Covariate),
      " Presence with Parameter ",
      unique(CurrentCovDF$Parameter),
      " is specified umbiguously: ",
      paste(Presence, sep = "/")
    )
  }

  Presence
}

gen_ContCovRelation <- function(CurrentCovDF) {
  Relations <- c("exponential", "power", "linear")
  CovRelationCode <- pmatch(tolower(CurrentCovDF$CovarRelation),
                            Relations)
  stopifnot(!is.na(CovRelationCode))
  CovRelation <- Relations[CovRelationCode]
  CovThetaName <-
    paste0("THETA(",
           CurrentCovDF$Parameter[1],
           "~",
           CurrentCovDF$Covariate[1],
           ")")
  if (CovRelation == "exponential") {
    Centering <- ifelse(is.na(CurrentCovDF$Center),
                        "",
                        paste0(" - ", CurrentCovDF$Center))
    CovCurrentParamString <-
      paste0("*EXP(",
             CovThetaName,
             "*(",
             CurrentCovDF$Covariate,
             Centering,
             "))")
  } else if (CovRelation == "power") {
    Centering <- ifelse(is.na(CurrentCovDF$Center),
                        "",
                        paste0("/", CurrentCovDF$Center))

    CovCurrentParamString <-
      paste0("*(",
             CurrentCovDF$Covariate,
             Centering,
             ")**(",
             CovThetaName,
             ")")
  } else if (CovRelation == "linear") {
    Centering <- ifelse(is.na(CurrentCovDF$Center),
                        "",
                        paste0(" - ", CurrentCovDF$Center))
    CovCurrentParamString <-
      paste0("*(1 + ",
             CovThetaName,
             " * (",
             CurrentCovDF$Covariate,
             Centering,
             "))")
  } else {
    CovCurrentParamString <- ""
  }

  CovCurrentParamString
}

gen_CatCovRelation <- function(CurrentCovDF) {
  Relations <- c("exponential", "linear")
  CovaRelation <- unique(tolower(CurrentCovDF$CovarRelation))
  if (length(CovaRelation) > 1) {
    stop(
      "For covariate ",
      unique(CurrentCovDF$Covariate),
      " relationship with Parameter ",
      unique(CurrentCovDF$Parameter),
      " is specified umbiguously: ",
      paste(CovaRelation, sep = "/")
    )
  }
  CovRelationCode <- pmatch(tolower(CovaRelation),
                            Relations)
  stopifnot(!is.na(CovRelationCode))
  CovRelation <- Relations[CovRelationCode]
  CurrentCatCovPrep <- ""
  for (Row in 1:nrow(CurrentCovDF)) {
    CovThetaName <-
      paste0(
        "THETA(",
        CurrentCovDF$Parameter[Row],
        "~",
        CurrentCovDF$Covariate[Row],
        CurrentCovDF$Category[Row],
        ")"
      )

    if (Row == 1) {
      TypicalValueInit <-
        paste0("TV",
               CurrentCovDF$Parameter[Row],
               CurrentCovDF$Covariate[Row],
               " = 0\n")
    } else {
      TypicalValueInit <- ""
    }

    CurrentCatCovPrep <- paste0(
      CurrentCatCovPrep,
      "\n",
      TypicalValueInit,
      "IF(",
      CurrentCovDF$Covariate[Row],
      " == ",
      CurrentCovDF$Category[Row],
      ") THEN",
      "\n\tTV",
      CurrentCovDF$Parameter[Row],
      CurrentCovDF$Covariate[Row],
      " = ",
      CovThetaName,
      "\nENDIF\n"
    )

    if (Row != 1)
      next()
    if (CovRelation == "exponential") {
      CovCurrentParamString <-
        paste0("*EXP(TV",
               CurrentCovDF$Parameter[Row],
               CurrentCovDF$Covariate[Row],
               ")")
    } else if (CovRelation == "linear") {
      CovCurrentParamString <-
        paste0("*(1 + TV",
               CurrentCovDF$Parameter[Row],
               CurrentCovDF$Covariate[Row],
               ")")
    } else {
      CovCurrentParamString <- ""
    }
  }

  list(CovCurrentParamString = CovCurrentParamString,
       CurrentCatCovPrep = CurrentCatCovPrep)
}

gen_ContCovMultRelations <-
  function(CovCurrentParamString,
           ContinDF,
           TokensListMain) {
    for (Covariate in unique(ContinDF$Covariate)) {
      CurrentCovDF <- ContinDF[ContinDF$Covariate == Covariate,]
      if (nrow(CurrentCovDF) == 0)
        next()

      Presence <- check_Presence(CurrentCovDF)

      if (Presence == 1) {
        if (nrow(CurrentCovDF) != 1) {
          stop(paste("Multiple relationships are not supported when Presence == 1",
                     "Covariate:", Covariate,
                     "Parameter:", CurrentCovDF$Parameter[1]))
        }

        CovCurrentParamString <- paste0(CovCurrentParamString,
                                        gen_ContCovRelation(CurrentCovDF))

      } else if (Presence == 2) {
        TokenThetaCovName <-
          paste0(CurrentCovDF$Parameter[1],
                 "~",
                 CurrentCovDF$Covariate[1])

        CovThetaTokens <- c("")
        # if Presence == 2, multiple relationships are possible
        for (Relationship in unique(CurrentCovDF$CovarRelation)) {
          CurrentRelationCurrentCovDF <-
            CurrentCovDF[CurrentCovDF$CovarRelation == Relationship,]
          CovThetaTokens <-
            c(CovThetaTokens,
              gen_ContCovRelation(CurrentRelationCurrentCovDF))
        }

        DoNotChangeTokenListMain <- FALSE
        TokenValue <-
          paste_Token(
            TokenName = TokenThetaCovName,
            TokenAddress = TokenThetaCovName,
            ListElementName = "PK",
            TokenValues = CovThetaTokens,
            TokensListMain,
            DoNotChangeTokenListMain = DoNotChangeTokenListMain
          )

        CovCurrentParamString <-
          paste(CovCurrentParamString, TokenValue)
      }
    }

    assign("TokensListMain", TokensListMain, envir = parent.frame())
    CovCurrentParamString
  }

gen_CatCovMultRelations <-
  function(CovCurrentParamString,
           CatCovPrep,
           CategoricalDF,
           TokensListMain) {
    for (Covariate in unique(CategoricalDF$Covariate)) {
      CurrentCovDF <-
        CategoricalDF[CategoricalDF$Covariate == Covariate,]

      if (nrow(CurrentCovDF) == 0)
        next()

      Presence <- check_Presence(CurrentCovDF)

      if (Presence == 1) {
        gen_CatCovRelationResult <-
          gen_CatCovRelation(CurrentCovDF)
        CatCovPrep <-
          paste0(CatCovPrep,
                 gen_CatCovRelationResult$CurrentCatCovPrep)
        CovCurrentParamString <-
          paste0(CovCurrentParamString,
                 gen_CatCovRelationResult$CovCurrentParamString)

      } else if (Presence == 2) {
        TokenThetaCovName <-
          paste0(CurrentCovDF[1, "Parameter"],
                 "~",
                 CurrentCovDF[1, "Covariate"])

        CatCovPrepTokens <- c("")
        CovThetaTokens <- c("")
        # if Presence == 2, multiple relationships are possible
        for (Relationship in unique(CurrentCovDF$CovarRelation)) {
          CurrentRelationCurrentCovDF <-
            CurrentCovDF[CurrentCovDF$CovarRelation == Relationship,]
          gen_CatCovRelationResult <-
            gen_CatCovRelation(CurrentRelationCurrentCovDF)
          CatCovPrepTokens <-
            c(CatCovPrepTokens,
              gen_CatCovRelationResult$CurrentCatCovPrep)
          CovThetaTokens <-
            c(CovThetaTokens,
              gen_CatCovRelationResult$CovCurrentParamString)
        }

        DoNotChangeTokenListMain <- FALSE
        CovPrepToken <-
          paste_Token(
            TokenName = TokenThetaCovName,
            TokenAddress = TokenThetaCovName,
            ListElementName = "PK",
            TokenValues = CatCovPrepTokens,
            TokensListMain,
            DoNotChangeTokenListMain = DoNotChangeTokenListMain
          )

        CatCovPrep <-
          paste(CatCovPrep, "\n", CovPrepToken)

        CovInParamToken <-
          paste_Token(
            TokenName = TokenThetaCovName,
            TokenAddress = TokenThetaCovName,
            ListElementName = "PK",
            TokenValues = CovThetaTokens,
            TokensListMain,
            DoNotChangeTokenListMain = DoNotChangeTokenListMain
          )

        CovCurrentParamString <-
          paste(CovCurrentParamString, CovInParamToken)
      }
    }
    assign("TokensListMain", TokensListMain, envir = parent.frame())
    list(CatCovPrep = CatCovPrep, CovCurrentParamString = CovCurrentParamString)
  }

gen_CovRelation <-
  function(ModelDF,
           ObligatoryParameter,
           TokensListMain,
           Advan) {
    CatCovPrep <- ""
    CovCurrentParamString <- ""
    CovDF <- ModelDF[paste(ModelDF$ADVAN, ModelDF$TRANS) == Advan &
                       ModelDF$Parameter == ObligatoryParameter &
                       !is.na(ModelDF$Covariate), ]
    # if there are covariates bound to this parameter
    if (nrow(CovDF)) {
      # different ways for categorical and continuous
      ContinDF <- CovDF[is.na(CovDF$Category),]
      CategoricalDF <- CovDF[!is.na(CovDF$Category),]
      if (nrow(ContinDF) > 0) {
        CovCurrentParamString <-
          gen_ContCovMultRelations(CovCurrentParamString, ContinDF, TokensListMain)
      }

      if (nrow(CategoricalDF) > 0) {
        CovCurrentParamStringResult <-
          gen_CatCovMultRelations(CovCurrentParamString,
                                  CatCovPrep,
                                  CategoricalDF,
                                  TokensListMain)
        CatCovPrep <- CovCurrentParamStringResult$CatCovPrep
        CovCurrentParamString <-
          CovCurrentParamStringResult$CovCurrentParamString
      }
    }

    assign("TokensListMain", TokensListMain, envir = parent.frame())
    list(CatCovPrep = CatCovPrep, CovCurrentParamString = CovCurrentParamString)
  }

gen_ContCovThetaInit <-
  function(ModelDF,
           ObligatoryParameter,
           TokensListMain,
           Advan) {
    CovCurrentParamString <- ""
    ContinDF <-
      ModelDF[paste(ModelDF$ADVAN, ModelDF$TRANS) == Advan &
                ModelDF$Parameter == ObligatoryParameter &
                !is.na(ModelDF$Covariate) &
                is.na(ModelDF$Category),]
    # if there are covariates bound to this parameter

    if (nrow(ContinDF) > 0) {
      for (Covariate in unique(ContinDF$Covariate)) {
        CurrentCovDF <- ContinDF[ContinDF$Covariate == Covariate, ]
        Presence <- check_Presence(CurrentCovDF)
        if (Presence == 1) {
          CovCurrentParamString <-
            paste0(
              CovCurrentParamString,
              "\n",
              gen_ThetaInitials(
                CurrentCovDF$LowerBound,
                CurrentCovDF$InitEst,
                CurrentCovDF$UpperBound,
                CurrentCovDF$ThetaFrozen
              ),
              "; ",
              ObligatoryParameter,
              "~",
              Covariate
            )

        } else if (Presence == 2) {
          CovThetaTokens <- c("")
          # if Presence == 2, multiple relationships are possible
          for (Relationship in unique(CurrentCovDF$CovarRelation)) {
            CurrentRelationCurrentCovDF <-
              CurrentCovDF[CurrentCovDF$CovarRelation == Relationship,]

            CovThetaToken <-
              paste0(
                gen_ThetaInitials(
                  CurrentRelationCurrentCovDF$LowerBound,
                  CurrentRelationCurrentCovDF$InitEst,
                  CurrentRelationCurrentCovDF$UpperBound,
                  CurrentRelationCurrentCovDF$ThetaFrozen
                ),
                "; ",
                ObligatoryParameter,
                "~",
                Covariate
              )

            CovThetaTokens <-
              c(CovThetaTokens,
                CovThetaToken)
          }

          TokenThetaCovName <-
            paste0(CurrentCovDF$Parameter[1],
                   "~",
                   CurrentCovDF$Covariate[1])
          DoNotChangeTokenListMain <- FALSE
          TokenValue <-
            paste_Token(
              TokenName = TokenThetaCovName,
              TokenAddress = TokenThetaCovName,
              ListElementName = "PK",
              TokenValues = CovThetaTokens,
              TokensListMain,
              DoNotChangeTokenListMain = DoNotChangeTokenListMain
            )
          CovCurrentParamString <-
            paste0(CovCurrentParamString, "\n",
                   TokenValue)
        }
      }
    }

    assign("TokensListMain", TokensListMain, envir = parent.frame())
    CovCurrentParamString
  }

gen_CatCovThetaInit <-
  function(ModelDF,
           ObligatoryParameter,
           TokensListMain,
           Advan) {
    CovCurrentParamString <- ""
    CategoricalDF <-
      ModelDF[paste(ModelDF$ADVAN, ModelDF$TRANS) == Advan &
                ModelDF$Parameter == ObligatoryParameter &
                !is.na(ModelDF$Covariate) &
                !is.na(ModelDF$Category),]
    # if there are covariates bound to this parameter
    if (nrow(CategoricalDF) > 0) {
      for (Covariate in unique(CategoricalDF$Covariate)) {
        CurrentCovDF <-
          CategoricalDF[CategoricalDF$Covariate == Covariate, ]
        CatCovInits <- c()
        # adding inits for different relations
        # not supported by Presence == 1
        for (Relation in unique(CurrentCovDF$CovarRelation)) {
          CurrentRelationCurrentCovDF <-
            CurrentCovDF[CurrentCovDF$CovarRelation == Relation, ]

          CatCovInitsCurRelation <- "\n"
          for (Row in 1:nrow(CurrentRelationCurrentCovDF)) {
            CatCovInitsCurRelation <- paste0(
              CatCovInitsCurRelation,
              gen_ThetaInitials(
                CurrentRelationCurrentCovDF$LowerBound[Row],
                CurrentRelationCurrentCovDF$InitEst[Row],
                CurrentRelationCurrentCovDF$UpperBound[Row],
                CurrentRelationCurrentCovDF$ThetaFrozen[Row]
              ),
              ";",
              ObligatoryParameter,
              "~",
              Covariate,
              CurrentRelationCurrentCovDF$Category[Row],
              "\n"
            )
          }

          CatCovInits <- c(CatCovInits, CatCovInitsCurRelation)
        }

        # taking the first row only
        if (CurrentCovDF[1, "Presence"] == 1) {
          CovCurrentParamString <-
            paste0(CovCurrentParamString,
                   CatCovInits)
        } else if (CurrentCovDF[1, "Presence"] == 2) {
          CovThetaTokens <-
            c("", CatCovInits)
          TokenThetaCovName <-
            paste0(CurrentCovDF[1, "Parameter"],
                   "~",
                   CurrentCovDF[1, "Covariate"])
          DoNotChangeTokenListMain <- FALSE
          TokenValue <-
            paste_Token(
              TokenName = TokenThetaCovName,
              TokenAddress = TokenThetaCovName,
              ListElementName = "PK",
              TokenValues = CovThetaTokens,
              TokensListMain,
              DoNotChangeTokenListMain = DoNotChangeTokenListMain
            )

          CovCurrentParamString <-
            paste0(CovCurrentParamString, "\n",
                   TokenValue)
        }
      }
    }

    assign("TokensListMain", TokensListMain, envir = parent.frame())
    CovCurrentParamString
  }
