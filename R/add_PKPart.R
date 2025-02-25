check_RATEPRESENT <- function(ModelDF, Advan) {
  CurrentAdvanDF <- ModelDF[paste(ModelDF$ADVAN, ModelDF$TRANS) == Advan & ModelDF$Presence > 0,]
  RateParametersPresent <- grepl("(^R\\d$)|(^D\\d$)", CurrentAdvanDF$ThetaName)
  if (any(RateParametersPresent)) {
    CurrentAdvanDFRateParamsPresence <- CurrentAdvanDF[RateParametersPresent, "Presence"]
    RatePresence <- CurrentAdvanDF$Presence[CurrentAdvanDF$ThetaName == "RATE"]
    RateParamsPresenceEqual <- CurrentAdvanDFRateParamsPresence == RatePresence
    if (any(!RateParamsPresenceEqual)) {
      RateParametersNotEqual <- CurrentAdvanDF[RateParametersPresent, "Parameter"][!RateParamsPresenceEqual]
      stop("For ", Advan, " parameter(s) bound to RATE: ", RateParametersNotEqual, " should have the same Presence as RATE does")
    }
  }}

add_VtoToken <- function(ObligatoryParameter, Advan, TokenValue) {
  if (substr(ObligatoryParameter, 1, 1) == "V") {
    # V scaling: SC = V
    # for the first V (for ADVAN4 and ADVAN 12 this is V2)
    if (ObligatoryParameter %in% c("V", "V1") ||
        (ObligatoryParameter == "V2" &&
         substr(Advan, 6, 7) %in% c("4 ", "12"))) {
      TokenValue <-
        paste(TokenValue,
              paste0("SC = ", ObligatoryParameter),
              sep = "\n")
    }
  }

  TokenValue
}

add_prepostCode <- function(CurrentParamAdvanDF, TokenValue) {
  if (length(CurrentParamAdvanDF$PreCode) > 0 &&
      !is.na(CurrentParamAdvanDF$PreCode) &&
      CurrentParamAdvanDF$PreCode != "") {
    TokenValue <- paste(CurrentParamAdvanDF$PreCode,
                        TokenValue,
                        sep = "\n")
  }

  if (length(CurrentParamAdvanDF$PostCode) > 0 &&
      !is.na(CurrentParamAdvanDF$PostCode) &&
      CurrentParamAdvanDF$PostCode != "") {
    TokenValue <- paste(TokenValue,
                        CurrentParamAdvanDF$PostCode,
                        sep = "\n")
  }

  TokenValue
}

add_PKPart <-
  function(ModelTemplate,
           TokensListMain,
           ModelDF,
           UniqueAdvans,
           UniqueObligatoryParameters) {
    ModelTemplate["PKHEAD"] <- "$PK"
    PKRowsCount <- 0

    for (ObligatoryParameter in UniqueObligatoryParameters) {
      TokenValues <- c()
      CurrentParamDF <-
        ModelDF[ModelDF$Parameter == ObligatoryParameter &
                  ModelDF$ThetaName == ObligatoryParameter &
                  is.na(ModelDF$Covariate),]

      for (Advan in UniqueAdvans) {
        CurrentParamAdvanDF <-
          CurrentParamDF[paste(CurrentParamDF$ADVAN, CurrentParamDF$TRANS) == Advan,]
        ParamInDFPresent <- CurrentParamAdvanDF$Presence
        stopifnot(length(ParamInDFPresent) <= 1)

        if (ObligatoryParameter == "RATE") {
          check_RATEPRESENT(ModelDF, Advan)
        }

        if (length(ParamInDFPresent) == 0 ||
            ParamInDFPresent == 0) {
          if (ObligatoryParameter == "RATE") {
            TokenValues <- c(TokenValues, "DROP")
          } else {
            TokenValues <-
              c(TokenValues,
                paste(";", ObligatoryParameter, "is not used"))
          }
        } else if (ParamInDFPresent == 1) {
          if (ObligatoryParameter == "RATE") {
            RateMap <-
              regmatches(ModelTemplate["INPUT"],
                         regexpr("(?<=\\{).*(?=\\})", ModelTemplate["INPUT"], perl = TRUE))

            TokenValues <- c(TokenValues, RateMap)
          } else {
            TokenValue <-
              paste0(ObligatoryParameter,
                     " = THETA(",
                     ObligatoryParameter,
                     ")")

            CovCurrentParamStringResult <-
              gen_CovRelation(ModelDF, ObligatoryParameter, TokensListMain, Advan)

            TokenValue <-
              paste0(
                CovCurrentParamStringResult$CatCovPrep,
                TokenValue,
                CovCurrentParamStringResult$CovCurrentParamString
              )

            if (CurrentParamAdvanDF$OmegaPresence == 1) {
              TokenValue <-
                paste0(TokenValue,
                       " * EXP(ETA(",
                       ObligatoryParameter,
                       "))")
            } else if (CurrentParamAdvanDF$OmegaPresence == 2) {
              TokenOmegaValues <-
                c("",
                  paste0(" * EXP(ETA(", ObligatoryParameter, "))"))

              if (grepl("(^R\\d$)|(^D\\d$)", ObligatoryParameter)) {
                TokenValue <- paste0(
                  TokenValue,
                  paste_Token(
                    TokenName = "RATE",
                    TokenAddress = "RATE",
                    ListElementName = "OmegaInit",
                    TokenValues = TokenOmegaValues,
                    TokensListMain
                  )
                )
              } else {
                TokenOmegaName <- paste0(ObligatoryParameter, "_OMEGA")
                DoNotChangeTokenListMain <-
                  length(TokensListMain[[TokenOmegaName]]) != 0

                TokenValue <- paste0(
                  TokenValue,
                  paste_Token(
                    TokenName = TokenOmegaName,
                    TokenAddress = TokenOmegaName,
                    ListElementName = "PK",
                    TokenValues = TokenOmegaValues,
                    TokensListMain,
                    DoNotChangeTokenListMain = DoNotChangeTokenListMain
                  )
                )
              }
            }

            # Add scale (Sc)
            TokenValue <-
              add_VtoToken(ObligatoryParameter, Advan, TokenValue)

            TokenValue <-
              add_prepostCode(CurrentParamAdvanDF, TokenValue)

            TokenValues <- c(TokenValues, TokenValue)
          }
        } else if (ParamInDFPresent == 2) {
          if (ObligatoryParameter == "RATE") {
            RateMap <-
              regmatches(ModelTemplate["INPUT"],
                         regexpr("(?<=\\{).*(?=\\})", ModelTemplate["INPUT"], perl = TRUE))
            TokenRateValues <- c("DROP", RateMap)
            DoNotChangeTokenListMain <-
              length(TokensListMain[["RATE"]]) != 0

            TokenValue <- paste_Token(
              TokenName = "RATE",
              TokenAddress = "RATE",
              ListElementName = "RATEMAP",
              TokenValues = TokenRateValues,
              TokensListMain,
              DoNotChangeTokenListMain = DoNotChangeTokenListMain
            )
          } else {
            if (CurrentParamAdvanDF$OmegaPresence == 2) {
              TokenOmegaName <- paste0(ObligatoryParameter, "_OMEGA")
              DoNotChangeTokenListMain <-
                length(TokensListMain[[TokenOmegaName]]) != 0
              TokenOmegaValues <-
                c("",
                  paste0(" * EXP(ETA(", ObligatoryParameter, "))"))
              EtaString <-
                paste_Token(
                  TokenName = TokenOmegaName,
                  TokenAddress = TokenOmegaName,
                  ListElementName = "PK",
                  TokenValues = TokenOmegaValues,
                  TokensListMain,
                  DoNotChangeTokenListMain = DoNotChangeTokenListMain
                )

            } else if (CurrentParamAdvanDF$OmegaPresence == 1) {
              EtaString <- paste0("* EXP(ETA(", ObligatoryParameter, "))")
            } else {
              EtaString <- ""
            }

            CovCurrentParamStringResult <-
              gen_CovRelation(ModelDF, ObligatoryParameter, TokensListMain, Advan)

            TokenThetaValue <- paste0(
              CovCurrentParamStringResult$CatCovPrep,
              ObligatoryParameter,
              " = THETA(",
              ObligatoryParameter,
              ") ",
              CovCurrentParamStringResult$CovCurrentParamString,
              EtaString
            )

            TokenThetaValue <-
              add_prepostCode(CurrentParamAdvanDF, TokenThetaValue)
            TokenThetaValues <- c("", TokenThetaValue)

            TokenThetaName <- paste0(ObligatoryParameter, "_THETA")
            DoNotChangeTokenListMain <-
              length(TokensListMain[[TokenThetaName]]) != 0

            if (grepl("(^R\\d$)|(^D\\d$)", ObligatoryParameter)) {
              TokenValue <- paste_Token(
                TokenName = "RATE",
                TokenAddress = "RATE",
                ListElementName = "RATEPARAMS",
                TokenValues = TokenThetaValues,
                TokensListMain,
                DoNotChangeTokenListMain = DoNotChangeTokenListMain
              )
            } else {
              TokenValue <- paste_Token(
                TokenName = TokenThetaName,
                TokenAddress = TokenThetaName,
                ListElementName = "PK",
                TokenValues = TokenThetaValues,
                TokensListMain,
                DoNotChangeTokenListMain = DoNotChangeTokenListMain
              )
            }
          }


          TokenValues <- c(TokenValues, TokenValue)
        }
      }

      if (ObligatoryParameter == "RATE") {
        check_RATEPRESENT(ModelDF, Advan)

        RateTokenName <- paste_Token(
          TokenName = "ADVAN",
          TokenAddress = c("ADVAN"),
          ListElementName = "RateMap",
          TokenValues = TokenValues,
          TokensListMain
        )

        ModelTemplate["INPUT"] <-
          gsub(
            pattern = paste0("{", RateMap, "}"),
            replacement = RateTokenName,
            x = ModelTemplate["INPUT"],
            fixed = TRUE
          )
      } else {
        PKRowsCount <- PKRowsCount + 1

        ModelTemplate[paste0("PK", PKRowsCount)] <-
          paste(
            paste_Token(
              TokenName = "ADVAN",
              TokenAddress = c("ADVAN"),
              ListElementName = "PK",
              TokenValues = TokenValues,
              TokensListMain
            ),
            ";",
            ObligatoryParameter,
            collapse = " "
          )
      }
    }

    assign("TokensListMain", TokensListMain, envir = parent.frame())

    ModelTemplate
  }
