#' @noRd
#' @keywords internal NONMEM
gen_ThetaInitials <- function(Low,
                              Init,
                              High,
                              Frozen) {
  if (!is.na(High) && is.na(Low)) {
    stop("If an  upper  bound  is used, a lower bound must also be used")
  }

  if (!is.na(Frozen) && Frozen == 1) {
    paste(Init, "FIXED")
  } else {
    paste0(
      "(",
      ifelse(is.na(Low), "", paste0(Low, ", ")),
      ifelse(is.na(Init), "", Init),
      ifelse(is.na(High), "", paste0(", ", High)),
      ")"
    )
  }
}

#' @noRd
#' @keywords internal NONMEM
add_ThetaPart <-
  function(ModelTemplate,
           TokensListMain,
           ModelDF,
           UniqueAdvans,
           UniqueObligatoryParameters) {
    ModelTemplate["THETAHEAD"] <- "$THETA"
    ThetaRowsCount <- 0
    for (ObligatoryParameter in UniqueObligatoryParameters) {
      if (ObligatoryParameter == "RATE")
        next()

      TokenValues <- c()
      CurrentParamDF <-
        ModelDF[ModelDF$Parameter == ObligatoryParameter &
                  ModelDF$ThetaName == ObligatoryParameter &
                  is.na(ModelDF$Covariate), ]
      for (Advan in UniqueAdvans) {
        CurrentParamAdvanDF <-
          CurrentParamDF[paste(CurrentParamDF$ADVAN, CurrentParamDF$TRANS) == Advan, ]
        ParamInDFPresent <- CurrentParamAdvanDF$Presence
        if (length(ParamInDFPresent) == 0 ||
            ParamInDFPresent == 0) {
          TokenValues <-
            c(TokenValues,
              paste(";", ObligatoryParameter, "is not used"))
        } else if (ParamInDFPresent == 1) {
          TokenValue <-
            paste(
              gen_ThetaInitials(
                CurrentParamAdvanDF$LowerBound,
                CurrentParamAdvanDF$InitEst,
                CurrentParamAdvanDF$UpperBound,
                CurrentParamAdvanDF$ThetaFrozen
              ),
              ";",
              ObligatoryParameter
            )

          TokenValue <-
            paste(
              gen_CatCovThetaInit(ModelDF, ObligatoryParameter, TokensListMain, Advan),
              TokenValue,
              gen_ContCovThetaInit(ModelDF, ObligatoryParameter, TokensListMain, Advan)
            )

          TokenValues <- c(TokenValues, TokenValue)
        } else if (ParamInDFPresent == 2) {
          TokenThetaValue <-
            paste(
              gen_ThetaInitials(
                CurrentParamAdvanDF$LowerBound,
                CurrentParamAdvanDF$InitEst,
                CurrentParamAdvanDF$UpperBound,
                CurrentParamAdvanDF$ThetaFrozen
              ),
              ";",
              ObligatoryParameter
            )

          TokenThetaValue <-
            paste0(
              gen_CatCovThetaInit(ModelDF, ObligatoryParameter, TokensListMain, Advan),
              TokenThetaValue,
              gen_ContCovThetaInit(ModelDF, ObligatoryParameter, TokensListMain, Advan)
            )

          TokenThetaValues <-
            c(paste(";", ObligatoryParameter, "is not used"),
              TokenThetaValue)

          TokenThetaName <- paste0(ObligatoryParameter, "_THETA")
          # DoNotChangeTokenListMain <-
          #   "ThetaInit" %in% names(TokensListMain[[TokenThetaName]][[1]])

          if (grepl("(^R\\d$)|(^D\\d$)", ObligatoryParameter)) {
            TokenValue <- paste_Token(
              TokenName = "RATE",
              TokenAddress = "RATE",
              ListElementName = "ThetaInit",
              TokenValues = TokenThetaValues,
              TokensListMain,
              DoNotChangeTokenListMain = FALSE
            )
          } else {
            TokenValue <- paste_Token(
              TokenName = TokenThetaName,
              TokenAddress = TokenThetaName,
              ListElementName = "ThetaInit",
              TokenValues = TokenThetaValues,
              TokensListMain,
              DoNotChangeTokenListMain = FALSE
            )
          }

          TokenValues <- c(TokenValues, TokenValue)
        }
      }

      ThetaRowsCount <- ThetaRowsCount + 1
      ModelTemplate[paste0("Theta", ThetaRowsCount)] <-
        paste(
          paste_Token(
            TokenName = "ADVAN",
            TokenAddress = c("ADVAN"),
            ListElementName = "ThetaInit",
            TokenValues = TokenValues,
            TokensListMain
          ),
          #";", ObligatoryParameter,
          collapse = " "
        )
    }

    assign("TokensListMain", TokensListMain, envir = parent.frame())

    ModelTemplate
  }
