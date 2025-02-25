#' @noRd
#' @keywords internal NONMEM
add_OmegaPart <- function(ModelTemplate,
                          TokensListMain,
                          ModelDF,
                          UniqueAdvans,
                          UniqueObligatoryParameters) {
  OmegaRowsCount <- 0
  PreviousOmegaBlock <- 0
  for (ObligatoryParameter in UniqueObligatoryParameters) {
    if (ObligatoryParameter == "RATE")
      next()
    TokenValues <- c()

    CurrentParamDF <-
      ModelDF[ModelDF$Parameter == ObligatoryParameter &
                ModelDF$ThetaName == ObligatoryParameter &
                is.na(ModelDF$Covariate),]
    for (Advan in UniqueAdvans) {
      CurrentParamAdvanDF <-
        CurrentParamDF[paste(CurrentParamDF$ADVAN, CurrentParamDF$TRANS) == Advan,]
      ParamInDFPresent <- CurrentParamAdvanDF$Presence
      if (length(ParamInDFPresent) == 0 ||
          ParamInDFPresent == 0 ||
          CurrentParamAdvanDF$OmegaPresence == 0) {
        TokenValues <-
          c(TokenValues,
            paste(";", ObligatoryParameter, "is not used"))
      } else if (CurrentParamAdvanDF$OmegaPresence == 1 &&
                 CurrentParamAdvanDF$Presence == 1) {
        # omega is presented and theta is not searchable
        TokenValue <-
          paste(
            CurrentParamAdvanDF$Omega,
            ifelse(CurrentParamAdvanDF$OmegaFrozen == 1, "FIXED", ""),
            ";",
            ObligatoryParameter
          )

        TokenValues <- c(TokenValues, TokenValue)
      } else if (CurrentParamAdvanDF$OmegaPresence == 2 &&
                 CurrentParamAdvanDF$Presence == 1) {
        # omega is searchable, theta is not
        TokenOmegaValue <- paste(
          CurrentParamAdvanDF$Omega,
          ifelse(CurrentParamAdvanDF$OmegaFrozen == 1, "FIXED", ""),
          ";",
          ObligatoryParameter
        )

        TokenOmegaValues <- c("",
                              TokenOmegaValue)

        TokenOmegaName <- paste0(ObligatoryParameter, "_OMEGA")

        if (grepl("(^R\\d$)|(^D\\d$)", ObligatoryParameter)) {
          TokenValue <- paste_Token(
            TokenName = "RATE",
            TokenAddress = "RATE",
            ListElementName = "OmegaInit",
            TokenValues = TokenOmegaValues,
            TokensListMain
          )
        } else {
          TokenValue <- paste_Token(
            TokenName = TokenOmegaName,
            TokenAddress = TokenOmegaName,
            ListElementName = "OmegaInit",
            TokenValues = TokenOmegaValues,
            TokensListMain
          )
        }

        TokenValues <- c(TokenValues, TokenValue)
      } else if (CurrentParamAdvanDF$OmegaPresence >= 1 &&
                 CurrentParamAdvanDF$Presence == 2) {
        # theta is searchable
        # omega is searchable
        TokenOmegaValue <-
          paste(
            CurrentParamAdvanDF$Omega,
            ifelse(CurrentParamAdvanDF$OmegaFrozen == 1, "FIXED", ""),
            paste(";", ObligatoryParameter)
          )

        if (CurrentParamAdvanDF$OmegaPresence == 2) {
          TokenOmegaValues <-
            c(paste(";", ObligatoryParameter, "is not used"),
              TokenOmegaValue)

          TokenOmegaName <- paste0(ObligatoryParameter, "_OMEGA")
          OmegaToken <- paste_Token(
            TokenName = TokenOmegaName,
            TokenAddress = TokenOmegaName,
            ListElementName = "OmegaInit",
            TokenValues = TokenOmegaValues,
            TokensListMain,
            DoNotChangeTokenListMain = FALSE
          )

          TokenThetaValues <-
            c(paste(";", ObligatoryParameter, "is not used"),
              OmegaToken)
        } else if (CurrentParamAdvanDF$OmegaPresence == 1) {
          # dive in omega into theta
          TokenThetaValues <-
            c(paste(";", ObligatoryParameter, "is not used"),
              TokenOmegaValue)
        }

        if (grepl("(^R\\d$)|(^D\\d$)", ObligatoryParameter)) {
          TokenValue <- paste_Token(
            TokenName = "RATE",
            TokenAddress = "RATE",
            ListElementName = "OmegaInit",
            TokenValues = TokenThetaValues,
            TokensListMain,
            DoNotChangeTokenListMain = FALSE
          )
        } else {
          TokenThetaName <- paste0(ObligatoryParameter, "_THETA")
          # DoNotChangeTokenListMain <-
          #   length(TokensListMain[[TokenThetaName]]) != 0

          TokenValue <- paste_Token(
            TokenName = TokenThetaName,
            TokenAddress = TokenThetaName,
            ListElementName = "OmegaInit",
            TokenValues = TokenThetaValues,
            TokensListMain,
            DoNotChangeTokenListMain = FALSE
          )
        }

        TokenValues <- c(TokenValues, TokenValue)
      }
    }

    CurrentOmegaBlock <-
      ModelDF[ModelDF$Parameter == ObligatoryParameter, "OmegaSearchBlock"][[1]][1]
    if (PreviousOmegaBlock < CurrentOmegaBlock) {
      OmegaHeadToPaste <- "$OMEGA"
      if (CurrentOmegaBlock != 1) {
        OmegaHeadToPaste <- paste(OmegaHeadToPaste, ";; search band")
      }
      ModelTemplate[paste0("OMEGAHEAD", CurrentOmegaBlock)] <-
        OmegaHeadToPaste

      PreviousOmegaBlock <- CurrentOmegaBlock
    }

    OmegaRowsCount <- OmegaRowsCount + 1
    ModelTemplate[paste0("Omega", OmegaRowsCount)] <-
      paste(
        paste_Token(
          TokenName = "ADVAN",
          TokenAddress = c("ADVAN"),
          ListElementName = "OmegaInit",
          TokenValues = TokenValues,
          TokensListMain
        ),
        # "; ETA ON", ObligatoryParameter,
        collapse = " "
      )
  }

  assign("TokensListMain", TokensListMain, envir = parent.frame())

  ModelTemplate
}
