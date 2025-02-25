omit_OrphanTokens <- function(TokensListMain, ModelTemplate) {
  OneOptionOnly <- sapply(TokensListMain, length) == 1
  #if (all(!OneOptionOnly)) {
  return(list(TokensListMain = TokensListMain,
              ModelTemplate = ModelTemplate))
  #}

  TokensOneOption <- TokensListMain[OneOptionOnly]
  TokensListMain <- TokensListMain[!OneOptionOnly]
  # for each main token name with just one element
  for (TokenIndex in seq_along(TokensOneOption)) {
    Token <- TokensOneOption[TokenIndex]
    TokenName <- names(Token)
    TokensLength <- length(unlist(Token))
    # go through all elements
    for (PastedTokenIndex in 1:TokensLength) {
      TokenKeyToSearch <-
        paste0("{", TokenName, "[", PastedTokenIndex, "]}")
      # find out if there are any greps
      for (ModelTemplateIndex in seq_along(ModelTemplate)) {
        CurrentTemplateRow <- ModelTemplate[[ModelTemplateIndex]]

        if (grepl(TokenKeyToSearch, CurrentTemplateRow, fixed = TRUE)) {
          ModelTemplate[[ModelTemplateIndex]] <- gsub(
            pattern = TokenKeyToSearch,
            replacement = unlist(Token)[PastedTokenIndex],
            x = CurrentTemplateRow,
            fixed = TRUE
          )
          break()
        }
      }
    }
  }

  list(TokensListMain = TokensListMain,
       ModelTemplate = ModelTemplate)
}

paste_Token <- function(TokenName,
                        TokenAddress,
                        ListElementName,
                        TokenValues,
                        TokensList,
                        DoNotChangeTokenListMain = FALSE) {
  TokenAddressNonEval <-
    paste("TokensList", paste(paste0("[['", TokenAddress, "']]"), collapse = ""))
  TokenInList <- eval(parse(text = TokenAddressNonEval))
  Index <- length(TokenInList)

  if (DoNotChangeTokenListMain) {
    IndexInSubList <- length(Index[[1]])

    return(paste0("{", TokenName, "[", IndexInSubList, "]}"))
  }

  if (!Index) {
    NextIndex <- Index + 1
    TokenInList <-
      lapply(TokenValues,
             function(x, ListElementName) {
               names(x) <- ListElementName
               x
             },
             ListElementName)

  } else {
    NextIndex <- length(TokenInList[[1]]) + 1
    for (ListEntityIndex in seq_along(TokenValues)) {
      TokenValue <- TokenValues[ListEntityIndex]
      names(TokenValue) <- ListElementName
      TokenInList[[ListEntityIndex]] <-
        c(TokenInList[[ListEntityIndex]], TokenValue)
    }
  }

  eval(parse(text = paste(TokenAddressNonEval, "= TokenInList")))

  assign("TokensListMain", TokensList, envir = parent.frame())
  paste0("{", TokenName, "[", NextIndex, "]}")
}

#' NONMEM Template and tokens lists generation
#'
#' @inheritParams print_TemplateTokens
#'
#' @return a list with 2 sublists: TokensListMain and ModelTemplate.
#' TokenListMain includes all information used for tokens file generation.
#' ModelTemplate includes the rows for NONMEM template file generation.
#'
#' @noRd
#' @keywords internal NONMEM
gen_TemplateTokens <- function(ModelDF,
                               SizesText,
                               ProblemName,
                               DataMapping,
                               DataFilePath,
                               SigmasChosen = list(
                                 Additive = 0,
                                 Proportional = 0.1,
                                 Combined = c(PropPart = 0.2, AddPart = 1)
                               ),
                               EstimationRow = "METHOD = COND INTER NOABORT MAX = 999999",
                               CovarianceRow = "UNCOND PRINT=E",
                               AppendixRows = "",
                               OmegaSearch = list()) {
  ModelTemplate <- list()
  TokensListMain <- list()

  if (length(SizesText) > 0) {
    ModelTemplate["SIZES"] <- paste("$SIZES", SizesText)
  }


  ModelTemplate["PROBLEM"] <- paste("$PROBLEM", ProblemName)

  ModelTemplate["INPUT"] <- gen_InputRow(DataMapping)

  ModelTemplate["DATA"] <- paste("$DATA", DataFilePath, "IGNORE=@")

  UniqueAdvans <- unique(paste(ModelDF$ADVAN, ModelDF$TRANS))
  # need to specify TOL for ADVAN10
  UniqueAdvansForSubroutine <-
    ifelse(UniqueAdvans == "ADVAN10 TRANS1",
           paste(UniqueAdvans, "TOL=6"),
           UniqueAdvans)
  ModelTemplate["SUBROUTINE"] <-
    paste(
      "$SUBROUTINE",
      paste_Token(
        TokenName = "ADVAN",
        TokenAddress = c("ADVAN"),
        ListElementName = "AdvanName",
        TokenValues = UniqueAdvansForSubroutine,
        TokensListMain
      )
    )

  if (length(OmegaSearch) > 0) {
    ModelDFRestructured <- data.frame()
    for (BlockNumber in seq_along(OmegaSearch)) {
      ModelDFRestructuredBlock <- data.frame()
      if (length(OmegaSearch[[BlockNumber]]) != 0) {
        ModelDFRestructuredBlock <-
          rbind.data.frame(ModelDFRestructuredBlock,
                           ModelDF[ModelDF$Parameter %in% OmegaSearch[[BlockNumber]],])
      }

      if (BlockNumber == 1) {
        # need to collect not only first block parameters
        # but also parameters not mentioned in the search
        ModelDFRestructuredBlock <-
          rbind.data.frame(ModelDFRestructuredBlock,
                           ModelDF[!ModelDF$Parameter %in% unlist(OmegaSearch),])
      }

      ModelDFRestructuredBlock$OmegaSearchBlock <- BlockNumber
      ModelDFRestructured <- rbind.data.frame(ModelDFRestructured,
                                              ModelDFRestructuredBlock)
    }
  } else {
    ModelDFRestructured <- ModelDF
    ModelDFRestructured$OmegaSearchBlock <- 1
  }

  ModelDF <- ModelDFRestructured

  ObligatoryParametersConditions <-
    ModelDF$Presence >= 1 &
    ModelDF$Parameter == ModelDF$ThetaName &
    (is.na(ModelDF$Covariate) | ModelDF$Covariate == "")
  UniqueObligatoryParameters <-
    unique(ModelDF[ObligatoryParametersConditions, ]$ThetaName)

  if (!"RATE" %in% UniqueObligatoryParameters &&
      any(grepl("(^R\\d$)|(^D\\d$)", UniqueObligatoryParameters))) {
    stop("D and R parameters could be used with RATE only")
  }

  ModelTemplate <-
    add_PKPart(ModelTemplate,
               TokensListMain,
               ModelDF,
               UniqueAdvans,
               UniqueObligatoryParameters)

  ModelTemplate <-
    add_ErrorPart(ModelTemplate, TokensListMain, SigmasChosen)

  ModelTemplate <- add_ThetaPart(ModelTemplate,
                                 TokensListMain,
                                 ModelDF,
                                 UniqueAdvans,
                                 UniqueObligatoryParameters)

  ModelTemplate <- add_OmegaPart(ModelTemplate,
                                 TokensListMain,
                                 ModelDF,
                                 UniqueAdvans,
                                 UniqueObligatoryParameters)

  ModelTemplate["EST"] <- paste("$EST", EstimationRow)

  ModelTemplate["COV"] <- paste("$COV", CovarianceRow)

  ModelTemplate["APPENDIX"] <- AppendixRows

  results <- omit_OrphanTokens(TokensListMain, ModelTemplate)
  results
}
