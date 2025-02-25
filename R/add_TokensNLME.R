TokensEnv <- new.env()

add_TokensNLME <- function(TokenName,
                           ListElementName,
                           TokenValues,
                           DoNotChangeTokenListMain = FALSE) {
  if (!exists("TokensList", envir = TokensEnv)) {
    TokensList <- list()
    assign("TokensList", TokensList, envir = TokensEnv)
    reg.finalizer(e = TokensEnv, f = clean_TokensEnv, onexit = TRUE)
  } else {
    TokensList <- get("TokensList", envir = TokensEnv)
  }

  TokenAddressNonEval <-
    paste("TokensList", paste(paste0("[['", TokenName, "']]"), collapse = ""))
  TokenInList <- eval(parse(text = TokenAddressNonEval))
  Index <- length(TokenInList)

  if (DoNotChangeTokenListMain) {
    IndexInSubList <- length(Index[[1]])

    return(paste0("{", TokenName, "[", IndexInSubList, "]}"))
  }

  if (Index == 0) {
    NextIndex <- 1
    TokenInList <-
      lapply(TokenValues,
             function(x, ListElementName) {
               names(x) <- ListElementName
               x
             },
             ListElementName)

  } else {
    if (length(TokenValues) != length(TokenInList)) {
      stop("Token values given: ", paste(TokenValues, collapse = ", "), "but current Token ", TokenName, " has ", length(TokenInList), " entities.")
    }

    # check if current TokenValues are presented in TokenInList
    MatchedValuesFound <- FALSE
    for (TokenInListIndex in seq_along(TokenInList)) {
      MatchedValues <- match(TokenValues, sapply(TokenInList, function(x) x[TokenInListIndex]))
      if (all(!(is.na(MatchedValues)))) {
        MatchedValuesFound <- TRUE
        break
      }
    }

    if (!MatchedValuesFound) {
      NextIndex <- length(TokenInList[[1]]) + 1
      for (ListEntityIndex in seq_along(TokenValues)) {
        TokenValue <- TokenValues[ListEntityIndex]
        names(TokenValue) <- ListElementName
        TokenInList[[ListEntityIndex]] <-
          c(TokenInList[[ListEntityIndex]], TokenValue)
      }
    } else {
      # no need to add a token if there's exactly the same already added
      return(paste0("{", TokenName, "[", TokenInListIndex, "]}"))
    }

  }

  eval(parse(text = paste(TokenAddressNonEval, "= TokenInList")))

  assign("TokensList", TokensList, envir = TokensEnv)

  paste0("{", TokenName, "[", NextIndex, "]}")
}

#' Clean Tokens Environment
#'
#' This function removes all objects in a specified environment.
#'
#' @param e The environment from which to remove objects. Default is TokensEnv.
#'
#' @examples
#' \dontrun{
#' # Clean objects in TokensEnv
#' clean_TokensEnv()
#'
#' # Clean objects in a different environment
#' my_env <- new.env()
#' clean_TokensEnv(my_env)
#' }
#'
#' @noRd
#' @keywords internal NLME
clean_TokensEnv <- function(e = TokensEnv) {
  rm(list = ls(envir = e), envir = e)
}
