add_ErrorPart <-
  function(ModelTemplate,
           TokensListMain,
           SigmasChosen) {
    ModelTemplate["ERRORHEAD"] <- "$ERROR"

    ModelTemplate["ERROR1"] <- "IPRED = F"
    ResErrors <- c()
    SigmaValues <- c()
    for (SigmaIndex in seq_along(SigmasChosen)) {
      Sigma <- SigmasChosen[SigmaIndex]
      stopifnot(
        length(unlist(Sigma)) == 1 &&
          names(Sigma) %in% c("Additive", "Proportional") ||
          length(unlist(Sigma)) == 2 &&
          names(Sigma) %in% c("Combined")
      )
      if (names(Sigma) == "Additive") {
        if (is.na(Sigma) || unlist(Sigma) <= 0)
          next()
        ResErrors <- c(ResErrors, "+EPS(RESERRA)")
        SigmaValues <-
          c(SigmaValues,
            paste0("\t\t;additive error\n\t", unlist(Sigma), "\t; EPS(RESERRA)"))
      } else if (names(Sigma) == "Proportional") {
        if (is.na(Sigma) || unlist(Sigma) <= 0)
          next()
        ResErrors <- c(ResErrors, "*(1+EPS(RESERRA))")
        SigmaValues <-
          c(SigmaValues,
            paste0(
              "\t\t;proportional error\n\t",
              unlist(Sigma),
              "\t; EPS(RESERRA)"
            ))
      } else if (names(Sigma) == "Combined") {
        if (any(is.na(unlist(Sigma))) || any(unlist(Sigma) <= 0))
          next()
        stopifnot(all(names(SigmasChosen[[3]]) %in% c("PropPart", "AddPart")))
        ResErrors <- c(ResErrors, "*(1+EPS(RESERRA))+EPS(RESERRB)")
        SigmaValues <-
          c(
            SigmaValues,
            paste0(
              "\t\t;proportional error\n\t",
              unlist(Sigma)["Combined.PropPart"],
              "\t; EPS(RESERRA)\n",
              "\t\t;additive error\n\t",
              unlist(Sigma)["Combined.AddPart"],
              "\t; EPS(RESERRB)"
            )
          )
      }
    }

    if (length(ResErrors) == 0) {
      stop("Please check the template/tokens. No sigmas were found.")
    }

    ModelTemplate["ERROR2"] <- paste(
      "Y = F",
      paste_Token(
        TokenName = "RESERR",
        TokenAddress = "RESERR",
        ListElementName = "ERROR",
        TokenValues = ResErrors,
        TokensListMain
      )
    )

    ModelTemplate["SIGMAHEAD"] <- "$SIGMA"

    ModelTemplate["SIGMA"] <- paste_Token(
      TokenName = "RESERR",
      TokenAddress = "RESERR",
      ListElementName = "SIGMA",
      TokenValues = SigmaValues,
      TokensListMain
    )

    assign("TokensListMain", TokensListMain, envir = parent.frame())

    ModelTemplate
  }
