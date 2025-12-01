.parse_CustomStatements <- function(CustomCodeToSearch, Statement) {
  # CustomResponsesWords <- c("multi", "LL", "event", "count", "ordinal")

  # parse custom statements
  Pattern <-
    paste0("(?<=", Statement, ")(\\((?:[^()]++|(?-1))*+\\))")
  CustomInputNames <- character()
  CustomStatements <- list()
  CustomStatementsNames <- character()


  ParenthesisRegexpr <-
    gregexpr(Pattern, CustomCodeToSearch, perl = TRUE)
  if (ParenthesisRegexpr[[1]][1] == -1)
    return(list())

  InParents <-
    unlist(regmatches(CustomCodeToSearch, ParenthesisRegexpr))
  for (InParent in InParents) {
    if (Statement == "stparm") {
      # need to find out if more than 1 stparm is presented
      # get rid of spaces around '='
      InParentModified <-
        gsub(paste0("\\s*\\=\\s*"), "=", InParent, perl = TRUE)
      # need to distinct == and =
      StParmNamesWithEq <- regmatches(InParentModified,
                                      gregexpr("\\w+\\=(?!\\=)", InParentModified, perl = TRUE))[[1]]
      StParmNames <- gsub("=", "", StParmNamesWithEq, fixed = TRUE)

      StParmPattern <- paste0("(", StParmNames, ")", collapse = "|")
      StParmPattern <-
        paste0("(?<=\\W)(", StParmPattern, ")(?=\\W)")
      StParmExpressions <-
        strsplit(InParent, StParmPattern, perl = TRUE)[[1]]
      # removing the first parenthesis (substituted to "" above)
      StParmExpressions <-
        StParmExpressions[StParmExpressions != "("]
      # removing the last parenthesis
      StParmExpressions[length(StParmExpressions)] <-
        gsub(")$", "", StParmExpressions[length(StParmExpressions)])
      Statements <- paste0("(", StParmNames, StParmExpressions, ")")

      if (length(StParmNames) != length(Statements)) {
        warning(
          "Length of the stparm names is not the same as the length of statements parsed: ",
          paste(StParmNames, collapse = ", "),
          paste(Statements, collapse = ", ")
        )
      }

      for (StParmNameIndex in seq_along(StParmNames)) {
        CustomStatements[[StParmNames[StParmNameIndex]]] <-
          list(Statement = Statements[StParmNameIndex])
      }
    } else if (Statement == "fixef") {
      InParentSplitted <-
        strsplit(InParent, "[^a-zA-Z0-9_\\-\\.]", perl = TRUE)[[1]]
      FixefNames <-
        unique(InParentSplitted[nchar(InParentSplitted) > 0])
      SpecialFuncNames <- c(.get_SpecialFuncNames(), "c")
      # removing special words
      FixefNames <- FixefNames[!FixefNames %in% SpecialFuncNames]
      # removing numbers
      FixefNames <-
        FixefNames[!grepl(.get_NumericPattern(), FixefNames)]
      if (length(FixefNames) == 1) {
        CustomStatements[[FixefNames]] <-
          c(Statement = InParent)
      } else if (length(FixefNames) > 1) {
        FixefNamesPattern <- paste0("(", FixefNames, ")", collapse = "|")
        FixefNamesPattern <-
          paste0("(?<=\\W)(", FixefNamesPattern, ")(?=\\W)")
        # since there could be enable=\\d, we cannot use stparm approach
        FixefStatements <-
          strsplit(InParent, FixefNamesPattern, perl = TRUE)[[1]]
        # the first one is a parenthesis
        FixefStatements <- FixefStatements[FixefStatements != "("]
        # removing the last parenthesis
        FixefStatements[length(FixefStatements)] <-
          gsub(")$", "", FixefStatements[length(FixefStatements)])
        Statements <- paste0("(", FixefNames, FixefStatements, ")")

        if (length(FixefNames) != length(Statements)) {
          warning(
            "Length of the Fixef names is not the same as the length of statements parsed: ",
            paste(FixefNames, collapse = ", "),
            paste(Statements, collapse = ", ")
          )
        }

        for (FixefNameIndex in seq_along(FixefNames)) {
          CustomStatements[[FixefNames[FixefNameIndex]]] <-
            c(Statement = Statements[FixefNameIndex])
        }
      }

    } else if (Statement == "ranef") {
      # removing brackets
      InParentModified <-
        gsub("(^\\()|(\\)$)|[\\s\\r\\n\\t]", "", InParent, perl = TRUE)
      RanefNamesPattern <- "[a-zA-Z0-9_\\,]+"
      RanefValuesPattern <- "[0-9\\.\\,Ee]+"
      Pattern <-
        paste0(
          "(diag|block)\\(",
          RanefNamesPattern,
          "\\)\\=(c\\()?",
          RanefValuesPattern,
          "\\)?(\\,?same\\(",
          RanefNamesPattern,
          "\\))*"
        )
      OneRanefRegexpr <-
        gregexpr(Pattern, InParentModified, perl = TRUE)
      if (OneRanefRegexpr[[1]][1] == -1)
        return(list())

      Ranefs <-
        unlist(regmatches(InParentModified, OneRanefRegexpr))

      for (Ranef in Ranefs) {
        if (grepl("block(", Ranef, fixed = TRUE)) {
          RanefType <- "block"
        } else {
          RanefType <- "diag"
        }

        # parse all names in current ranef
        OneRanefNamesRegexpr <-
          gregexpr(
            paste0(
              "((?<=",
              RanefType,
              "\\()|(?<=same\\())",
              RanefNamesPattern
            ),
            Ranef,
            perl = TRUE
          )
        if (OneRanefRegexpr[[1]][1] == -1)
          return(list())

        RanefNames <-
          strsplit(unlist(regmatches(Ranef, OneRanefNamesRegexpr)), ",")

        # parse all values in current ranef
        OneRanefValuesRegexpr <-
          gregexpr(paste0("(?<=\\W)", RanefValuesPattern, "(?=\\W+)"),
                   Ranef,
                   perl = TRUE)
        if (OneRanefValuesRegexpr[[1]][1] == -1)
          return(list())

        RanefValues <-
          strsplit(unlist(regmatches(Ranef, OneRanefValuesRegexpr)), ",")

        CustomStatements[[paste(RanefNames[[1]], collapse = "__")]] <-
          list(
            RanefType = RanefType,
            RanefNames = RanefNames,
            RanefValues = RanefValues,
            Statement = Ranef
          )
      }
    } else if (Statement == "error") {
      InParentSplitted <- strsplit(InParent, "( |\\(|\\=|\\))")[[1]]
      CustomStatementValues <-
        unique(InParentSplitted[nchar(InParentSplitted) > 0])
      # the first one is the name
      ErrorName <- CustomStatementValues[1]
      # the last one is stdev
      ErrorValue <-
        as.numeric(CustomStatementValues[length(CustomStatementValues)])

      CustomStatements[[ErrorName]] <-
        c(Statement = InParent, ErrorValue = ErrorValue)

    } else if (Statement %in% c("dosepoint", "dosepoint2")) {
      # R cmd check variables
      rate <-
        duration <-
        idosevar <- infdosevar <- infratevar <- bioavail <- tlag <- NULL
      # get rid of spaces around '='
      InParent <-
        gsub(paste0("\\s*\\=\\s*"), "=", InParent, perl = TRUE)

      # extract dobefore doafter
      InParentWODosList <- .extract_Dos(InParent)

      # now we can split it to the named arguments
      InParentWODosArgs <-
        strsplit(InParentWODosList$InParentWODos,
                 "(\\,\\s*)|(\\)$)|(\\(^)")[[1]]

      DosepointWords <-
        c("rate",
          "duration",
          "idosevar",
          "infdosevar",
          "infratevar",
          "bioavail",
          "tlag")
      for (DosepointWord in DosepointWords) {
        ArgName <- paste0(DosepointWord, "\\=")
        CurrentArg <-
          InParentWODosArgs[grepl(ArgName, InParentWODosArgs)]
        CurrentArg <- gsub(ArgName, "", CurrentArg)
        assign(DosepointWord, CurrentArg)
      }

      InParentSplitted <-
        strsplit(InParentWODosList$InParentWODos,
                 "[^a-zA-Z0-9_\\-\\.]",
                 perl = TRUE)[[1]]
      CustomStatementNames <-
        unique(InParentSplitted[nchar(InParentSplitted) > 0])
      CustomInputName <- CustomStatementNames[1]

      # removing numbers
      CustomStatementNames <-
        CustomStatementNames[!grepl(.get_NumericPattern(),
                                    CustomStatementNames)]

      CustomInputNames <- c(CustomInputNames, CustomInputName)

      CustomStatements[[CustomInputName]] <-
        c(
          Statement = InParent,
          StatementNames = list(CustomStatementNames[!CustomStatementNames %in% CustomInputName]),
          Dobefore = InParentWODosList$dobefore,
          Doafter = InParentWODosList$doafter,
          Rate = rate,
          Duration = duration,
          Idosevar = idosevar,
          Infdosevar = infdosevar,
          Infratevar = infratevar,
          Bioavail = bioavail,
          Tlag = tlag
        )
    } else if (Statement == "observe") {
      # get rid of spaces around '='
      InParent <-
        gsub(paste0("\\s*\\=\\s*"), "=", InParent, perl = TRUE)

      # extract dobefore doafter
      InParentWODosList <- .extract_Dos(InParent)
      InParentSplitted <-
        strsplit(InParentWODosList$InParentWODos,
                 "[^a-zA-Z0-9_\\-\\.]",
                 perl = TRUE)[[1]]
      CustomStatementNames <-
        unique(InParentSplitted[nchar(InParentSplitted) > 0])

      # figure out lloq
      LLOQ <- NA
      if ("bql" %in% CustomStatementNames) {
        BQL <- TRUE
        if (grepl(.get_NumericPattern(), CustomStatementNames[length(CustomStatementNames)])) {
          # the last one is LLOQ
          LLOQ <-
            as.numeric(CustomStatementNames[length(CustomStatementNames)])
        }
      } else {
        BQL <- FALSE
      }

      CustomInputName <- CustomStatementNames[1]

      # removing numbers
      CustomStatementNames <-
        CustomStatementNames[!grepl(.get_NumericPattern(),
                                    CustomStatementNames)]

      CustomInputNames <- c(CustomInputNames, CustomInputName)

      CustomStatements[[CustomInputName]] <-
        c(
          Statement = InParent,
          StatementNames = list(CustomStatementNames[!CustomStatementNames %in% CustomInputName]),
          dobefore = InParentWODosList$dobefore,
          doafter = InParentWODosList$doafter,
          BQL = BQL,
          LLOQ = LLOQ
        )
    } else {
      # covariates, responses
      InParentSplitted <-
        strsplit(InParent, "[^a-zA-Z0-9_\\-\\.]", perl = TRUE)[[1]]
      CustomStatementNames <-
        unique(InParentSplitted[nchar(InParentSplitted) > 0])
      # removing special words
      CustomStatementNames <-
        CustomStatementNames[!CustomStatementNames %in% .get_SpecialFuncNames()]

      # removing numbers
      CustomStatementNames <-
        CustomStatementNames[!grepl(.get_NumericPattern(),
                                    CustomStatementNames)]

      # all covariates should be added as names
      if (Statement %in% c("covariate", "fcovariate", "interpolate")) {
        for (CustomInputName in CustomStatementNames) {
          IsCategorical <-
            grepl(paste0(CustomInputName, "\\W*\\(\\W*\\)"),
                  InParent)
          CustomStatements[[CustomInputName]] <-
            c(
              Statement =
                paste0(
                  Statement,
                  "(",
                  CustomInputName,
                  ifelse(IsCategorical, "()", ""),
                  ")"
                ),
              IsCategorical = IsCategorical
            )

        }
      } else {
        CustomInputName <- CustomStatementNames[1]

        CustomInputNames <- c(CustomInputNames, CustomInputName)

        CustomStatements[[CustomInputName]] <-
          c(Statement = InParent,
            StatementNames = list(CustomStatementNames[!CustomStatementNames %in% CustomInputName]))
      }
    }

  }

  lapply(CustomStatements, function(x) {
    x[["Type"]] <- Statement
    x
  })
}

.extract_Dos <- function(InParent) {
  dobefore <- c()
  doafter <- c()
  InParentWODos <- InParent
  for (DoWord in c("dobefore", "doafter")) {
    DoPattern <-
      paste0("\\,\\s*", DoWord, "\\s*=(\\{(?:[^{}]++|(?-1))*+\\})")
    DoParenthesisRegexpr <-
      gregexpr(DoPattern, InParent, perl = TRUE)
    if (DoParenthesisRegexpr[[1]] != -1) {
      InDo <-
        unlist(regmatches(InParentWODos, DoParenthesisRegexpr))
      assign(DoWord, InDo)
      InParentWODos <- gsub(InDo, "", InParentWODos, fixed = TRUE)
    }
  }

  list(dobefore = dobefore,
       doafter = doafter,
       InParentWODos = InParentWODos)
}

.get_SpecialFuncNames <- function() {
  c(
    "abs",
    "block",
    "CalcTMax",
    "diag",
    "dinvgauss",
    "doafter",
    "dobefore",
    "dweibull",
    "enable",
    "erfunc",
    "factorial",
    "freeze",
    "icloglog",
    "idosevar",
    "ilogit",
    "iloglog",
    "iprobit",
    "lambertw",
    "ldinvgauss",
    "ldweibull",
    "lgamm",
    "lnegbin",
    "lnegbin_rp",
    "lnorm",
    "log",
    "log10",
    "lphi",
    "lpinvgauss",
    "lpois",
    "lpweibull",
    "max",
    "megnin_rp",
    "min",
    "phi",
    "pinvgauss",
    "pnegbin",
    "ppois",
    "probit",
    "pweibull",
    "rnegbin",
    "rpois",
    "runif",
    "unifToPoisson"
  )
}


.get_CodeHash <- function(Code) {
  paste0("l", nchar(Code))
}

.get_NumericPattern <- function() {
  "^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][-]?[0-9]+)$"
}
