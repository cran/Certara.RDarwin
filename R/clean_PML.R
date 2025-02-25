# removing comments from the model statements
# we are expecting to have one line for one statement
# trying to collapse the parts in parentheses
# returning the string where the outer `()` are substituted to `[]`
# syntax errors are not checked
.clean_PML <-
  function(StatementsLines) {
    if (is.list(StatementsLines)) {
      StatementsLines <- unlist(StatementsLines)
    }

    Statements <- paste(StatementsLines, collapse = "\n")
    Statements <- gsub("\\r", "", Statements)
    OneLine2Slash <- "(?:\\/\\/(?:\\\\\\n|[^\\n])*(?=$|\\n))"
    OneLineSharp <- "(?:#(?:\\\\\\n|[^\\n])*(?=$|\\n))"
    Asterisk <- "(?:\\/\\*[\\s\\S]*?\\*\\/)"

    Pattern <- paste(OneLine2Slash,
                       OneLineSharp,
                       Asterisk,
                       sep = "|",
                       collapse = "|")

    StatementsWOComm <-
      gsub(Pattern, "\n", Statements, perl = TRUE)

    # we cannot handle multiple capturing groups at once
    Pattern <- "(\\((?:[^()]++|(?-1))*+\\))"
    #"(?=\\()(?:(?=.*?\\((?!.*?\\1)(.*\\)(?!.*\\2).*))(?=.*?\\)(?!.*?\\2)(.*)).)+?.*?(?=\\1)[^(]*(?=\\2$)"
    for (iChar in 1:nchar(StatementsWOComm)) {
      ParenthesisRegexpr <- regexpr(Pattern,
                                    StatementsWOComm,
                                    perl = TRUE)

      if (ParenthesisRegexpr[1] == -1) break
      FirstInParent <-
        unlist(regmatches(
          StatementsWOComm,
          ParenthesisRegexpr
        ))

      # the PML code does not have any `[` or `]`
      # substituting the opening `(`
      FirstInParentModified <- sub("\\(", "[", FirstInParent, perl = TRUE)
      # substituting the closing `)`
      FirstInParentModified <- sub("\\)$", "]", FirstInParentModified, perl = TRUE)
      # substituting all paragraph marks to ' '
      FirstInParentModified <- gsub("\\s+", " ", FirstInParentModified, perl = TRUE)

      regmatches(
        StatementsWOComm,
        ParenthesisRegexpr
      ) <- FirstInParentModified
    }

    StatementsWOComm <- gsub("\\[", "\\(", StatementsWOComm)
    StatementsWOComm <- gsub("\\]", "\\)", StatementsWOComm)

    for (Sign in c("+", "-", "*", "/", "(", "=")) {
      StatementsWOComm <- gsub(paste0("\\s*\\", Sign, "\\s*"), Sign, StatementsWOComm, perl = TRUE)
    }

    StatementsWOComm <- gsub("\\t", " ", StatementsWOComm)
    StatementsWOComm
  }
