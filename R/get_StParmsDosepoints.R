.is_numeric <- function(Value) {
  grepl(
    "^([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][-]?[0-9]+)$",
    Value
  )
}

#' @noRd
#' @keywords internal NLME
get_StParmsDosepoints <- function(PMLStructure) {
  RawStructure <- gsub(" ", "", PMLStructure)
  Statements <-
    unlist(strsplit(RawStructure, split = "(\\r\\n)|(\\n)"))

  ParametersCurrentStructure <- c()
  for (Statement in Statements) {
    # find out the statement we are working
    if (grepl("cfMicro\\(.*\\)", Statement)) {
      InBracketsPart <- gsub("^.*?\\((.*)\\).*$", "\\1", Statement)
      Parameters <- strsplit(InBracketsPart, "\\W+")[[1]]
      # avoid special words
      Parameters <- Parameters[Parameters != 'first']
    } else if (grepl("delayInfCpt\\(.*\\)", Statement)) {
      InBracketsPart <- gsub("^.*?\\((.*)\\).*$", "\\1", Statement)
      Parameters <- strsplit(InBracketsPart, "\\W+")[[1]]
      # avoid special words
      Parameters <-
        Parameters[!Parameters %in% c('in',
                                      'out',
                                      'dist',
                                      'Gamma',
                                      'InverseGaussian',
                                      'Weibull')]
    } else if (grepl("deriv\\(.*\\)", Statement)) {
      InBracketsPart <- gsub("^.*?\\((.*)\\).*$", "\\1", Statement)
      Parameters <- strsplit(InBracketsPart, "\\W+")[[1]]
    } else if (grepl("urinecpt\\(.*\\)", Statement)) {
      InBracketsPart <- gsub("^.*?\\((.*)\\).*$", "\\1", Statement)
      Parameters <- strsplit(InBracketsPart, "\\W+")[[1]]
      # avoid special words
      Parameters <- Parameters[!Parameters %in% c('fe')]
    } else {
      Parameters <- strsplit(Statement, "\\W+")[[1]]
    }

    ParametersCurrentStructure <-
      c(ParametersCurrentStructure, Parameters)
  }

  ParametersCurrentStructure <- unique(ParametersCurrentStructure)
  # these are numbers, structural parameters and dosepoints
  # filter numbers
  ParametersCurrentStructure <-
    ParametersCurrentStructure[!.is_numeric(ParametersCurrentStructure)]

  # main input names for observations
  InputNames <-
    ParametersCurrentStructure[grepl("(^C$)|(^E$)|(^A0$)", ParametersCurrentStructure)]

  # filter empty and input names
  ParametersCurrentStructure <-
    ParametersCurrentStructure[!grepl("(^$)|(^C\\d?$)|(^A0$)|(^E$)", ParametersCurrentStructure)]

  # dosepoint is A+a or A+number
  Dosepoints <-
    ParametersCurrentStructure[grepl("^A(a|[1-9])$", ParametersCurrentStructure)]
  if (any(grepl("^Aa$", Dosepoints))) {
    MainDosepoint <- "Aa"
  } else if (any(grepl("^A1$", Dosepoints))) {
    MainDosepoint <- "A1"
  } else {
    MainDosepoint <- c()
  }

  SecondaryDosepoints <- Dosepoints[Dosepoints != MainDosepoint]

  StParms <-
    ParametersCurrentStructure[!ParametersCurrentStructure %in% Dosepoints]

  list(
    MainDosepoint = MainDosepoint,
    SecondaryDosepoints = SecondaryDosepoints,
    StParms = StParms,
    InputNames = InputNames
  )
}
