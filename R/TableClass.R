new_Table <- function(Name,
                      SubstitutedTimesList,
                      CovrSet,
                      WhenDose,
                      WhenObs,
                      VariablesList,
                      KeepSource,
                      TimeAfterDose,
                      IRES,
                      Weight,
                      IWRES,
                      Mode = c("all", "first", "unique"),
                      ForSimulation) {
  if (!.check_0nzchar(Name)) {
    stop("Name of the table to output should be specified")
  }

  stopifnot(is.logical(KeepSource) & length(KeepSource) == 1)
  stopifnot(is.logical(TimeAfterDose) & length(TimeAfterDose) == 1)
  stopifnot(is.logical(IRES) & length(IRES) == 1)
  stopifnot(is.logical(Weight) & length(Weight) == 1)
  stopifnot(is.logical(IWRES) & length(IWRES) == 1)
  stopifnot(is.logical(ForSimulation) & length(ForSimulation) == 1)
  Mode <- match.arg(Mode)

  if (length(eval(SubstitutedTimesList)) > 0) {
    TimesList <- .check_Table(SubstitutedTimesList, KeepSource)
  } else {
    TimesList <- eval(SubstitutedTimesList)
  }

  CovrSet <- .split_String(CovrSet)
  if (is.character(WhenDose)) {
    WhenDose <- .split_String(WhenDose)
  }

  WhenObs <- .split_String(WhenObs)
  VariablesList <- .split_String(VariablesList)

  structure(
    list(
      Name = Name,
      TimesList = TimesList,
      CovrSet = CovrSet,
      WhenDose = WhenDose,
      WhenObs = WhenObs,
      VariablesList = VariablesList,
      KeepSource = KeepSource,
      TimeAfterDose = TimeAfterDose,
      IRES = IRES,
      Weight = Weight,
      IWRES = IWRES,
      Mode = Mode,
      ForSimulation = ForSimulation
    ),
    class = "Table"
  )
}

validate_Table <- function(TableInstance) {
  SimpleTablesAttributes <-
    TableInstance$IRES ||
    TableInstance$Weight ||
    TableInstance$IWRES

  if (SimpleTablesAttributes && TableInstance$ForSimulation) {
    message(
      "Arguments 'IRES', 'Weight', 'IWRES', 'Mode' ",
      "are not applicable for simulation tables and will be ignored."
    )
  }

  TableInstance
}

#' Class initializer for NLME tables
#'
#' Creates Table class object used to specify triggers and columns for tables
#' output.
#' @param  Name Character; Name of the generated table.
#' @param  TimesList  Numeric; Time values for simulation. Applicable for
#'   time-based models only. Ignored when `keepSource=TRUE`.
#' @param  CovrSet Character; Vector of covariate names. Simulation point is
#'   added when the covariate value is set.
#' @param  WhenDose Character or logical;
#'   Vector of dosing compartment names. Alternatively if `WhenDose == TRUE`,
#'   triggers are added for all dosepoints for each PMLParametersSet separately;
#'   that approach is useful when different models in the set have different
#'   dosing compartments.
#'   Simulation point is added when the dose value is set.
#' @param  WhenObs Character; String of observed variables names. Simulation
#'   point is added when the observation value is set.
#' @param  VariablesList Character; List of variables from the model for
#'   simulation.
#' @param  KeepSource Logical; Set to `TRUE` to keep the number of rows
#'   appearing in the table the same as the number of rows in the input dataset.
#' @param  TimeAfterDose Set to `TRUE` to output time after dose.
#' @param  IRES Logical; Set to `TRUE` to output individual residuals. Valid
#'   only if `WhenObs` is specified and `ForSimulation==FALSE`.
#' @param  Weight Logical; Set to `TRUE` to output the weight of current
#'   observation. Valid only if `WhenObs` is specified and
#'   `ForSimulation==FALSE`.
#' @param  IWRES Logical; Set to `TRUE` to output individual weighted residuals.
#'   Valid only if `WhenObs` is specified and `ForSimulation==FALSE`.
#' @param  Mode Character; The mode of output. Options are `all` (default),
#'   `unique`, `first`. Only applicable to non time-based models for the case
#'   where only `CovrSet` is defined or the case where only `CovrSet` and
#'   `VariablesList` are defined. Since current version supports time-based
#'   models only, this argument is not applicable and won't change the output.
#' @param ForSimulation Logical; Set to `TRUE` if the table should be generated
#'   during simulation, otherwise the table will be generated after fitting.
#'
#' @details If the table has a flag `ForSimulation==TRUE`, it will be ignored
#' and won't be generated during estimation stage. Simulation stage should be
#' added for simulation table generation. Tables with `ForSimulation==FALSE`
#' will be ignored during simulation stage.
#'
#' @return A Table class used to store custom table information.
#' @examples
#' table01 <-
#'   Table(Name = "table01.csv",
#'       TimesList = seq(1,3,1),
#'       CovrSet = "WT",
#'       WhenDose = "A1",
#'       WhenObs = "CObs",
#'       VariablesList = "C",
#'       KeepSource = FALSE,
#'       TimeAfterDose = TRUE,
#'       IRES = TRUE,
#'       Weight = TRUE,
#'       IWRES = TRUE,
#'       ForSimulation = FALSE)
#'
#' @export
Table <- function(Name = "table01.csv",
                  TimesList = numeric(0),
                  CovrSet = "",
                  WhenDose = "",
                  WhenObs = "",
                  VariablesList = "",
                  KeepSource = FALSE,
                  TimeAfterDose = FALSE,
                  IRES = FALSE,
                  Weight = FALSE,
                  IWRES = FALSE,
                  Mode = "all",
                  ForSimulation = FALSE) {
  TableInstance <- new_Table(
    Name = Name,
    SubstitutedTimesList = substitute(TimesList),
    CovrSet = CovrSet,
    WhenDose = WhenDose,
    WhenObs = WhenObs,
    VariablesList = VariablesList,
    KeepSource = KeepSource,
    TimeAfterDose = TimeAfterDose,
    IRES = IRES,
    Weight = Weight,
    IWRES = IWRES,
    Mode = Mode,
    ForSimulation = ForSimulation
  )

  validate_Table(TableInstance)
}

is.Table <- function(x) {
  inherits(x, "Table")
}

.check_Table <- function(SubstitutedTimesList, KeepSource) {
  TimesList <- eval(SubstitutedTimesList)
  if (length(TimesList) == 0) {
    return(TimesList)
  }

  if (length(TimesList) > 0 && KeepSource) {
    warning("In keep source mode the specified time points are ignored!",
            call. = FALSE)

    return(TimesList)
  }

  Comma <- "\\,"
  White <- "\\W*"
  Numeric <-
    "([-]?[0-9]+[.]?[0-9]*|[-]?[0-9]+[L]?|[-]?[0-9]+[.]?[0-9]*[eE][-]?[0-9]+){1}"
  NumericPattern <- paste0("(^", Numeric, "$)")
  SequencePattern <- paste0(
    "(^seq\\(",
    Numeric,
    Comma,
    White,
    Numeric,
    Comma,
    White,
    Numeric,
    White,
    "\\)",
    White,
    "$)"
  )

  SimpleSequencePattern <- "(^seq$)"
  Pattern <-
    paste(SequencePattern,
          SimpleSequencePattern,
          NumericPattern,
          sep = "|")

  if (is.numeric(TimesList)) {
    TimesListSplit <- as.character(SubstitutedTimesList)

    #grepl("(^seq\\(\\d+\\,\\W*\\d+\\,\\W*\\d+\\)$)|(\\d+)")
    if (all(grepl(Pattern, TimesListSplit))) {
      # trying to preserve seq() evaluation if possible
      TimesListFinal <- deparse(SubstitutedTimesList)
    } else {
      TimesListFinal <- TimesList
    }

  } else if (is.character(TimesList)) {
    TimesListFinal <- paste(TimesList, collapse = ", ")

    tryCatch(
      TimesListFinalEvaluated <-
        eval(parse(text = paste0(
          "c(", TimesListFinal, ")"
        ))),
      error = function(e) {
        stop("current time series for a table:\n",
             TimesListFinal,
             "\nis not valid",
             call. = FALSE)
      }
    )

    # trying to preserve seq() evaluation if possible
    TimesListSplit <-
      as.character(parse(text = parse(
        text = paste0("c(", TimesListFinal, ")"),
        keep.source = FALSE
      )[[1]]))

    # removing first c()
    TimesListSplit <- TimesListSplit[-c(1)]

    if (all(grepl(Pattern, TimesListSplit))) {
      # trying to preserve seq() if possible
      TimesListFinal <- TimesList
    } else {
      # if not - use evaluated
      TimesListFinal <- TimesListFinalEvaluated
    }

  }

  TimesListFinal
}

.split_String <- function(String) {
  if (any(grepl(",", String, fixed = TRUE))) {
    String <- unlist(strsplit(String, "\\W*,\\W*"))
  }

  String <- trimws(String)

  if (length(String) > 1) {
    String <- String[String != "" & !is.null(String)]
  }

  if (length(String) == 0) {
    String <- ""
  }

  String
}

.check_TablesElements <-
  function(ElementsNotPresent,
           TableName,
           ElementsName,
           BlockName) {
    if (length(ElementsNotPresent) > 0) {
      warning(
        "Current ",
        ElementsName,
        "(s) are not in the PMLParametersSet: ",
        paste(ElementsNotPresent, collapse = ", "),
        ", but present in ",
        TableName,
        " definition.",
        "\nPlease check '",
        BlockName,
        "' block.",
        call. = FALSE
      )

      FALSE
    } else {
      TRUE
    }
  }

#' @noRd
#' @keywords internal NLME
generate.Table <-
  function(x,
           PMLParametersSets = list(),
           Observations = "",
           Covariates = "",
           Dosepoints = "",
           IsTimeBasedModel = TRUE,
           ...) {
    if (x$ForSimulation) {
      TableString <- paste0("simtbl(file=\"", x$Name, "\"")
    } else {
      TableString <- paste0("table(file=\"", x$Name, "\"")
    }

    # Time
    TimeString <- x$TimesList
    if (length(TimeString) > 0 &&
        any(TimeString != "") && !x$KeepSource) {
      TimeString <- TimeString[TimeString != ""]
      TableString <- c(TableString,
                       paste0("time(", paste0(TimeString, collapse = ", "), ")"))
    }

    # Covariates
    if (any(.check_0nzchar(x$CovrSet))) {
      CovariatesNotPresent <- x$CovrSet[!x$CovrSet %in% Covariates]
      .check_TablesElements(
        ElementsNotPresent = CovariatesNotPresent,
        TableName = x$Name,
        ElementsName = "Covariate",
        BlockName = "When covariate set"
      )
    }

    CovrSet <- x$CovrSet
    if (any(CovrSet != "")) {
      CovrSet <- CovrSet[CovrSet != ""]
      TableString <- c(TableString,
                       paste0("covr(", paste0(CovrSet, collapse = ", "), ")"))
    }

    # Observations
    if (any(.check_0nzchar(x$WhenObs))) {
      ObservationsNotPresent <- x$WhenObs[!x$WhenObs %in% Observations]
      .check_TablesElements(
        ElementsNotPresent = ObservationsNotPresent,
        TableName = x$Name,
        ElementsName = "Observation",
        BlockName = "When observe"
      )
    }

    WhenObs <- x$WhenObs
    if (any(WhenObs != "")) {
      WhenObs <- WhenObs[WhenObs != ""]
      TableString <- c(TableString,
                       paste0("obs(", paste0(WhenObs, collapse = ", "), ")"))
    }

    # Doses
    WhenDose <- ""
    if (is.character(x$WhenDose) &&
        any(.check_0nzchar(x$WhenDose))) {
      DosepointsNotPresent <- x$WhenDose[!x$WhenDose %in% Dosepoints]
      .check_TablesElements(
        ElementsNotPresent = DosepointsNotPresent,
        TableName = x$Name,
        ElementsName = "Dosepoint",
        BlockName = "When Dose set"
      )

      WhenDose <- x$WhenDose
    } else if (is.logical(x$WhenDose) && x$WhenDose) {
      # need to find doses and create tokens
      PMLDoseSet <- c()
      for (PMLParametersSet in PMLParametersSets) {
        DosepointNamesCurrentSet <-
          unique(.gather_ClassProperties(PMLParametersSet, "Dosepoint", "DosepointName", c()))
        PMLDoseSet <-
          c(PMLDoseSet,
            paste(DosepointNamesCurrentSet, collapse = ", "))
      }

      if (length(unique(PMLDoseSet)) == 1) {
        WhenDose <- PMLDoseSet[1]
      } else {
        WhenDose <-
          add_TokensNLME(
            TokenName = "PML",
            ListElementName = "Doses",
            TokenValues = PMLDoseSet,
            DoNotChangeTokenListMain = FALSE
          )
      }
    }


    if (any(WhenDose != "")) {
      WhenDose <- WhenDose[WhenDose != ""]
      TableString <- c(TableString,
                       paste0("dose(", paste0(WhenDose, collapse = ", "), ")"))
    }


    VariablesList <- x$VariablesList
    if (any(VariablesList != "")) {
      VariablesList <- VariablesList[VariablesList != ""]
      TableString <- c(TableString,
                       paste0(VariablesList, collapse = ", "))
    }

    if (x$ForSimulation && x$TimeAfterDose) {
      TableString <- c(TableString,
                       "specvar(TAD)")
    } else if (!x$ForSimulation) {
      SpecVars <- c("TAD", "IRES", "Weight", "IWRES")
      SpecVarsPresent <-
        c(x$TimeAfterDose, x$IRES, x$Weight, x$IWRES)
      if (any(SpecVarsPresent)) {
        TableString <- c(TableString,
                         paste0("specvar(", paste0(SpecVars[SpecVarsPresent], collapse = ", "), ")"))
      }
    }

    if (x$KeepSource) {
      TableString <- c(TableString,
                       "mode = keep")
    } else if (!x$ForSimulation) {
      if (IsTimeBasedModel &&
          x$Mode != "all") {
        # time based model should not have mode specified
        message(
          "Mode argument is ignored for the table ",
          x$Name,
          " since the model is time-based.",
          call. = FALSE
        )
      } else if (any(CovrSet != "") &&
                 any(WhenObs != "") &&
                 x$Mode != "all") {
        # In the case where 'WhenObs' is also specified along with 'CovrSet',
        # neither Mode = 'unique' nor mode = 'first' work
        warning(
          "Mode argument is ignored for the table ",
          x$Name,
          " since 'WhenObs' is also specified along with 'CovrSet'.",
          call. = FALSE
        )
      } else if (any(CovrSet == "") &&
                 x$Mode != "all") {
        warning(
          "Mode argument is ignored for the table ",
          x$Name,
          " since 'CovrSet' is not specified.",
          call. = FALSE
        )
      } else if (!IsTimeBasedModel &&
                 x$Mode != "all") {
        # mode is applicable for simple non-timebased tables only
        TableString <- c(TableString,
                         paste0("mode = ", x$Mode))
      }
    }

    paste0(paste0(TableString, collapse = ", "), ")")
  }
