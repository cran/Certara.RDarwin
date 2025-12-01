#' Create a Custom Space (PML Model) from Code
#'
#' Parses a character string containing custom PML code to create a structured
#' representation within the `PMLModels` framework. This allows integrating
#' user-defined models with the package's modification and generation tools.
#'
#' @param CustomCode A character string containing the complete custom PML code
#'   (e.g., the content of a `test(){...}` block, potentially excluding the
#'   `test(){` and closing `}` depending on usage context, although including
#'   them is safer for parsing). Multi-line strings are collapsed. Cannot be empty.
#' @param SpaceName An optional character string specifying the name for this
#'   custom model space. This name will be used as the key for this model within
#'   the returned `PMLModels` list. If omitted or an empty string (`""`), a
#'   unique identifier will be created by concatenating the letter
#'   `"l"` with the number of characters in `CustomCode`. Providing a meaningful
#'   name is recommended for clarity, especially when working with multiple custom models.
#'   Ensure provided names are unique if creating multiple custom spaces intended
#'   to coexist.
#'
#' @return A list object of class `PMLModels`. This list contains a *single*
#'   element, named according to the provided or generated `SpaceName`. The
#'   value of this element is an internal list structure (created by the internal
#'   `CustomSpace` function) holding the original code and parsed components.
#'
#' @details
#' This function acts as a parser for custom PML code. It attempts to identify
#' and extract various PML statements within the provided `CustomCode` string.
#' Recognized statements include:
#' \itemize{
#'   \item Observations/Responses: `observe()`, `error()`, `multi()`, `ordinal()`, `count()`, `event()`, `LL()`
#'   \item Structural Parameters: `stparm()`
#'   \item Covariates: `covariate()` (parsed as backward), `fcovariate()` (parsed as forward), `interpolate()`
#'   \item Dosing: `dosepoint()`, `dosepoint2()`
#'   \item Parameter Definitions: `ranef()`, `fixef()`
#'   \item Model Dynamics: `deriv()`, `cfMicro()`, `cfMacro()`, `cfMacro1()`, `transit()`, `delayInfCpt()`
#'   \item Other Compartments: `urinecpt()`
#' }
#'
#' The function cleans the code (removes comments, standardizes spacing) for
#' parsing most statements but uses the original code for `stparm` and `fixef`
#' parsing to preserve complex structures.
#'
#' The extracted components are stored as specific S3 objects (e.g.,
#' `ObservationCustom`, `StParmCustom`, `CovariateCustom`, `DosepointCustom`)
#' within the list element corresponding to the `SpaceName`. This structured
#' representation allows the custom model to be potentially inspected or manipulated
#' by other package functions, although interacting with built-in models via
#' functions like `modify_StParm` is generally more robust than modifying custom
#' code components directly.
#'
#' @family Model Creation
#'
#' @examples
#' # Example custom PML code (simplified)
#' custom_pml <- "test() {
#'   cfMicro(A1, Cl / V)
#'   dosepoint(A1)
#'   C = A1 / V
#'   error(CEps = 1)
#'   observe(CObs = C + CEps)
#'   stparm(V = tvV * exp(nV))
#'   stparm(Cl = tvCl * exp(nCl))
#'   fixef(tvV = c(, 1, ))
#'   fixef(tvCl = c(, 1, ))
#'   ranef(diag(nV, nCl) = c(1, 1))
#' }
#' "
#'
#' # Create a custom space with an explicit name
#' custom_model_set <-
#'   create_CustomSpace(CustomCode = custom_pml,
#'                       SpaceName = "My1CptOral")
#'
#' # Print the structure (will show parsed components within the list element)
#' # print(custom_model_set)
#'
#' # List the name of the created space
#' names(custom_model_set) # Output: "My1CptOral"
#'
#' # Create a custom space with an automatically generated name
#' custom_model_set_auto_name <- create_CustomSpace(CustomCode = custom_pml)
#' names(custom_model_set_auto_name)
#'
#' @export
create_CustomSpace <- function(CustomCode = character(), SpaceName = character()) {
  if (any(!is.character(CustomCode))) {
    stop("CustomCode should be a character")
  }

  if (!is.character(SpaceName)) {
    stop("SpaceName should be a character")
  }

  if (length(CustomCode) > 1) {
    CustomCode <- paste(CustomCode, collapse = " ")
  } else if (length(CustomCode) == 0) {
    stop("CustomCode couldn't be null.")
  }

  if (length(SpaceName) == 0) {
    SpaceName <- .get_CodeHash(CustomCode)
  }

  CustomCode <-
    paste0(gsub("\\\n(?!=\\\t)", "\\\n\\\t", CustomCode, perl = TRUE),
           collapse = "\n")

  OriginalCustomCode <- CustomCode

  # remove comments and clean from spaces
  CustomCode <- .clean_PML(CustomCode)

  Responses <- list()
  CustomObserves <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "observe")

  CustomObserveNames <-
    names(CustomObserves)

  CustomErrors <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "error")

  CustomErrorsNames <- names(CustomErrors)
  if (length(CustomObserveNames) > 0 &&
      length(CustomErrors) == 0) {
    warning("No 'error' statement identified, but 'observe' is given.")
  } else if (length(CustomObserveNames) == 0 &&
             length(CustomErrors) > 0) {
    warning("No 'observe' statement identified, but 'error' is given.")
  } else if (length(CustomObserveNames) != length(CustomErrors)) {
    warning("Number of 'observe' statements is not the same as number of 'error' statements.")
  }

  for (CustomObserveIndex in seq_along(CustomObserves)) {
    CustomObserve <- CustomObserves[[CustomObserveIndex]]
    Sigma <- list()
    for (CustomErrorName in CustomErrorsNames) {
      if (CustomErrorName %in% CustomObserve$StatementNames) {
        Sigma[[CustomErrorName]] <-
          CustomErrors[[CustomErrorName]]["ErrorValue"]
        break
      }
    }

    Responses <- c(Responses,
                   list(
                     ObservationCustom(
                       ObservationName = CustomObserveNames[CustomObserveIndex],
                       Type = CustomObserve$Type,
                       Statement = CustomObserve$Statement,
                       StatementNames = list(CustomObserve$StatementNames),
                       Sigma = Sigma,
                       Dobefore = CustomObserve$dobefore,
                       Doafter = CustomObserve$doafter,
                       BQL = CustomObserve$BQL,
                       BQLValue = CustomObserve$LLOQ,
                       PMLStructure = SpaceName
                     )
                   ))
  }

  # responses
  CustomMultis <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "multi")

  CustomOrdinals <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "ordinal")

  CustomLLs <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "LL")

  CustomEvents <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "event")

  CustomCounts <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "count")

  # collect responses
  CustomResponses <- c(CustomMultis,
                       CustomOrdinals,
                       CustomLLs,
                       CustomEvents,
                       CustomCounts)
  for (CustomResponseIndex in seq_along(CustomResponses)) {
    Responses <- c(Responses,
                   list(
                     ObservationCustom(
                       ObservationName = names(CustomResponses)[CustomResponseIndex],
                       Type = CustomResponses[[CustomResponseIndex]]$Type,
                       Statement = CustomResponses[[CustomResponseIndex]]$Statement,
                       StatementNames = list(CustomResponses[[CustomResponseIndex]]$StatementNames),
                       PMLStructure = SpaceName
                     )
                   ))
  }

  if (length(Responses) > 0) {
    names(Responses) <- c(CustomObserveNames, names(CustomResponses))
    if (any(duplicated(names(Responses)))) {
      warning("Possible duplicates of responses detected: ",
              paste(names(Responses), collapse = ", "))
    }
  }

  # stparm
  CustomStParmsLists <-
    .parse_CustomStatements(CustomCodeToSearch = OriginalCustomCode, Statement = "stparm")
  CustomStParms <- list()
  for (StParmIndex in seq_along(CustomStParmsLists)) {
    CustomStParms <- c(CustomStParms,
                       list(
                         StParmCustom(
                           StParmName = names(CustomStParmsLists)[StParmIndex],
                           Statement = CustomStParmsLists[[StParmIndex]]$Statement,
                           PMLStructure = SpaceName
                         )
                       ))
  }

  if (length(CustomStParms) > 0) {
    names(CustomStParms) <- names(CustomStParmsLists)
    if (any(duplicated(names(CustomStParms)))) {
      warning("Possible duplicates of StParms detected: ",
              paste(names(CustomStParms), collapse = ", "))
    }
  }

  # covariates
  CustomFCovariates <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "fcovariate")

  # fcovariates are parsed here too:
  CustomBCovariates <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "covariate")

  # removing fcovariates
  CustomBCovariates <-
    CustomBCovariates[!names(CustomBCovariates) %in% names(CustomFCovariates)]

  CustomInterpolates <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "interpolate")

  CustomCovariates <- list()
  CustomCovs <- c(CustomFCovariates,
                  CustomBCovariates,
                  CustomInterpolates)
  for (CovariateIndex in seq_along(CustomCovs)) {
    if (CustomCovs[[CovariateIndex]]["Type"] == "fcovariate") {
      Direction <- "Forward"
    } else if (CustomCovs[[CovariateIndex]]["Type"] == "covariate") {
      Direction <- "Backward"
    } else {
      Direction <- "Interpolate"
    }

    CustomCovariate <-
      CovariateCustom(
        Name = names(CustomCovs)[CovariateIndex],
        Direction = Direction,
        Type = ifelse(CustomCovs[[CovariateIndex]]["IsCategorical"],
                      "Categorical",
                      "Continuous"),
        Statement = CustomCovs[[CovariateIndex]]["Statement"],
        PMLStructure = SpaceName
      )

    CustomCovariates <- c(CustomCovariates,
                          list(CustomCovariate))
  }

  if (length(CustomCovs) > 0) {
    names(CustomCovariates) <- names(CustomCovs)
    if (any(duplicated(names(CustomCovariates)))) {
      warning("Possible duplicates of covariates detected: ",
              paste(names(CustomCovariates), collapse = ", "))
    }
  }

  DosepointLists <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "dosepoint")
  if (any(duplicated(names(DosepointLists)))) {
    stop("Duplicated dosepoint names detected: ",
         names(DosepointLists)[duplicated(names(DosepointLists))])
  }

  Dosepoint2Lists <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "dosepoint2")
  DuplicatedDosepoints <-
    duplicated(c(names(DosepointLists),
                 names(Dosepoint2Lists)))
  CustomDosepointsLists <-
    c(DosepointLists,
      Dosepoint2Lists)

  CustomDosepoints <- list()
  for (DosepointIndex in seq_along(CustomDosepointsLists)) {
    CustomDosepoints <- c(CustomDosepoints,
                          list(
                            DosepointCustom(
                              DosepointName = names(CustomDosepointsLists)[DosepointIndex],
                              DoseType = CustomDosepointsLists[[DosepointIndex]]$Type,
                              dobefore = CustomDosepointsLists[[DosepointIndex]]$Dobefore,
                              doafter = CustomDosepointsLists[[DosepointIndex]]$Doafter,
                              rate = CustomDosepointsLists[[DosepointIndex]]$Rate,
                              duration = CustomDosepointsLists[[DosepointIndex]]$Duration,
                              idosevar = CustomDosepointsLists[[DosepointIndex]]$Idosevar,
                              infdosevar = CustomDosepointsLists[[DosepointIndex]]$Infdosevar,
                              infratevar = CustomDosepointsLists[[DosepointIndex]]$Infratevar,
                              bioavail = CustomDosepointsLists[[DosepointIndex]]$Bioavail,
                              tlag = CustomDosepointsLists[[DosepointIndex]]$Tlag,
                              Statement = CustomDosepointsLists[[DosepointIndex]]$Statement,
                              PMLStructure = SpaceName
                            )
                          ))
  }

  if (length(CustomDosepoints) > 0) {
    CustomDosepointsNames <- names(CustomDosepointsLists)
    names(CustomDosepoints) <- CustomDosepointsNames
    DuplicatedNames <-
      unique(CustomDosepointsNames[duplicated(CustomDosepointsNames)])
    for (DuplicatedName in DuplicatedNames) {
      DuplicatedIndices <- which(CustomDosepointsNames == DuplicatedName)
      if (length(DuplicatedIndices) > 2) {
        warning("More than 1 dosepoint/dosepoint2 detected for ",
                DuplicatedName)
      } else if (CustomDosepoints[[DuplicatedIndices[1]]]$DoseType ==
                 CustomDosepoints[[DuplicatedIndices[2]]]$DoseType) {
        # length(DuplicatedIndices) == 2
        warning("Possible duplicates of dosepoints detected: ",
                paste(names(CustomDosepoints), collapse = ", "))
      }
    }
  }


  CustomRanefs <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode,
                            Statement = "ranef")

  CustomFixefs <-
    .parse_CustomStatements(CustomCodeToSearch = OriginalCustomCode,
                            Statement = "fixef")

  # need to figure out whether the model is time based or not
  CustomDerivs <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "deriv")

  CustomUrines <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "urinecpt")

  CustomCFMicro <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "cfMicro")

  CustomCFMacro <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "cfMacro")

  CustomCFMacro1 <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "cfMacro1")

  CFs <- c(CustomCFMicro,
           CustomCFMacro,
           CustomCFMacro1)

  CustomTransits <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "transit")

  CustomCFDelayInfCpts <-
    .parse_CustomStatements(CustomCodeToSearch = CustomCode, Statement = "delayInfCpt")

  Transits <- c(CustomTransits, CustomCFDelayInfCpts)

  TimeBased <- as.logical(sum(
    length(CustomDerivs),
    length(CustomUrines),
    length(CustomCFMicro),
    length(CustomCFMacro),
    length(CustomCFMacro1),
    length(CustomTransits),
    length(CustomCFDelayInfCpts),
    length(CustomEvents)
  ))

  SpaceList <-
    list(
      CustomSpace(
        PMLCode = OriginalCustomCode,
        TimeBased = TimeBased,
        Responses = Responses,
        CustomCovariates = CustomCovariates,
        CustomDosepoints = CustomDosepoints,
        CustomStParms = CustomStParms,
        CustomFixefs = CustomFixefs,
        CustomRanefs = CustomRanefs,
        CFs = CFs,
        CustomDerivs = CustomDerivs,
        Transits = Transits,
        CustomUrines = CustomUrines,
        SpaceName = SpaceName
      )
    )

  names(SpaceList) <- SpaceName

  structure(SpaceList,
            class = "PMLModels")
}
