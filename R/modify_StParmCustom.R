#' Modify custom structural parameter in PML spaces
#'
#' @inheritParams write_ModelTemplateTokens
#' @inheritParams add_StParm
#' @return An updated list of PML models (`PMLModels` class instance) matching
#'   the specified options.
#'
#' @details
#' This function can  be applied to the custom models.
#' It allows modification of custom structural parameters defined in the PML code
#' of these spaces.
#'
#' When modifying a custom structural parameter, the corresponding `Stparm`
#' statement is removed from the PML code, and the updated parameter is added
#' back as a `StParm` class using the provided arguments. Similarly, associated
#' `fixef` and `ranef` statements related to the custom structural parameter are
#' removed.
#'
#' Please note that this function is specifically designed for modifying custom
#' structural parameters. For non-custom parameters, use `modify_StParm()`.
#'
#' @examples
#'
#' # Modify the custom structural parameter 'Cl':
#' OneCpt_CustomCode <-
#'   paste0(
#'     "\nderiv(A1 = - Cl * C)",
#'     "\ndosepoint(A1)",
#'     "\ndosepoint2(A1, tlag = 12)",
#'     "\nC = A1 / V",
#'     "\nerror(CEps = 0.01)",
#'     "\nobserve(CObs = C + CEps * sqrt(1 + C^2 * (CMultStdev/sigma())^2), bql = 0.01)",
#'     "\nstparm(V = tvV * exp(nV))",
#'     "\nstparm(Cl = tvCl * exp(nCl))",
#'     "\nstparm(CMultStdev = tvCMultStdev)",
#'     "\nfixef(tvV = c(, 5, ))",
#'     "\nfixef(tvCl = c(, 1, ))",
#'     "\nfixef(tvCMultStdev = c(, 0.1, ))",
#'     "\nranef(diag(nV) = c(1))",
#'     "\nranef(diag(nCl) = c(1))\n"
#'   )
#'
#' OneCpt_CustomCode <-
#'  modify_StParmCustom(
#'    create_CustomSpace(OneCpt_CustomCode),
#'                       StParmName = "Cl",
#'                       Type = "Normal")
#'
#' @family StParms
#' @seealso [Dosepoint()] [list_StParms()] [modify_StParm()]
#' @export
modify_StParmCustom <- function(PMLParametersSets,
                                StParmName,
                                Type,
                                State,
                                ThetaStParm,
                                OmegaStParm,
                                Covariates,
                                PMLStructures = NULL) {
  stopifnot(inherits(PMLParametersSets, "PMLModels"))
  stopifnot(is.character(StParmName) && length(StParmName) == 1)
  if (all(sapply(PMLParametersSets, "[[", "Type") != "Custom")) {
    warning(
      "There are no custom spaces in the current set. ",
      "`add_StParm()`/`modify_StParm()` is recommended instead of custom functions."
    )
  }

  # prepare PMLStructures
  PMLStructures <-
    .check_PMLStructures(PMLParametersSets, PMLStructures)

  StParmsAvailableDefault <- c()
  StParmsAvailableCustom <- c()
  for (PMLStructure in PMLStructures) {
    PMLParametersSet <- PMLParametersSets[[PMLStructure]]
    StParmsAvailableDefault <-
      c(StParmsAvailableDefault,
        list_StParms(PMLParametersSet,
                     IncludeAll = TRUE,
                     IncludeCustom = FALSE)
      )

    if (PMLParametersSet$Type == "Custom") {
      StParmsAvailableCustom <-
        c(StParmsAvailableCustom,
          names(PMLParametersSet$CustomStParms))
    }
  }

  StParmsAvailableDefault <-
    unique(StParmsAvailableDefault)
  StParmsAvailableCustom <-
    unique(StParmsAvailableCustom)
  StParmsAvailable <-
    unique(c(StParmsAvailableDefault, StParmsAvailableCustom))

  if (!StParmName %in% StParmsAvailable) {
    warning(
      StParmName,
      " was not found in the list of Structural Parameters within given PMLStructures.\n",
      "Structural parameters available: ",
      paste(StParmsAvailable, collapse = ", "),
      call. = FALSE
    )
    return(PMLParametersSets)
  } else if (StParmName %in% StParmsAvailableDefault &&
             !StParmName %in% StParmsAvailableCustom) {
    warning(
      StParmName,
      " was not found in the list of custom structural parameters",
      "It should be modified with modify_StParm()."
    )
  }

  for (PMLStructure in PMLStructures) {
    PMLParametersSet <-
      PMLParametersSets[[PMLStructure]]
    StParmsCurrentPML <-
      list_StParms(PMLParametersSet,
                   IncludeAll = TRUE,
                   IncludeCustom = TRUE)

    if (!StParmName %in% StParmsCurrentPML)
      next

    StParmIsCustom <-
      length(PMLParametersSets[[PMLStructure]]$CustomStParms[[StParmName]]) > 0

    if (!StParmIsCustom) {
      message(
        "StParm ",
        StParmName,
        " in the space ",
        PMLStructure,
        " is not custom and won't be modified."
      )
      next
    }

    StParmToModify <-
      StParm(StParmName = StParmName,
             PMLStructure = PMLStructure)

    StParmToCut <-
      .get_ClassInstance(
        ParmList = PMLParametersSet,
        Name = StParmName,
        InstanceNameElement = "StParmName"
      )

    if (!missing(Type)) {
      StParmToModify$Type <- Type
    }

    if (!missing(State)) {
      StParmToModify$State <- State
    }

    if (!missing(ThetaStParm)) {
      StParmToModify$ThetaStParm <- ThetaStParm
    }

    if (!missing(OmegaStParm)) {
      StParmToModify$OmegaStParm <- OmegaStParm
    }

    if (!missing(Covariates)) {
      StParmToModify$Covariates <- Covariates
    }


    StParmCustomStatement <-
      gsub("(^\\()|(\\)$)", "", StParmToCut$Statement)

    NewPMLCode <-
      gsub(StParmCustomStatement,
           "",
           PMLParametersSet$PMLCode,
           fixed = TRUE)

    # remove empty stparm
    UpdatedPMLCode <-
      gsub("stparm\\s*\\(\\s*\\)", "", NewPMLCode)

    if (NewPMLCode == UpdatedPMLCode) {
      message("Cannot remove ",
              StParmName,
              " stparm statement from the space ",
              PMLStructure)
    }

    # reinitialize
    StParmToPaste <-
      StParm(
        StParmName = StParmToModify$StParmName,
        Type = StParmToModify$Type,
        State = StParmToModify$State,
        ThetaStParm = StParmToModify$ThetaStParm,
        OmegaStParm = StParmToModify$OmegaStParm,
        Covariates = StParmToModify$Covariates,
        PMLStructure = PMLStructure
      )

    PMLParametersSets[[PMLStructure]]$StParms[[StParmName]] <-
      StParmToPaste
    PMLParametersSets[[PMLStructure]]$CustomStParms[[StParmName]] <-
      NULL

    # trying to extract fixefs ranefs
    CustomStParmTerms <-
      strsplit(StParmCustomStatement, "[^a-zA-Z0-9_\\-\\.]", perl = TRUE)[[1]]
    CustomStParmTerms <-
      unique(CustomStParmTerms[nchar(CustomStParmTerms) > 0])
    # removing special words
    CustomStParmTerms <-
      CustomStParmTerms[!CustomStParmTerms %in% .get_SpecialFuncNames()]
    # removing numbers
    CustomStParmTerms <-
      CustomStParmTerms[!grepl(.get_NumericPattern(), CustomStParmTerms)]

    CustomFixefNames <-
      names(PMLParametersSet$CustomFixefs)
    CustomRanefNames <-
      unlist(strsplit(names(PMLParametersSet$CustomRanefs), "__"))

    # note that we cannot change ranefs in general case,
    for (CustomTerm in CustomStParmTerms) {
      if (CustomTerm %in% CustomFixefNames) {
        FixefStatementToCut <-
          PMLParametersSet$CustomFixefs[[CustomTerm]]["Statement"]
        FixefStatementToCut <-
          gsub("(^\\()|(\\)$)", "", FixefStatementToCut)

        NewPMLCode <-
          gsub(FixefStatementToCut, "", UpdatedPMLCode, fixed = TRUE)
        if (NewPMLCode == UpdatedPMLCode) {
          message("Cannot remove ",
                  CustomTerm,
                  " fixef statement from the space ",
                  PMLStructure)
          next
        }

        # remove empty fixef
        UpdatedPMLCode <-
          gsub("fixef\\s*\\(\\s*\\)", "", NewPMLCode)

        PMLParametersSets[[PMLStructure]]$CustomFixefs[[CustomTerm]] <-
          NULL

      } else if (CustomTerm %in% CustomRanefNames) {
        # only single ranefs are supported
        RanefStatementToCut <-
          PMLParametersSet$CustomRanefs[[CustomTerm]]["Statement"]

        if (length(RanefStatementToCut) > 0) {
          for (SymbolToSpace in c("(", ")", "=", ",")) {
            RanefStatementToCut <-
              gsub(
                SymbolToSpace,
                paste0("\\s*\\", SymbolToSpace, "\\s*"),
                RanefStatementToCut,
                fixed = TRUE
              )

          }

          NewPMLCode <-
            gsub(RanefStatementToCut, "", UpdatedPMLCode)
        } else {
          # ranef name is not found since it is somewhere in block
          NewPMLCode <- UpdatedPMLCode
        }

        if (NewPMLCode == UpdatedPMLCode) {
          message(
            "Cannot remove ",
            CustomTerm,
            " ranef statement from ",
            PMLStructure,
            " model since it is presented not in isolated form."
          )
          next
        }

        # remove empty ranef
        UpdatedPMLCode <-
          gsub("ranef\\s*\\(\\s*\\)", "", NewPMLCode)

        PMLParametersSets[[PMLStructure]]$CustomFixefs[[CustomTerm]] <-
          NULL
      }
    }

    PMLParametersSets[[PMLStructure]]$PMLCode <-
      UpdatedPMLCode

  }

  PMLParametersSets
}
