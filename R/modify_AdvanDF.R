#' Internal function to modify DF with THETAS
#' #' @importFrom magrittr %>%
#'
#' @noRd
#' @keywords internal NONMEM
modify_AdvanDF <- function(ADVANDF,
                           NMAdvanMatrix,
                           ADVAN,
                           TRANS,
                           Parameter,
                           Presence = 1,
                           ThetaName = NA,
                           Covariate = NA,
                           CovarRelation = NA,
                           Center = NA,
                           Category = NA,
                           LowerBound = NA,
                           InitEst = NA,
                           UpperBound = NA,
                           OmegaPresence = 1,
                           Omega = 1,
                           ThetaFrozen = 0,
                           OmegaFrozen = 0,
                           PreCode = NA,
                           PostCode = NA) {
  ADVANDF <-
    dplyr::mutate(ADVANDF,
                  dplyr::across(names(which(
                    colSums(is.na(ADVANDF)) == nrow(ADVANDF)
                  )),
                  ~ as.numeric(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Covar"), ~ as.character(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("Code"), ~ as.character(.x)))

  if (missing(ADVAN)) {
    ADVAN <- unique(ADVANDF$ADVAN)
  }

  if (missing(TRANS)) {
    TRANS <- unique(ADVANDF$TRANS)
  }

  for (currentADVAN in ADVAN) {
    TRANSesToSearch <-
      unique(ADVANDF$TRANS[ADVANDF$ADVAN == currentADVAN &
                             ADVANDF$TRANS %in% TRANS])
    for (currentTRANS in TRANSesToSearch) {
      Row <- create_AdvanRow(
        NMAdvanMatrix,
        ADVAN = currentADVAN,
        TRANS = currentTRANS,
        Parameter,
        Presence,
        ThetaName,
        Covariate,
        CovarRelation,
        Center,
        Category,
        LowerBound,
        InitEst,
        UpperBound,
        OmegaPresence,
        Omega,
        ThetaFrozen,
        OmegaFrozen,
        PreCode,
        PostCode
      )


      ADVANDF <-
        dplyr::rows_upsert(ADVANDF,
                           Row,
                           by = c("ADVAN", "TRANS", "Parameter", "ThetaName", "CovarRelation"))
    }
  }

  ADVANDF
}
