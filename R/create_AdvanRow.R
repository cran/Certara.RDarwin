#' @importFrom magrittr %>%
#' @noRd
#' @keywords internal NONMEM
create_AdvanRow <- function(NMAdvanMatrix,
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
  if (is.na(ThetaName)) {
    ThetaName <- Parameter
  }

  if (is.na(InitEst)) {
    InitEst <- 1
  }

  NMAdvanMatrixFiltered <-
    dplyr::filter(NMAdvanMatrix,
                  ADVAN == !!ADVAN & TRANS == !!TRANS)

  stopifnot(nrow(NMAdvanMatrixFiltered) <= 1)
  Row <-
    tibble::tibble(
      ADVAN,
      TRANS,
      NComp = NMAdvanMatrixFiltered$NComp,
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

  Row <- dplyr::mutate(Row,
                       dplyr::across(names(which(
                         colSums(is.na(Row)) == nrow(Row)
                       )),
                       ~ as.numeric(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("Covar"), ~ as.character(.x))) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("Code"), ~ as.character(.x)))
  Row
}
