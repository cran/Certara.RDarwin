#' Model Term = Column Term
#' DataMapping <-
#'   c(ID = "ID", TIME = "TIME", AMT = "AMT", DV = "DV", RATE = "RATE")
#' @noRd
#' @keywords internal NONMEM
gen_InputRow <- function(DataMapping) {
  InputRow <- "$INPUT"
  stopifnot(all(!is.null(DataMapping)))
  stopifnot(all(!is.na(DataMapping)))
  stopifnot(all(nchar(DataMapping) > 0))
  for (DataTermIndex in seq_along(DataMapping)) {
    DataTerm <- DataMapping[DataTermIndex]
    if (is.null(names(DataTerm)) ||
        is.na(names(DataTerm))) {
      names(DataTerm) <- DataTerm
    }

    if (names(DataTerm) == "RATE") {
      InputRow <- paste0(InputRow, " {", names(DataTerm), "}")
    } else {
      InputRow <- paste(InputRow, names(DataTerm))
    }

  }

  InputRow
}
