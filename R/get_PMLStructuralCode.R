get_PMLStructuralCode <- function(RequestedPMLDF, ClosedForm) {
  RowsSelected <-
    merge.data.frame(NLMEFullStructuralOptionsDosesStParms,
                     RequestedPMLDF)

  PMLStructure <- NULL
  if (any(duplicated(subset(RowsSelected, select = -c(PMLStructure, ClosedForm))))) {
    # find out duplicated ClosedForms
    Duplicates1 <- duplicated(subset(RowsSelected, select = -c(PMLStructure, ClosedForm)))
    Duplicates2 <- duplicated(subset(RowsSelected, select = -c(PMLStructure, ClosedForm)), fromLast = TRUE)

    Duplicates <- Duplicates1 | Duplicates2
    DuplicatedRows <- RowsSelected[Duplicates, ]
    NonDuplicatedRows <- RowsSelected[!Duplicates, ]

    RowsSelected <- rbind.data.frame(NonDuplicatedRows,
                                     DuplicatedRows[DuplicatedRows$ClosedForm == ClosedForm, ])

    RowsSelected <- RowsSelected[order(as.numeric(rownames(RowsSelected))), ]
  }

  PMLStructuralCodeSelected <- unique(RowsSelected$PMLStructure)
  if (length(PMLStructuralCodeSelected) == 0) {
    stop("No PML structures selected according to given criteria")
  }

  PMLStructuralCodeSelected
}
