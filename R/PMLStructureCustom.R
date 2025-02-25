CustomSpace <- function(PMLCode = character(),
                        SpaceName = character(),
                        TimeBased = logical(),
                        Responses = list(),
                        CustomCovariates = list(),
                        CustomDosepoints = list(),
                        CustomStParms = list(),
                        CustomFixefs = list(),
                        CustomRanefs = list(),
                        CFs = list(),
                        CustomDerivs = list(),
                        Transits = list(),
                        CustomUrines = list()) {
  ArgsList <- as.list(environment())
  stopifnot(is.character(PMLCode))
  stopifnot(is.logical(TimeBased))
  stopifnot(is.character(SpaceName))

  ModelHeaderPattern <- "^\\W*\\w+\\(\\)\\W*\\{\\n*"
  if (length(PMLCode) > 0 && grepl(ModelHeaderPattern, PMLCode)) {
    # need to remove the header
    PMLCode <- gsub(ModelHeaderPattern, "", PMLCode)
    PMLCode <- gsub("\\n*\\}\\W*$", "", PMLCode)
  }

  for (i in 4:length(ArgsList)) {
    if (!is.list(ArgsList[[i]])) {
      stop(names(ArgsList)[i], " should be a list.")
    }
  }

  if (!nchar(PMLCode) &&
      !nchar(SpaceName)) {
    SpaceName <-
      "EmptySpace"
  }

  structure(
    list(
      Type = "Custom",
      PMLCode = PMLCode,
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
    ),
    class = "CustomSpace"
  )
}

#' Output a Custom Space
#'
#' This function generates the PML code representation of a custom space.
#'
#' @param x A `CustomSpace` object.
#' @param ... Additional arguments (not used).
#'
#' @return A character string containing the PML code.
#'
#' @export
output.CustomSpace <- function(x, ...) {
  paste0(gsub("\\\n(?!=\\\t)", "\\\n\\\t", x$PMLCode, perl = TRUE), collapse = "\n")
}

#' @export
print.CustomSpace <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x))
}

# # @export
# create_EmptySpace <- function() {
#   EmptySpace <- CustomSpace()
#   structure(setNames(list(EmptySpace), EmptySpace$SpaceName),
#             class = "PMLModels")
# }

#' Add a Custom Space
#'
#' This function adds a custom space to a list of spaces.
#'
#' @param Spaces A list of existing spaces.
#' @param CustomCode A character string containing the custom code for the new space.
#'
#' @return A list of spaces with the new custom space added.
#'
#' @export
add_CustomSpace <- function(Spaces, CustomCode) {
  stopifnot(is.list(Spaces))
  if (length(Spaces) == 1 &&
      length(Spaces[[1]]$PMLCode) > 0 &&
      nchar(Spaces[[1]]$PMLCode) == 0) {
    # only empty is given
    Spaces <- list()
  }

  NewCustomSpace <- create_CustomSpace(CustomCode)[[1]]

  while (NewCustomSpace$SpaceName %in% names(Spaces)) {
    NewCustomSpace$SpaceName <- paste0(NewCustomSpace$SpaceName,
                                       "_")
  }

  CustomSpaces <-
    stats::setNames(c(Spaces, list(NewCustomSpace)),
                    c(names(Spaces), NewCustomSpace$SpaceName))

  structure(CustomSpaces,
            class = "PMLModels")
}
