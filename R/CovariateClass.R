#' Create a new Covariate object
#'
#' @inheritParams  Covariate
#'
#' @return A Covariate object
#'
#' @noRd
#' @keywords internal NLME
new_Covariate <- function(Name = character(),
                          Type = c("Continuous", "Categorical", "Occasion"),
                          StParmName = character(),
                          State = c("Present", "None", "Searched"),
                          Direction = c("Forward", "Backward", "Interpolate"),
                          Center = "None",
                          Categories = c(),
                          Thetas = c(),
                          Omegas = c(),
                          PMLStructure = character()) {
  stopifnot(
    length(Name) == 1 &
      length(Type) == 1 &
      length(StParmName) <= 1 &
      length(State) == 1 &
      length(Direction) == 1 &
      length(Center) == 1
  )

  stopifnot(is.character(Name) & .check_0nzchar(Name))
  stopifnot(is.character(PMLStructure))

  State <- match.arg(State)
  Type <-
    match.arg(Type)
  Direction <-
    match.arg(Direction)

  stopifnot(is.character(Center) | is.numeric(Center))

  if (length(Categories) > 0) {
    stopifnot(is.numeric(Categories))
    stopifnot(is.null(names(Categories)) |
                all(names(Categories) != ""))
  }

  if (inherits(Thetas, "Theta")) {
    Thetas <- list(Thetas)
  } else if (length(Thetas) > 0) {
    stopifnot(is.list(Thetas))
    stopifnot(all(sapply(Thetas, function(x)
      inherits(x, "Theta"))))
  } else {
    Thetas <- list()
  }

  if (inherits(Omegas, "Omega")) {
    Omegas <- list(Omegas)
  } else if (length(Omegas) > 0) {
    stopifnot(is.list(Omegas))
    stopifnot(all(sapply(Omegas, function(x)
      inherits(x, "Omega"))))
  } else {
    Omegas <- list()
  }

  structure(
    list(
      Name = Name,
      Type = Type,
      StParmName = StParmName,
      State = State,
      Direction = Direction,
      Center = Center,
      Categories = Categories,
      Thetas = Thetas,
      Omegas = Omegas,
      PMLStructure = PMLStructure
    ),
    class = "Covariate"
  )
}

#' Validate a Covariate object
#'
#' @param CovariateInstance A Covariate object to be validated
#'
#' @return The validated Covariate object
#'
#' @noRd
#' @keywords internal NLME
validate_Covariate <- function(CovariateInstance) {
  if (length(CovariateInstance$Thetas) > 0 &
      length(CovariateInstance$Omegas) > 0) {
    stop("Covariate cannot be bound to Thetas and Omegas in the same time.")
  }

  if (.check_0nzchar(CovariateInstance$Center) &
      CovariateInstance$Type != "Continuous" &
      CovariateInstance$Center != "None") {
    warning(
      "For covariate ",
      CovariateInstance$Name,
      " Center is specified, but not used for non-continuous covariates"
    )
  }

  if (is.character(CovariateInstance$Center)) {
    CovariateInstance$Center <-
      match.arg(CovariateInstance$Center, c("None", "Mean", "Median"))
  } else {
    stopifnot(is.numeric(CovariateInstance$Center))
  }

  if (CovariateInstance$Type == "Continuous") {
    if (length(CovariateInstance$Categories) > 0) {
      stop(
        "Categories are not applicable for continuous covariate ",
        CovariateInstance$Name,
        ": ",
        paste(CovariateInstance$Categories, collapse = " ")
      )
    }

    if (length(CovariateInstance$Thetas) > 1) {
      stop(
        "Cannot use more than 1 theta for continuous covariate",
        CovariateInstance$Name,
        " : ",
        paste(CovariateInstance$Thetas, collapse = " ")
      )
    }

    if (length(CovariateInstance$Omegas) > 0) {
      stop(
        "Cannot use Omegas for continuous covariate ",
        CovariateInstance$Name,
        ": ",
        paste(CovariateInstance$Omegas, collapse = " ")
      )
    }

  } else if (CovariateInstance$Type == "Categorical") {
    if (length(CovariateInstance$Categories) < 2) {
      stop(
        "More than 1 Category should be specified for categorical covariate ",
        paste(CovariateInstance$Name)
      )
    }

    if (length(CovariateInstance$Thetas) != length(CovariateInstance$Categories) - 1) {
      stop(
        "Number of Thetas (",
        length(CovariateInstance$Thetas),
        ") bound to categorical covariate ",
        CovariateInstance$Name,
        " should be one less than number of categories: ",
        length(CovariateInstance$Categories)
      )
    }

    if (length(CovariateInstance$Omegas) > 0) {
      stop(
        "Cannot use Omegas for categorical covariate ",
        CovariateInstance$Name,
        ": ",
        paste(CovariateInstance$Omegas, collapse = " ")
      )

    }
  } else if (CovariateInstance$Type == "Occasion") {
    if (length(CovariateInstance$Categories) < 2) {
      stop(
        "More than 1 Category should be specified for Occasion covariate ",
        paste(CovariateInstance$Name)
      )
    }

    if (length(CovariateInstance$Thetas) > 0) {
      stop(
        "Cannot use Thetas for Occasion covariate ",
        CovariateInstance$Name,
        ": ",
        paste(CovariateInstance$Omegas, collapse = " ")
      )
    }

    if (length(CovariateInstance$Omegas) != length(CovariateInstance$Categories)) {
      stop(
        "Number of Omegas (",
        length(CovariateInstance$Thetas),
        ") bound to Occasional covariate ",
        CovariateInstance$Name,
        " should be the same as the number of categories: ",
        length(CovariateInstance$Categories)
      )
    }
  }

  if (CovariateInstance$Type %in% c("Categorical", "Occasion") &
      CovariateInstance$Direction == "Interpolate") {
    stop("Cannot interpolate categorical covariate",
         CovariateInstance$Name)
  }

  CovariateInstance
}

#' Create a new Covariate object and validate it
#'
#' @param Name Character specifying the name of the covariate.
#' @param Type A character specifying the type of the covariate.
#'   Possible values are:
#' \itemize{
#'   \item `Continuous` A covariate  can take values on a continuous scale.
#'   \item `Categorical` A covariate can only take a finite number of values.
#'   \item `Occasion` The associated PK parameter may vary within an individual
#'   from one event to the next, called interoccasion variability.
#' }
#'
#' @param StParmName A character specifying the corresponding structural
#'   parameter name.
#' @param State A character string representing the presence of the covariate on the structural parameters. Possible values are:
#' \itemize{
#'   \item `None` The covariate does not have an effect on any structural parameter.
#'   \item `Present` The covariate has an effect on the structural parameters (the default).
#'   \item `Searched` The effect of the covariate on structural parameters is searched.
#' }
#' @param Direction A character string representing the direction of the
#'   Covariate. Options are `Forward`, `Backward`, `Interpolate`. Default is
#'   `Forward`. `Interpolate` is only applicable to `Type == "Continuous"`.
#' @param Center A character string (`None`, `Mean` or `Median`) or numeric
#'   value representing the center of the Covariate. Default is `None`. Valid
#'   only if `Type == "Continuous"`.
#' @param Categories A numeric vector representing the categories (at least two)
#'   of the covariate. Applicable only if `Type` is either `Occasion` or
#'   `Categorical`. The first category is set to the reference category for
#' categorical covariate.
#' @param Thetas A list of Theta objects representing  Thetas covariate effects.
#'   Only applicable if `Type` is either `Categorical` or `Continuous`.
#'   If `Type == "Continuous"`, one Theta corresponding to current Covariate
#'   should be presented. If `Type == "Categorical"`, thetas corresponding to
#'   each category (except the reference category) can be specified. If not
#'   given, theta(s) will be automatically generated with initial estimate set
#'   to 0.0.
#' @param Omegas A list of Omega objects representing the Omegas of
#'   the inter-occasion random effects. Applicable only if `Type == "Occasion"`.
#'   The number of Omegas should be equal to the number of categories provided.
#'   If not given, Omegas will be created automatically with initial estimate
#'   set to 1.0.
#' @param PMLStructure PML structure current Covariate instance belongs to.
#'
#' @return A Covariate object
#'
#' @family Covariates
#' @examples
#' WT_Covariate <-
#'   Covariate(Name = "WT",
#'             Type = "Continuous",
#'             StParmName = "V",
#'             State = "Present",
#'             Direction = "Forward",
#'             Center = 70,
#'             Thetas = Theta("dVdWT", 1))
#'
#' Race_Covariate <-
#'   Covariate(
#'     Name = "Race",
#'     Type = "Categorical",
#'     StParmName = "V2",
#'     State = "Searched",
#'     Direction = "Backward",
#'     Center = "None",
#'     Categories = c(1,2,3))
#'
#' @export
Covariate <- function(Name = character(),
                      Type = "Continuous",
                      StParmName = character(),
                      State = "Present",
                      Direction = "Forward",
                      Center = "None",
                      Categories = c(),
                      Thetas = c(),
                      Omegas = c(),
                      PMLStructure = character()) {
  # need to check it here since it is interrupted by multiple theta PMLStructure otherwise
  if (length(PMLStructure) > 1) {
    stop("More than 1 'PMLStructure' provided for Covariate ",
         Name)
  }

  CovariateInstance <- new_Covariate(
    Name = Name,
    Type = Type,
    StParmName = StParmName,
    State = State,
    Direction = Direction,
    Center = Center,
    Categories = Categories,
    Thetas = Thetas,
    Omegas = Omegas,
    PMLStructure = PMLStructure
  )

  Name <- CovariateInstance$Name
  Type <- CovariateInstance$Type
  StParmName <- CovariateInstance$StParmName
  State <- CovariateInstance$State
  Direction <- CovariateInstance$Direction
  Center <- CovariateInstance$Center
  Categories <- CovariateInstance$Categories
  Thetas <- CovariateInstance$Thetas
  Omegas <- CovariateInstance$Omegas
  PMLStructure <- CovariateInstance$PMLStructure

  if (!.check_0nzchar(StParmName)) {
    StParmName <- "__"
  }

  if (Type == "Continuous" &&
      length(Thetas) == 0) {
    # create thetas for Continuous covariate if not given
    Thetas <- list()
    Thetas[[paste0("d", StParmName, "d", Name)]] <-
      Theta(
        Name = paste0("d", StParmName, "d", Name),
        InitialEstimates = 0,
        State = "Present",
        Frozen = F,
        StParmName = StParmName,
        PMLStructure = PMLStructure
      )
  } else if (Type == "Categorical" &&
             length(Categories) > 1 &&
             length(Thetas) == 0) {
    Thetas <- list()
    for (Category in Categories) {
      if (Category == Categories[1])
        next
      Thetas[[paste0("d", StParmName, "d", Name, Category)]] <-
        Theta(
          Name = paste0("d", StParmName, "d", Name, Category),
          InitialEstimates = 0,
          State = "Present",
          Frozen = F,
          StParmName = StParmName,
          PMLStructure = PMLStructure
        )
    }
  } else if (Type == "Occasion" &&
             length(Categories) > 1 &&
             length(Omegas) == 0) {
    Omegas <- list()
    for (Category in Categories) {
      Omegas[[paste0("n", StParmName, Name, "x", Category)]] <-
        Omega(
          Name = paste0("n", StParmName, Name, "x", Category),
          InitialOmega = 1,
          State = "Present",
          Frozen = FALSE,
          StParmName = StParmName,
          PMLStructure = PMLStructure
        )
    }
  }

  CovariateInstance$Thetas <- Thetas
  CovariateInstance$Omegas <- Omegas

  validate_Covariate(CovariateInstance)

  if (Type == "Continuous" &&
      Thetas[[1]]$Name == paste0("d__d", Name)) {
    # rename theta if given without StParm name knowledge
    names(Thetas) <- paste0("d", StParmName, "d", Name)
    Thetas[[1]]$Name <- names(Thetas)
  } else if (Type == "Categorical" &&
             all(!is.na(match(
               sapply(Thetas, function(x)
                 x$Name),
               paste0("d__d", Name, Categories)
             )))) {
    # rename thetas if given without StParm name knowledge
    names(Thetas) <-
      paste0("d", StParmName, "d", Name, Categories[-c(1)])
    for (Category in Categories) {
      if (Category == Categories[1])
        next
      Thetas[[paste0("d", StParmName, "d", Name, Category)]]$Name <-
        paste0("d", StParmName, "d", Name, Category)
    }
  } else if (Type == "Occasion" &&
             all(!is.na(match(
               sapply(Omegas, function(x)
                 x$Name), paste0("n__x", Categories)
             )))) {
    # rename omegas if given without StParm name knowledge
    names(Omegas) <- paste0("n", StParmName, "x", Categories)
    for (Category in Categories) {
      Omegas[[paste0("n", StParmName, "x", Category)]]$Name <-
        paste0("n", StParmName, "x", Category)
    }
  }

  CovariateInstance$Thetas <- Thetas
  CovariateInstance$Omegas <- Omegas

  CovariateInstance
}

#' @noRd
#' @keywords internal NLME
output.Covariate <- function(x, ...) {
  if (x$Direction == "Forward") {
    CovariateOutput <- paste0("fcovariate(", x$Name)
  } else if (x$Direction == "Interpolate") {
    CovariateOutput <- paste0("interpolate(", x$Name)
  } else {
    CovariateOutput <- paste0("covariate(", x$Name)
  }

  if (x$Type != "Continuous") {
    CovariateOutput <- paste0(CovariateOutput, "()")
  }

  CovariateOutput <- paste0(CovariateOutput, ")")
  CovariateOutput
}

#' @export
print.Covariate <- function(x, ...) {
  on.exit(clean_TokensEnv(e = TokensEnv))
  cat(output(x), "\n")
}
