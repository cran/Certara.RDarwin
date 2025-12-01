#' Specify Engine Parameters for Model Simulation
#'
#' Use to define engine parameters for model simulation. Generates a single
#' character string containing space-separated `name=value` pairs.
#'
#' @details This function allows customization of the NLME engine settings
#' specific to simulation runs. Parameters are validated based on type and
#' range.
#'
#' The parameters `numReplicates`, `seed`, and `sort` are **always** included in
#' the output string. Other parameters (`ODE`, `rtolODE`, `atolODE`,
#' `maxStepsODE`) are included only if their specified value differs from the
#' function's default value *and* they are applicable (ODE tolerances are
#' ignored if `ODE="MatrixExponent"`).
#'
#' Values are returned as character strings. The order of parameters in the
#' output string matches the order in the function definition.
#'
#' @param numReplicates Integer; Number of replicates (simulations) to generate.
#'   Must be positive. **Always included in output.**
#' @param seed Integer; Seed for the random number generator used during
#'   simulation.
#'   **Always included in output.**
#' @param sort Logical; Specifies whether to sort the input data by subject and
#'   time before simulation. **Default: `FALSE`**. **Always included in
#'   output.**
#' @param ODE Character; Specifies the ODE solver. Options: `"MatrixExponent"`,
#'   `"DVERK"`, `"DOPRI5"`, `"AutoDetect"`, `"Stiff"`. **Default:
#'   `"MatrixExponent"`**.
#' @param rtolODE Numeric; Relative tolerance for the ODE solver.
#'   **Default: `1e-6`**. *(Not applicable if `ODE = "MatrixExponent"`)*.
#' @param atolODE Numeric; Absolute tolerance for the ODE solver.
#'   **Default: `1e-6`**. *(Not applicable if `ODE = "MatrixExponent"`)*.
#' @param maxStepsODE Integer; Maximum number of steps for the ODE solver.
#'   **Default: `50000L`**. *(Not applicable if `ODE = "MatrixExponent"`)*.
#'
#' @return A **single character string** containing space-separated `name=value`
#'   pairs, ordered according to the function signature. Includes
#'   `numReplicates`, `seed`, `sort` always, and other parameters if specified
#'   with non-default, applicable values.
#'
#' @seealso [write_ModelTemplateTokens()], [specify_EngineParams()], [Table()]
#'
#' @examples
#' # Default settings (includes numReplicates, seed, sort)
#' SimArgs1 <- specify_SimParams()
#' print(SimArgs1)
#'
#' # Custom settings
#' SimArgs2 <-
#'   specify_SimParams(
#'     numReplicates = 50,
#'     seed = 9876,
#'     sort = TRUE, # Non-default
#'     ODE = "DVERK", # Non-default
#'     rtolODE = 1e-5 # Non-default and applicable
#'   )
#' print(SimArgs2)
#'
#' # Custom settings where ODE tolerances are ignored
#' SimArgs3 <-
#'   specify_SimParams(
#'     numReplicates = 20,
#'     ODE = "MatrixExponent", # Default, but tolerances are now inapplicable
#'     rtolODE = 1e-4 # Non-default, but ignored
#'    )
#' print(SimArgs3)
#'
#' @export
specify_SimParams <-
  function(numReplicates = 100L,
           seed = 1234L,
           sort = FALSE,
           ODE = c("MatrixExponent", "DVERK", "DOPRI5", "AutoDetect", "Stiff"),
           rtolODE = 1e-6,
           atolODE = 1e-6,
           maxStepsODE = 50000L) {
    # --- Get function formals and their evaluated default values ---
    formal_args <- formals(specify_SimParams)
    # Evaluate defaults ONCE
    evaluated_defaults <-
      lapply(formal_args, eval, envir = environment(specify_SimParams))
    all_param_names <- names(formal_args) # Keep this order

    # --- Get the actual argument values used in this function call ---
    resolved_args <- mget(all_param_names, envir = environment())

    # --- Resolve Choices and Validate ALL arguments ---
    # Define choice vectors for validation/matching
    ode_choices_sim <-
      c("MatrixExponent", "DVERK", "DOPRI5", "AutoDetect", "Stiff")

    # Apply match.arg
    resolved_args$ODE <-
      tryCatch(
        match.arg(resolved_args$ODE, choices = ode_choices_sim),
        error = function(e)
          stop("Invalid 'ODE': ", conditionMessage(e), call. = FALSE)
      )

    # Validation helpers
    validate_numeric <-
      function(val,
               name,
               min_val = -Inf,
               max_val = Inf,
               integer_only = FALSE) {
        if (!is.numeric(val) ||
            length(val) != 1 ||
            is.na(val))
          stop("'", name, "' must be a single, non-NA numeric value.", call. = FALSE)
        if (val < min_val ||
            val > max_val)
          stop(
            "'",
            name,
            "' must be between ",
            min_val,
            " and ",
            max_val,
            ", received: ",
            val,
            ".",
            call. = FALSE
          )
        if (integer_only &&
            (round(val) != val))
          stop("'", name, "' must be an integer.", call. = FALSE)
      }

    validate_logical <-
      function(val, name) {
        if (!is.logical(val) ||
            length(val) != 1 ||
            is.na(val))
          stop("'", name, "' must be a single TRUE or FALSE value.", call. = FALSE)
      }

    # Apply validations
    validate_numeric(
      resolved_args$numReplicates,
      "numReplicates",
      min_val = 1,
      integer_only = TRUE
    )
    validate_numeric(resolved_args$seed, "seed", integer_only = TRUE) # Basic check for seed
    validate_logical(resolved_args$sort, "sort")
    validate_numeric(resolved_args$rtolODE, "rtolODE", min_val = .Machine$double.eps)
    validate_numeric(resolved_args$atolODE, "atolODE", min_val = .Machine$double.eps)
    validate_numeric(
      resolved_args$maxStepsODE,
      "maxStepsODE",
      min_val = 1,
      integer_only = TRUE
    )


    # --- Collect Applicable Values (Always include numReplicates, seed, sort) ---
    output_args_list <- list()

    for (p_name in all_param_names) {
      # Iterate in signature order
      current_value <- resolved_args[[p_name]]
      effective_default <- evaluated_defaults[[p_name]]

      # If default was choice vector, take first element for comparison
      if (is.character(effective_default) &&
          length(effective_default) > 1) {
        effective_default <- effective_default[1]
      }

      # Determine if different from default
      is_different <- !identical(current_value, effective_default)

      # Check applicability
      param_applicable <- TRUE
      if (resolved_args$ODE == "MatrixExponent" &&
          p_name %in% c("rtolODE", "atolODE", "maxStepsODE")) {
        param_applicable <- FALSE
      }

      # Add to list if applicable AND (is always output OR is different from default)
      if (param_applicable &&
          (p_name %in% c("numReplicates", "seed", "sort") ||
           is_different)) {
        output_args_list[[p_name]] <- current_value
      }
    }

    # --- Format Output String ---
    if (length(output_args_list) == 0) {
      # Should only happen if numRep, seed, sort were somehow excluded (logic error)
      return("")
    } else {
      # The list is already effectively ordered by the loop above
      output_strings <- mapply(function(name, value) {
        value_str <-
          ifelse(is.logical(value),
                 ifelse(value, "TRUE", "FALSE"),
                 as.character(value))
        paste0(name, "=", value_str)
      },
      names(output_args_list),
      output_args_list,
      USE.NAMES = FALSE)
      # Add leading space as per original format
      return(paste(c("", output_strings), collapse = " "))
    }
  }
