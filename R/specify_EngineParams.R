#' Specify Engine Parameters for NLME Model Execution
#'
#' Defines optional engine parameters to control the estimation or simulation
#' process in Phoenix NLME. This function generates a **single character
#' string** containing space-separated `name=value` pairs for non-default
#' settings. Parameters are included in the output string according to the order
#' in the function signature.
#'
#' @details This function allows customization of the NLME engine settings.
#' Parameters are validated based on type, range, and applicability (detailed in
#' parameter descriptions). Only parameters explicitly set to a value different
#' from their default *for the specified context* (e.g., method-specific
#' defaults for `stdErr`) are included in the output string. Values are returned
#' as character strings.
#'
#' **Important Note on Defaults:** Uses fixed defaults as specified in the argument
#' list for comparison *unless otherwise noted* (e.g., `stdErr`). The actual
#' default applied by NLME might differ based on model context (population vs
#' individual, presence of reset info, discontinuities, BQL data).
#'
#' **Parameter Applicability & Warnings:**
#' The function checks for common cases where provided parameters might be
#' ignored by the NLME engine based on the selected `method` or other settings.
#' Warnings are issued in such cases. It assumes population context unless
#' `method="Naive-Pooled"`.
#'
#' @param sort Logical; Specifies whether to sort the input data by subject and
#'   time. **Default: `FALSE`**. (Note: NLME/RsNLME may default to `TRUE` if
#'   model has no reset info). Included in output only if set to `TRUE`.
#' @param ODE Character; Specifies the ODE solver. Options: `"MatrixExponent"`,
#'   `"DVERK"`, `"DOPRI5"`, `"AutoDetect"`, `"Stiff"`, `"LSODE"`. **Default:
#'   `"MatrixExponent"`**. `"AutoDetect"` and `"LSODE"` use LSODA. `"Stiff"` is
#'   LSODE configured for stiff systems.
#' @param rtolODE Numeric; Relative tolerance for the ODE solver.
#'   **Default: `1e-6`**. *(Not applicable if `ODE = "MatrixExponent"`)*.
#' @param atolODE Numeric; Absolute tolerance for the ODE solver.
#'   **Default: `1e-6`**. *(Not applicable if `ODE = "MatrixExponent"`)*.
#' @param maxStepsODE Integer; Maximum number of steps for the ODE solver.
#'   **Default: `50000L`**. *(Not applicable if `ODE = "MatrixExponent"`)*.
#' @param numIterations Integer; Maximum number of estimation iterations (max:
#'   10000).
#'   **Default: `1000L`**.
#' @param method Character; Estimation method. Options: `"FOCE-ELS"`, `"QRPEM"`,
#'   `"Laplacian"`, `"Naive-Pooled"`, `"FOCE-LB"`, `"IT2S-EM"`, `"FO"`.
#'   **Default: `"FOCE-ELS"`**. (Note: NLME/RsNLME default depends on model type).
#'   Only `"Naive-Pooled"` is valid for individual models.
#' @param stdErr Character; Standard error computation method. Options:
#'   `"Sandwich"`, `"Hessian"`, `"Fisher-Score"`, `"Auto-Detect"`, `"None"`.
#'   **Default:** `"Sandwich"` (for most methods), `"Fisher-Score"` (if `method="QRPEM"`),
#'   `"None"` (if `method="IT2S-EM"`). The default applied depends on the chosen
#'   `method`. See `Certara.RsNLME::engineParams` documentation for full
#'   details.
#' @param isCentralDiffStdErr Logical; Use central (`TRUE`) or forward (`FALSE`)
#'   difference for numerical standard error calculations. **Default: `TRUE`**.
#' @param stepSizeStdErr Numeric; Relative step size for numerical Hessian
#'   computation for standard errors. Must be positive. **Default: `0.01`**.
#'   (Note: NLME/RsNLME default differs for Naive-Pooled method).
#' @param logTransform Logical or NULL; Controls log-transformation behavior for
#'   models with log-additive residual error (e.g., C*exp(epsilon)).
#'   **Default: `NULL`**.
#'   If set to `TRUE` or `FALSE` (i.e., not `NULL`), the `logTransform=VALUE`
#'   pair is included in the output string. The NLME engine interprets this
#'   parameter based on the model structure.
#'   \itemize{
#'     \item For models with a **single log-additive residual error**:
#'       \itemize{
#'         \item `NULL` or `TRUE`: Enables fitting in the log-domain (LTBS approach).
#'         \item `FALSE`: The log-additive error is treated as proportional during fitting.
#'       }
#'     \item For models with **multiple residual errors where at least one is
#'       log-additive**: The log-additive error(s) are treated as proportional
#'       during fitting, regardless of the `logTransform` value.
#'     \item For models **without log-additive residual errors**: This setting is
#'       generally ignored by the engine concerning special log-additive handling.
#'   }
#'   Note: This function includes `logTransform` in the output string if it's
#'   not `NULL`. The ultimate applicability and interpretation are handled by
#'   the NLME engine based on the detailed model structure.
#' @param numIntegratePtsAGQ Integer; Number of quadrature points per dimension
#'   for Adaptive Gaussian Quadrature (AGQ). 1 means no AGQ. >1 enables AGQ.
#'   **Default: `1L`**. *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param numIterNonParametric Integer; Controls non-parametric (NP)
#'   optimization. 0: Disable. 1: NONMEM-style NP. >1: Evolutionary NP algorithm
#'   generations.
#'   **Default: `0L`**. *(Population models, `method != "Naive-Pooled"` only)*.
#' @param fastOptimization Logical; Use Automatic Differentiation (`TRUE`) or Finite
#'   Difference (`FALSE`) for optimizing random effects (etas). **Default:
#'   `FALSE`**.
#'   *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param numIterMAPNP Integer; Number of preliminary Naive-Pooled iterations.
#'   **Default: `0L`**. *(Population models, `method != "Naive-Pooled"` only)*.
#' @param numRepPCWRES Integer; Replicates for PCWRES (max 10000). 0 disables
#'   calculation.
#'   **Default: `0L`**. *(Population models, `method != "Naive-Pooled"` only)*.
#' @param stepSizeLinearize Numeric; Relative step size for numerical
#'   differentiation during linearization. Must be positive. **Default:
#'   `0.002`**.
#'   *(Population models? Check NLME docs)*.
#' @param numDigitLaplacian Integer; Optimization accuracy (NDIGIT) for the
#'   outer loop (`"FOCE-ELS"`/`"Laplacian"`). Positive integer. **Default:
#'   `7L`**.
#'   *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param numDigitBlup Integer; Optimization accuracy (NDIGIT) for the inner
#'   loop (etas) or for `"Naive-Pooled"`. Positive integer. **Default: `13L`**.
#'   *(Population models or `method = "Naive-Pooled"`)*.
#' @param gradTolOuter Numeric; Max gradient tolerance, outer loop.
#'   Non-negative.
#'   **Default: `2e-4`**. *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param stepTolOuter Numeric; Max step tolerance, outer loop. Non-negative.
#'   **Default: `1e-4`**. *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param gradTolInner Numeric; Max gradient tolerance, inner loop (etas).
#'   Non-negative.
#'   **Default: `1.71e-5`**. *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param stepTolInner Numeric; Max step tolerance, inner loop (etas).
#'   Non-negative.
#'   **Default: `7.07e-8`**. *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param refDeltaLagl Numeric; Log-likelihood change tolerance. Non-negative.
#'   **Default: `1e-3`**. *(Population models, `method = "FOCE-ELS"` or `"Laplacian"` only)*.
#' @param mapAssist Integer; Periodicity for MAP assistance. 0 disables.
#'   **Default: `0L`**. *(Population models, `method = "QRPEM"` only)*.
#' @param iSample Integer; Sample points. Positive integer.
#'   **Default: `300L`**. *(Population models, `method = "QRPEM"` only)*.
#' @param iAcceptRatio Numeric; Acceptance ratio for covariance scaling.
#'   Positive.
#'   **Default: `0.1`**. *(Population models, `method = "QRPEM"` only)*.
#' @param impDist Character; Importance sampling distribution. Options:
#'   `"Normal"`, etc.
#'   **Default: `"Normal"`**. *(Population models, `method = "QRPEM"` only)*.
#' @param tDOF Integer; Degrees of freedom for T distribution importance
#'   sampling (3-30).
#'   **Default: `4L`**. *(Population models, `method = "QRPEM"` and `impDist = "T"` only)*.
#' @param numSampleSIR Integer; Samples per eta per subject for SIR. Positive
#'   integer.
#'   **Default: `10L`**. *(Population models, `method = "QRPEM"` only)*.
#' @param numBurnIn Integer; Burn-in iterations.
#'   **Default: `0L`**. *(Population models, `method = "QRPEM"` only)*.
#' @param freezeOmega Logical; Freeze Omega during burn-in.
#'   **Default: `FALSE`**. *(Population models, `method = "QRPEM"` only)*.
#' @param MCPEM Logical; Use Monte-Carlo (`TRUE`) or Quasi-Random (`FALSE`)
#'   sampling.
#'   **Default: `FALSE`**. *(Population models, `method = "QRPEM"` only)*.
#' @param runAllIterations Logical; Force execution of all iterations.
#'   **Default: `FALSE`**. *(Population models, `method = "QRPEM"` only)*.
#' @param scramble Character; Quasi-random scrambling. Options: `"Owen"`, etc.
#'   **Default: `"Owen"`**. *(Population models, `method = "QRPEM"` only)*.
#' @param emTolType Integer; QRPEM convergence check type (0-3).
#'   **Default: `0L`**. *(Population models, `method = "QRPEM"` only)*.
#' @param emConvLen Integer; Iterations for QRPEM convergence check window.
#'   Positive.
#'   **Default: `10L`**. *(Used when `emTolType` > 0, QRPEM only)*.
#' @param emConvCritVal Numeric; Critical value for QRPEM convergence check.
#'   Positive.
#'   **Default: `5.0`**. *(Used when `emTolType` > 0, QRPEM only)*.
#' @param stepSizePartialDeriv Numeric; Step size for numerical partial
#'   derivatives. Positive.
#'   **Default: `1e-5`**. *(Individual models only)*.
#' @param numTimeStepPartialDeriv Integer; Time steps for outputting partial
#'   derivatives. Positive integer.
#'   **Default: `20L`**. *(Individual models only)*.
#'
#' @return A **single character string** containing space-separated `name=value`
#'   pairs, ordered according to the function signature. Includes parameters if
#'   specified with non-default values (using method-specific defaults for
#'   `stdErr`).
#'
#' @seealso [write_ModelTemplateTokens()], [specify_SimParams()]
#' @family Engine Specification
#'
#' @examples
#' # Default settings
#' EstArgs_def <- specify_EngineParams()
#' print(EstArgs_def)
#'
#' # Setting sort = TRUE
#' EstArgs_sort_true <- specify_EngineParams(sort = TRUE)
#' print(EstArgs_sort_true)
#'
#' # QRPEM method with several custom settings
#' EstArgs_qrpem_str <-
#'   specify_EngineParams(
#'     sort = TRUE, # Explicitly non-default
#'     ODE = "DVERK",
#'     rtolODE = 1e-5,
#'     numIterations = 500,
#'     method = "QRPEM",
#'     isCentralDiffStdErr = FALSE,
#'     numIterMAPNP = 3,
#'     iSample = 350,
#'     impDist = "Mixture-2",
#'     scramble = "Tezuka-Faur",
#'     numBurnIn = 10,
#'     freezeOmega = TRUE
#'   )
#' print(EstArgs_qrpem_str)
#'
#' @export
specify_EngineParams <-
  function(sort = FALSE,
           # <-- CHANGED DEFAULT
           ODE = c("MatrixExponent",
                   "DVERK",
                   "DOPRI5",
                   "AutoDetect",
                   "Stiff",
                   "LSODE"),
           rtolODE = 1e-6,
           atolODE = 1e-6,
           maxStepsODE = 50000L,
           numIterations = 1000L,
           method = c("FOCE-ELS",
                      "QRPEM",
                      "Laplacian",
                      "Naive-Pooled",
                      "FOCE-LB",
                      "IT2S-EM",
                      "FO"),
           stdErr = c("Sandwich", "Hessian", "Fisher-Score", "Auto-Detect", "None"),
           isCentralDiffStdErr = TRUE,
           stepSizeStdErr = 0.01,
           logTransform = NULL,
           numIntegratePtsAGQ = 1L,
           numIterNonParametric = 0L,
           fastOptimization = FALSE,
           numIterMAPNP = 0L,
           numRepPCWRES = 0L,
           stepSizeLinearize = 0.002,
           numDigitLaplacian = 7L,
           numDigitBlup = 13L,
           gradTolOuter = 2e-4,
           stepTolOuter = 1e-4,
           gradTolInner = 1.71e-5,
           stepTolInner = 7.07e-8,
           refDeltaLagl = 1e-3,
           mapAssist = 0L,
           iSample = 300L,
           iAcceptRatio = 0.1,
           impDist = c("Normal",
                       "DoubleExponential",
                       "Direct",
                       "T",
                       "Mixture-2",
                       "Mixture-3"),
           tDOF = 4L,
           numSampleSIR = 10L,
           numBurnIn = 0L,
           freezeOmega = FALSE,
           MCPEM = FALSE,
           runAllIterations = FALSE,
           scramble = c("Owen", "Tezuka-Faur", "None"),
           emTolType = 0L,
           emConvLen = 10L,
           emConvCritVal = 5.0,
           stepSizePartialDeriv = 1e-5,
           numTimeStepPartialDeriv = 20L) {
    # --- Get function formals and their evaluated default values ---
    formal_args <- formals(specify_EngineParams)
    evaluated_defaults <-
      lapply(formal_args, eval, envir = environment(specify_EngineParams))
    all_param_names <- names(formal_args) # Keep this order

    # --- Get the actual argument values used in this function call ---
    resolved_args <- mget(all_param_names, envir = environment())

    # --- Resolve Choices and Validate ALL arguments ---
    resolved_args$ODE <-
      tryCatch(
        match.arg(resolved_args$ODE, choices = evaluated_defaults$ODE),
        error = function(e)
          stop("Invalid 'ODE': ", conditionMessage(e), call. = FALSE)
      )
    resolved_args$method <-
      tryCatch(
        match.arg(resolved_args$method, choices = evaluated_defaults$method),
        error = function(e)
          stop("Invalid 'method': ", conditionMessage(e), call. = FALSE)
      )
    resolved_args$stdErr <-
      tryCatch(
        match.arg(resolved_args$stdErr, choices = evaluated_defaults$stdErr),
        error = function(e)
          stop("Invalid 'stdErr': ", conditionMessage(e), call. = FALSE)
      )
    resolved_args$impDist <-
      tryCatch(
        match.arg(resolved_args$impDist, choices = evaluated_defaults$impDist),
        error = function(e)
          stop("Invalid 'impDist': ", conditionMessage(e), call. = FALSE)
      )
    resolved_args$scramble <-
      tryCatch(
        match.arg(resolved_args$scramble, choices = evaluated_defaults$scramble),
        error = function(e)
          stop("Invalid 'scramble': ", conditionMessage(e), call. = FALSE)
      )

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
    validate_logical <- function(val, name) {
      if (!is.logical(val) ||
          length(val) != 1 ||
          is.na(val))
        stop("'", name, "' must be a single TRUE or FALSE value.", call. = FALSE)
    }

    # Apply validations to values in resolved_args
    validate_logical(resolved_args$sort, "sort")

    validate_numeric(resolved_args$rtolODE, "rtolODE", min_val = .Machine$double.eps)

    validate_numeric(resolved_args$atolODE, "atolODE", min_val = .Machine$double.eps)

    validate_numeric(
      resolved_args$maxStepsODE,
      "maxStepsODE",
      min_val = 1,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$numIterations,
      "numIterations",
      min_val = 0,
      max_val = 10000,
      integer_only = TRUE
    )

    validate_logical(resolved_args$isCentralDiffStdErr, "isCentralDiffStdErr")

    validate_numeric(resolved_args$stepSizeStdErr,
                     "stepSizeStdErr",
                     min_val = .Machine$double.eps)

    if (!is.null(resolved_args$logTransform)) {
      validate_logical(resolved_args$logTransform, "logTransform")
    }

    validate_numeric(
      resolved_args$numIntegratePtsAGQ,
      "numIntegratePtsAGQ",
      min_val = 1,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$numIterNonParametric,
      "numIterNonParametric",
      min_val = 0,
      integer_only = TRUE
    )

    validate_logical(resolved_args$fastOptimization, "fastOptimization")

    validate_numeric(
      resolved_args$numIterMAPNP,
      "numIterMAPNP",
      min_val = 0,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$numRepPCWRES,
      "numRepPCWRES",
      min_val = 0,
      max_val = 10000,
      integer_only = TRUE
    )

    validate_numeric(resolved_args$stepSizeLinearize,
                     "stepSizeLinearize",
                     min_val = .Machine$double.eps)

    validate_numeric(
      resolved_args$numDigitLaplacian,
      "numDigitLaplacian",
      min_val = 1,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$numDigitBlup,
      "numDigitBlup",
      min_val = 1,
      integer_only = TRUE
    )

    validate_numeric(resolved_args$gradTolOuter, "gradTolOuter", min_val = 0)
    validate_numeric(resolved_args$stepTolOuter, "stepTolOuter", min_val = 0)
    validate_numeric(resolved_args$gradTolInner, "gradTolInner", min_val = 0)
    validate_numeric(resolved_args$stepTolInner, "stepTolInner", min_val = 0)
    validate_numeric(resolved_args$refDeltaLagl, "refDeltaLagl", min_val = 0)
    validate_numeric(
      resolved_args$mapAssist,
      "mapAssist",
      min_val = 0,
      integer_only = TRUE
    )

    validate_numeric(resolved_args$iSample,
                     "iSample",
                     min_val = 1,
                     integer_only = TRUE)

    validate_numeric(resolved_args$iAcceptRatio,
                     "iAcceptRatio",
                     min_val = .Machine$double.eps)

    validate_numeric(
      resolved_args$tDOF,
      "tDOF",
      min_val = 3,
      max_val = 30,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$numSampleSIR,
      "numSampleSIR",
      min_val = 1,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$numBurnIn,
      "numBurnIn",
      min_val = 0,
      integer_only = TRUE
    )

    validate_logical(resolved_args$freezeOmega, "freezeOmega")

    validate_logical(resolved_args$MCPEM, "MCPEM")

    validate_logical(resolved_args$runAllIterations, "runAllIterations")

    validate_numeric(
      resolved_args$emTolType,
      "emTolType",
      min_val = 0,
      max_val = 3,
      integer_only = TRUE
    )

    validate_numeric(
      resolved_args$emConvLen,
      "emConvLen",
      min_val = 1,
      integer_only = TRUE
    )

    validate_numeric(resolved_args$emConvCritVal,
                     "emConvCritVal",
                     min_val = .Machine$double.eps)

    validate_numeric(
      resolved_args$stepSizePartialDeriv,
      "stepSizePartialDeriv",
      min_val = .Machine$double.eps
    )

    validate_numeric(
      resolved_args$numTimeStepPartialDeriv,
      "numTimeStepPartialDeriv",
      min_val = 1,
      integer_only = TRUE
    )

    # --- Adjust stdErr based on resolved method ---
    method_resolved <- resolved_args$method
    stdErr_resolved <- resolved_args$stdErr # Use the matched value
    if (method_resolved == "QRPEM" &&
        !(stdErr_resolved %in% c("Fisher-Score", "None"))) {
      message(
        "For method='QRPEM', stdErr must be 'Fisher-Score' or 'None'. Setting stdErr='Fisher-Score'."
      )
      resolved_args$stdErr <- "Fisher-Score"
    } else if (method_resolved == "IT2S-EM" &&
               stdErr_resolved != "None") {
      message("For method='IT2S-EM', stdErr must be 'None'. Setting stdErr='None'.")
      resolved_args$stdErr <- "None"
    }

    # --- Collect Non-Default AND Applicable Values ---
    output_args_list <- list()

    for (p_name in all_param_names) {
      # Iterate in signature order
      current_value <- resolved_args[[p_name]]
      effective_default <- evaluated_defaults[[p_name]]

      if (is.character(effective_default) &&
          length(effective_default) > 1) {
        effective_default <- effective_default[1]
      }

      # Determine if different from default (special case for stdErr)
      if (p_name == "stdErr") {
        method_default_stdErr <- "Sandwich"
        if (method_resolved == "QRPEM")
          method_default_stdErr <- "Fisher-Score"
        if (method_resolved == "IT2S-EM")
          method_default_stdErr <- "None"
        is_different <-
          !identical(current_value, method_default_stdErr)
      } else {
        is_different <- !identical(current_value, effective_default)
      }

      # Proceed only if it's 'sort' or different from its effective default
      if (p_name == "sort" || is_different) {
        # Now check applicability for this non-default (or sort) parameter
        param_applicable <- TRUE
        inapplicable_reason <- "" # Store reason for warning message

        if (resolved_args$ODE == "MatrixExponent" &&
            p_name %in% c("rtolODE", "atolODE", "maxStepsODE")) {
          param_applicable <- FALSE
          inapplicable_reason <- "ODE is MatrixExponent"
        }
        qrpem_only_args <-
          c(
            "mapAssist",
            "iSample",
            "iAcceptRatio",
            "impDist",
            "tDOF",
            "numSampleSIR",
            "numBurnIn",
            "freezeOmega",
            "MCPEM",
            "runAllIterations",
            "scramble",
            "emTolType",
            "emConvLen",
            "emConvCritVal"
          )
        if (resolved_args$method != "QRPEM" &&
            p_name %in% qrpem_only_args) {
          param_applicable <- FALSE
          inapplicable_reason <- paste("method is not QRPEM")
        }
        if (resolved_args$impDist != "T" && p_name == "tDOF") {
          param_applicable <- FALSE
          inapplicable_reason <-
            paste(inapplicable_reason, "impDist is not T") # Append reason
        }
        if (resolved_args$emTolType == 0 &&
            p_name %in% c("emConvLen", "emConvCritVal")) {
          param_applicable <- FALSE
          inapplicable_reason <-
            paste(inapplicable_reason, "emTolType is 0")
        }
        foce_lap_only_args <-
          c(
            "numIntegratePtsAGQ",
            "fastOptimization",
            "numDigitLaplacian",
            "gradTolOuter",
            "stepTolOuter",
            "gradTolInner",
            "stepTolInner",
            "refDeltaLagl"
          )
        if (!(resolved_args$method %in% c("FOCE-ELS", "Laplacian")) &&
            p_name %in% foce_lap_only_args) {
          param_applicable <- FALSE
          inapplicable_reason <-
            paste(inapplicable_reason,
                  "method is not FOCE-ELS or Laplacian")
        }
        pop_not_naive_args <-
          c("numIterNonParametric", "numIterMAPNP", "numRepPCWRES")
        if (resolved_args$method == "Naive-Pooled" &&
            p_name %in% pop_not_naive_args) {
          param_applicable <- FALSE
          inapplicable_reason <-
            paste(inapplicable_reason, "method is Naive-Pooled")
        }
        individual_only_args <-
          c("stepSizePartialDeriv", "numTimeStepPartialDeriv")
        if (resolved_args$method != "Naive-Pooled" &&
            p_name %in% individual_only_args) {
          param_applicable <- FALSE
          inapplicable_reason <-
            paste(inapplicable_reason, "method is not Naive-Pooled")
        }

        # Add to list if applicable, otherwise issue warning
        if (param_applicable) {
          output_args_list[[p_name]] <- current_value
        } else {
          # Generate specific warning message
          warning(
            "Parameter '",
            p_name,
            "' was set to a non-default value '",
            current_value,
            "' but is inapplicable because ",
            trimws(inapplicable_reason),
            ". It is excluded from the output string.",
            call. = FALSE
          )
        }
      } # End if (is_different or sort)
    }

    # --- Format Output String ---
    if (length(output_args_list) == 0) {
      return("")
    } else {
      # Output list IS already in signature order because we iterated that way
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
      return(paste(c("", output_strings), collapse = " "))
    }
  }
