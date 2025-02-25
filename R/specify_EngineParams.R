#' Specify engine parameters for model execution
#'
#' Use to define extra engine parameters for model execution.
#'
#' @param sort Logical; Specifying whether or not to sort the input data by
#'   subject and time values. Default is `TRUE`.
#' @param ODE Character; Specifying the solver used to numerically solve
#'   Ordinary Differential Equations (ODEs). Options are
#' * `MatrixExponent` (the default),
#' * `DVERK`,
#' * `DOPRI5`,
#' * `AutoDetect`,
#' * `Stiff`.
#'
#' Note: both `DVERK` and `DOPRI5` are non-stiff solvers. NLME will
#' automatically switches to `DVERK` if ODEs are nonlinear.
#' @param rtolODE Numeric; Specifying relative tolerance for the ODE solver.
#' Not applicable when `ODE == MatrixExponent`.
#' @param atolODE Numeric; Specifying absolute tolerance for the ODE solver.
#' @param maxStepsODE Numeric; Specifying maximum number of allowable steps or
#'   function evaluations for the ODE solver.
#' @param numIterations  Numeric; Specifying maximum number of iterations for
#'   estimation.
#' @param method Character; Specifying engine method for estimation. Options
#'   are:
#' * `FOCE-ELS` (the default),
#' * `QRPEM`,
#' * `Laplacian`,
#' * `Naive-Pooled`,
#' * `FOCE-LB`,
#' * `IT2S-EM`,
#' * `FO`.
#'
#' Note: if model involves any discontinuous observed variable (e.g., count
#' data) or BQL data, NLME will switch from default method `FOCE-ELS` to
#' `Laplacian`.
#' @param stdErr Character; Specifying method for standard error computations.
#'   Options are:
#' * `Auto-Detect` (the default),
#' * `Sandwich`,
#' * `Hessian`,
#' * `Fisher-Score`,
#' * `None`.
#'
#' Here `None` means that standard error calculations are not performed. Since
#' when `method = QRPEM` only `Fisher-Score` standard error type is available
#' in NLME, any selected option except `None` will reset to `stdErr = "Fisher-Score"`.
#' @param isCentralDiffStdErr Logical; Default `TRUE` uses central difference
#'   for `stdErr` calculations. Set to `FALSE` for forward difference method.
#' @param stepSizeStdErr Numeric; Specifying the step size used for `stdErr`
#'   calculations.
#' @param numIntegratePtsAGQ Numeric; Specifying the number of integration
#'   points for adaptive Gaussian quadrature (AGQ) algorithm. Only applicable to
#'   models with `method` set to either `FOCE-ELS` or `Laplacian`.
#' @param numIterNonParametric Numeric;  Specifying the number of iterations to
#'   perform non-parametric estimation. Only applicable when `method` is not set
#'   to `Naive-Pooled` (otherwise ignored).
#' @param allowSyntheticGradient Logical, Set to `TRUE` to use synthetic
#'   gradient during the estimation process. Only applicable to population
#'   models when `method` is not set to `Naive-Pooled` (otherwise ignored).
#' @param numIterMAPNP Numeric; Specifying the number of iterations to perform
#'   Maximum A Posterior (MAP) initial Naive Pooling (NP) run before estimation.
#'   Only applicable to population models when `method` is not set to
#'   `Naive-Pooled` (otherwise ignored).
#' @param numRepPCWRES Numeric; Specifying the number of replicates to generate
#'   the PCWRES after the simple estimation. Only applicable to the models when
#'   `method` is not set to `Naive-Pooled`.
#' @param numRepPCWRES Numeric; Specifying the number of replicates to generate
#'   the PCWRES after the simple estimation. Only applicable to population
#'   models when `method` is not set to `Naive-Pooled` (otherwise ignored).
#' @param stepSizeLinearize Numeric; Specifying the step size used for numerical
#'   differentiation when linearizing the model function during the estimation
#'   process.
#' @param numDigitLaplacian Numeric; Specifying the number of significant
#'   decimal digits for the Laplacian/ELS algorithm to use to reach convergence.
#' @param numDigitBlup Numeric; Specifying the number of significant decimal
#'   digits for the individual estimation to use to reach convergence.
#' @param mapAssist Numeric; Specifying the period used to perform MAP
#'   assistance (`mapAssist = 0` means that MAP assistance is not performed).
#'   Only applicable when `method == "QRPEM"`.
#' @param iSample Numeric; Specifying the number of samples. Only applicable
#'   when `method == "QRPEM"`.
#' @param iAcceptRatio Numeric; Specifying the acceptance ratio. Only applicable
#'   when `method == "QRPEM"`.
#' @param impDist Character; Specifying the distribution used for important
#'   sampling, and options are
#' * `Normal` (the default),
#' * `DoubleExponential`,
#' * `Direct`,
#' * `T`,
#' * `Mixture-2`,
#' * `Mixture-3`.
#'
#' Only applicable to the model with `method = "QRPEM"`.
#' @param tDOF Numeric; Specifying the degree of freedom (allowed value is
#'   between 3 and 30) for T distribution. Only applicable when `method =="QRPEM"`
#'   and `impDist == "T"`.
#' @param numSampleSIR Numeric; Specifying the number of samples per subject
#'   used in the Sampling Importance Re-Sampling (SIR) algorithm to determine
#'   the number of SIR samples taken from the empirical discrete distribution
#'   that approximates the target conditional distribution. Only applicable to
#'   population models with `method = "QRPEM"`.
#' @param numBurnIn Numeric; Specifying the number of burn-in iterations to
#'   perform at startup to adjust certain internal parameters. Only applicable
#'   to population models with `method = "QRPEM"`.
#' @param freezeOmega Logical; Set to `TRUE` to freeze Omega but not Theta for
#'   the number of iterations specified in the `numBurnIn`. Only applicable to
#'   population models with `method = "QRPEM"`.
#' @param MCPEM Logical; Set to `TRUE` to use Monte-Carlo sampling instead of
#'   Quasi-Random. Only applicable to population models with `method = "QRPEM"`.
#' @param runAllIterations Logical; Set to `TRUE` to execute all requested
#'   iterations specified in `numIterations`. Only applicable to population
#'   models with `method = "QRPEM"`.
#' @param scramble Character; Specifying the quasi-random scrambling method to
#'   use, and options are
#' * `Owen` (the default),
#' * `Tezuka-Faur`,
#' * `None`.
#'
#' Only applicable to population models with `method = "QRPEM"`.
#'
#' @seealso [write_ModelTemplateTokens()], [specify_SimParams()]
#' @return Character
#'
#' @examples
#' # default
#' EstArgs <- specify_EngineParams()
#' # QRPEM method
#' EstArgs <-
#'   specify_EngineParams(
#'     sort = TRUE,
#'     ODE = "DVERK",
#'     rtolODE = 1e-5,
#'     atolODE = 1e-5,
#'     maxStepsODE = 6000,
#'     numIterations = 100,
#'     method = "QRPEM",
#'     numIterMAPNP = 3,
#'     stdErr = "Fisher-Score",
#'     isCentralDiffStdErr = FALSE,
#'     iSample = 350,
#'     impDist = "Mixture-2",
#'     scramble = "Tezuka-Faur")
#'
#' @export
specify_EngineParams <-
  function(sort = FALSE,
           ODE = c("MatrixExponent", "DVERK", "DOPRI5", "AutoDetect", "Stiff"),
           rtolODE = 1e-6,
           atolODE = 1e-6,
           maxStepsODE = 50000L,
           numIterations = 1000L,
           method = c(
             "FOCE-ELS",
             "QRPEM",
             "Laplacian",
             "Naive-Pooled",
             "FOCE-LB",
             "IT2S-EM",
             "FO"
           ),
           stdErr = c("Sandwich", "Auto-Detect", "Hessian", "Fisher-Score", "None"),
           isCentralDiffStdErr = TRUE,
           stepSizeStdErr = 0.01,
           numIntegratePtsAGQ = 1L,
           numIterNonParametric = 0L,
           allowSyntheticGradient = FALSE,
           numIterMAPNP = 0L,
           numRepPCWRES = 0L,
           stepSizeLinearize = 0.002,
           numDigitLaplacian = 7L,
           numDigitBlup = 13L,
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
           scramble = c("Owen", "Tezuka-Faur", "None")) {
    FormalArguments <- formals(specify_EngineParams)

    OutputEngineParams <- ""
    OutputEngineParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        OutputAnyway = TRUE,
        Arg = sort
      )

    OutputEngineParams <-
      .output_Textual(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = ODE
      )
    # need this later:
    ODE <- match.arg(ODE)

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = rtolODE
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = atolODE
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = maxStepsODE
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numIterations
      )

    OutputEngineParams <-
      .output_Textual(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = method
      )
    # need this later:
    method <- match.arg(method)

    if (method == "QRPEM") {
      if (length(stdErr) == 1 &&
                 stdErr %in% c("Auto-Detect", "Sandwich", "Hessian")) {
        message("stdErr specified '", stdErr, "' is not compatible with method='", method, "'.",
                " Switching to 'Fisher-Score'.")
        stdErr <- "Fisher-Score"
      }
    } else if (method == "IT2S-EM") {
      if (length(stdErr) == 1 &&
                 stdErr != "None") {
        message("stdErr specified '", stdErr, "' is not compatible with method='", method, "'.",
                " Switching to 'None'.")

        stdErr <- "None"
      }
    }

    OutputEngineParams <-
      .output_Textual(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = stdErr
      )

    OutputEngineParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = isCentralDiffStdErr
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = stepSizeStdErr
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numIntegratePtsAGQ
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numIterNonParametric
      )

    OutputEngineParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = allowSyntheticGradient
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numIterMAPNP
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numRepPCWRES
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = stepSizeLinearize
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numDigitLaplacian
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numDigitBlup
      )

    if (method != "QRPEM") {
      return(OutputEngineParams)
    }

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = mapAssist
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = iSample
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = iAcceptRatio
      )

    OutputEngineParams <-
      .output_Textual(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = impDist
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = tDOF
      )

    if (tDOF < 3 || tDOF > 30 && impDist == "T") {
      stop("tDOF should be between 3 and 30, not ", tDOF)
    }

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numSampleSIR
      )

    OutputEngineParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = numBurnIn
      )

    OutputEngineParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = freezeOmega
      )

    OutputEngineParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = MCPEM
      )

    OutputEngineParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = runAllIterations
      )

    OutputEngineParams <-
      .output_Textual(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputEngineParams,
        Arg = scramble
      )

    OutputEngineParams
  }

.output_Textual <-
  function(FormalArguments,
           OutputEngineParams = "",
           Arg,
           ArgName = deparse(substitute(Arg))) {
    Formals <- eval(FormalArguments[[ArgName]])
    if (length(Arg) > 1 &&
        length(unique(Arg)) == length(Formals) &&
        all(!is.na(match(Arg, Formals)))) {
      # default one, no need to output
      return(OutputEngineParams)
    }

    Arg <- match.arg(Arg, Formals)

    paste(OutputEngineParams, paste(ArgName, Arg, sep = "="))
  }

.output_Logical <-
  function(FormalArguments,
           OutputEngineParams = "",
           OutputAnyway = FALSE,
           Arg,
           ArgName = deparse(substitute(Arg))) {
    if (length(Arg) != 1 || !is.logical(Arg)) {
      stop(ArgName,
           " passed to engine parameters could be only TRUE or FALSE.")
    }

    if (OutputAnyway) {
      return(paste(OutputEngineParams, paste(ArgName, Arg, sep = "=")))
    }

    Formals <- eval(FormalArguments[[ArgName]])
    if (Arg == Formals) {
      OutputEngineParams
    } else {
      paste(OutputEngineParams, paste(ArgName, Arg, sep = "="))
    }
  }

.output_Numeric <-
  function(FormalArguments,
           OutputEngineParams = "",
           OutputAnyway = FALSE,
           Arg,
           ArgName = deparse(substitute(Arg))) {
    if (length(Arg) != 1 || !is.numeric(Arg)) {
      stop(ArgName,
           " passed to engine parameters should be single numeric value.")
    }

    if (FormalArguments[[ArgName]] %% 1 == 0 &&
        Arg %% 1 != 0) {
      stop(ArgName, " passed to engine parameters should be integer.")
    }

    if (FormalArguments[[ArgName]] != Arg ||
        OutputAnyway) {
      paste(OutputEngineParams, paste(ArgName, Arg, sep = "="))
    } else {
      # do not output default numeric
      OutputEngineParams
    }
  }
