#' Specify engine parameters for model simulation
#'
#' Use to define engine parameters for model simulation.
#'
#' @inheritParams specify_EngineParams
#' @param  numReplicates Integer; Number of replicates to simulate the model
#' @param  seed Integer; Random number generator seed
#'
#' @return Character
#' @examples
#' SimArgs1 <- specify_SimParams()
#'
#' SimArgs2 <-
#'   specify_SimParams(
#'     numReplicates = 100,
#'     seed = 1,
#'     ODE = "DVERK")
#'
#' @seealso [write_ModelTemplateTokens()], [specify_EngineParams()], [Table()]
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
    FormalArguments <- formals(specify_SimParams)

    OutputSimParams <- ""

    OutputSimParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        OutputAnyway = TRUE,
        Arg = numReplicates
      )

    OutputSimParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        OutputAnyway = TRUE,
        Arg = seed
      )

    OutputSimParams <-
      .output_Logical(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        OutputAnyway = TRUE,
        Arg = sort
      )

    OutputSimParams <-
      .output_Textual(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        Arg = ODE
      )

    OutputSimParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        OutputAnyway = FALSE,
        Arg = rtolODE
      )

    OutputSimParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        OutputAnyway = FALSE,
        Arg = atolODE
      )

    OutputSimParams <-
      .output_Numeric(
        FormalArguments = FormalArguments,
        OutputEngineParams = OutputSimParams,
        OutputAnyway = FALSE,
        Arg = maxStepsODE
      )

    OutputSimParams
  }
