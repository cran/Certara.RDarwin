#' Run pyDarwin Model Search
#'
#' This function executes a pyDarwin model search by calling the specified Python
#' interpreter and the `darwin.run_search` module.
#'
#' @param InterpreterPath Character string. The full path to the Python
#'   interpreter executable (e.g., `python.exe` or `python`).
#' @param Flags Character vector. Optional flags passed directly to the Python
#'   interpreter. Defaults to `c("-u", "-m")`. `-u` forces unbuffered binary
#'   stdout and stderr streams. `-m` runs a library module as a script and is
#'   essential for calling `darwin.run_search`.
#' @param DirectoryPath Character string. Optional path to the directory
#'   containing the `template.txt`, `tokens.json`, and `options.json` files. If
#'   provided, this path is used to locate these files, overriding any directory
#'   information in the `TemplatePath`, `TokensPath`, and `OptionsPath` arguments
#'   (a warning will be issued). Defaults to the current R working directory.
#' @param TemplatePath Character string. Path to the pyDarwin template file
#'   (typically `template.txt`). If `DirectoryPath` is specified, only the
#'   basename of `TemplatePath` is used, combined with `DirectoryPath`.
#' @param TokensPath Character string. Path to the pyDarwin tokens JSON file
#'   (typically `tokens.json`). If `DirectoryPath` is specified, only the
#'   basename of `TokensPath` is used, combined with `DirectoryPath`.
#' @param OptionsPath Character string. Path to the pyDarwin options JSON file
#'   (typically `options.json`). This file defines run settings like
#'   `working_dir`, `output_dir`, etc. If `DirectoryPath` is specified, only the
#'   basename of `OptionsPath` is used, combined with `DirectoryPath`.
#' @param Wait Logical. If `TRUE` (default), R waits for the pyDarwin process to
#'   complete before proceeding. If `FALSE`, R launches the pyDarwin process in
#'   the background and returns immediately. See the 'Background Execution'
#'   section for important details when using `Wait = FALSE`.
#'
#' @return
#' \itemize{
#'   \item If `Wait = TRUE`: A list containing the search results read from the
#'     `output_dir` (specified in `options.json`). This typically includes:
#'     \itemize{
#'       \item `results`: A data frame (`results.csv`).
#'       \item `FinalResultFile`: Character vector containing lines from the
#'         final model's result file (e.g., `.lst`, `.txt`).
#'       \item `FinalControlFile`: Character vector containing lines from the
#'         final model's control file (e.g., `.mod`, `.mmdl`).
#'     }
#'     If result files are not found, warnings are issued. If no results are
#'     found but `messages.txt` exists, its content might be returned with a
#'     warning. If the Python call fails (non-zero exit code), the function stops
#'     with an error.
#'   \item If `Wait = FALSE`: A character string giving the full path to the main
#'     pyDarwin log file (`messages.txt`) within the `working_dir`.
#' }
#'
#' @section Background Execution (`Wait = FALSE`):
#' When `Wait` is set to `FALSE`, the pyDarwin process is launched in the
#' background, and the R function returns immediately. This allows R to continue
#' processing while pyDarwin runs.
#' \itemize{
#'   \item **Output Redirection:** Standard output (stdout) and standard error
#'     (stderr) from the Python process are redirected to files named
#'     `stdout.log` and `stderr.log` respectively, within the `working_dir`.
#'     These files are crucial for diagnosing issues if the background process
#'     fails or behaves unexpectedly.
#'   \item **Primary Log:** The main pyDarwin log file (`messages.txt`, located
#'     in the `working_dir`) remains the primary source for detailed run
#'     information, but the `stdout.log` and `stderr.log` capture console
#'     output and errors directly.
#' }
#'
#' @seealso [run_pyDarwinRemote()], [reconnect_pyDarwinJob()]
#'
#' @examples
#' \dontrun{
#' # Example: Running pyDarwin and waiting for results
#' # Assuming python is in the PATH and input files are in 'my_project'
#'
#' results <- run_pyDarwin(
#'   InterpreterPath = "python",
#'   DirectoryPath = "my_project",
#'   Wait = TRUE
#' )
#' print(results$results)
#'
#' # Example: Launching pyDarwin in the background
#'
#' log_file_path <- run_pyDarwin(
#'   InterpreterPath = "python",
#'   DirectoryPath = "my_project",
#'   Wait = FALSE
#' )
#' }
#'
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom utils read.csv
#' @importFrom methods hasArg
run_pyDarwin <- function(InterpreterPath,
                         Flags = c("-u", "-m"),
                         DirectoryPath = ".",
                         TemplatePath = "template.txt",
                         TokensPath = "tokens.json",
                         OptionsPath = "options.json",
                         Wait = TRUE) {
  if (missing(InterpreterPath)) {
    stop("InterpreterPath must be specified.")
  } else if (!file.exists(InterpreterPath)) {
    # Try finding python on PATH if simple name given
    py_path <- Sys.which(InterpreterPath)
    if (py_path == "") {
      stop("InterpreterPath '",
           InterpreterPath,
           "' not found directly or on system PATH.")
    } else if (InterpreterPath != py_path) {
      message("Using interpreter found on PATH: ", py_path)
      InterpreterPath <- py_path
    }
  }

  stopifnot("'Wait' must be logical (TRUE or FALSE)" = is.logical(Wait))

  # Normalize DirectoryPath first if provided
  if (!missing(DirectoryPath) && methods::hasArg(DirectoryPath)) {
    if (!dir.exists(DirectoryPath)) {
      stop("Specified DirectoryPath does not exist: ", DirectoryPath)
    }
    DirectoryPath <-
      normalizePath(DirectoryPath, winslash = "/", mustWork = TRUE)

    # Construct full paths using DirectoryPath and basenames
    TemplatePath <- file.path(DirectoryPath, basename(TemplatePath))
    TokensPath <- file.path(DirectoryPath, basename(TokensPath))
    OptionsPath <- file.path(DirectoryPath, basename(OptionsPath))

  }

  # Check existence and normalize final paths
  TemplatePath <-
    normalizePath(TemplatePath, winslash = "/", mustWork = TRUE)

  TokensPath <-
    normalizePath(TokensPath, winslash = "/", mustWork = TRUE)

  OptionsPath <-
    normalizePath(OptionsPath, winslash = "/", mustWork = TRUE)

  # --- Determine Working and Output Directories from Options ---
  tryCatch({
    pyDarwinOptions <- jsonlite::fromJSON(OptionsPath)
  }, error = function(e) {
    stop("Failed to read or parse JSON OptionsPath '",
         OptionsPath,
         "': ",
         e$message)
  })

  ProjectDirAlias <- dirname(OptionsPath) # Already normalized

  # Determine working_dir
  if (is.null(pyDarwinOptions$working_dir)) {
    # Default working_dir logic
    PYDARWIN_HOME <- Sys.getenv("PYDARWIN_HOME")
    if (PYDARWIN_HOME == "") {
      if (.Platform$OS.type == "windows") {
        PYDARWIN_HOME <- Sys.getenv("USERPROFILE")
      } else {
        PYDARWIN_HOME <- Sys.getenv("HOME")
      }

      if (PYDARWIN_HOME == "") {
        # Fallback if HOME/USERPROFILE not set
        PYDARWIN_HOME <- tempdir()
        warning(
          "PYDARWIN_HOME, HOME, or USERPROFILE environment variables not set.",
          " Using temporary directory for default PYDARWIN_HOME: ",
          PYDARWIN_HOME
        )
      }
      PYDARWIN_HOME <- file.path(PYDARWIN_HOME, "pydarwin")
    }

    project_stem <- if (!is.null(pyDarwinOptions$project_name)) {
      pyDarwinOptions$project_name
    } else {
      basename(ProjectDirAlias)
    }

    project_stem <- gsub("[^a-zA-Z0-9_]", "_", project_stem)
    working_dir <- file.path(PYDARWIN_HOME, project_stem)
  } else {
    # User-specified working_dir logic
    working_dir <-
      gsub("{project_dir}",
           ProjectDirAlias,
           pyDarwinOptions$working_dir,
           fixed = TRUE)
    # Normalize path but allow it not to exist yet
    working_dir <-
      normalizePath(working_dir, winslash = "/", mustWork = FALSE)
  }

  # Ensure working directory exists (crucial for background runs)
  working_dir <- gsub("\\\\", "/", working_dir)
  if (!dir.exists(working_dir)) {
    if (!dir.create(working_dir,
                    recursive = TRUE,
                    showWarnings = TRUE)) {
      stop(
        "Failed to create required working directory: ",
        working_dir,
        ". Please check permissions or create it manually."
      )
    }
  }

  MessagesFile <- file.path(working_dir, "messages.txt")
  StdOutLogFile <- file.path(working_dir, "stdout.log")
  StdErrLogFile <- file.path(working_dir, "stderr.log")

  # Determine output_dir (relative to working_dir if specified)
  if (is.null(pyDarwinOptions$output_dir)) {
    # Default output is often within working_dir or specified explicitly
    # Assuming pyDarwin defaults if not set, might need adjustment
    output_dir <- file.path(working_dir, "output")
  } else {
    output_dir <-
      gsub("{project_dir}",
           ProjectDirAlias,
           pyDarwinOptions$output_dir,
           fixed = TRUE)
    # output_dir can be relative to working_dir
    output_dir <-
      gsub("{working_dir}", working_dir, output_dir, fixed = TRUE)
    # Normalize path but allow it not to exist yet
    output_dir <-
      normalizePath(output_dir, winslash = "/", mustWork = FALSE)
    output_dir <-
      gsub("\\\\", "/", output_dir) # Ensure forward slashes
  }

  engine_adapter <- pyDarwinOptions$engine_adapter
  if (!is.null(pyDarwinOptions$use_system_options) &&
      pyDarwinOptions$use_system_options &&
      file.exists(Sys.getenv("PYDARWIN_OPTIONS"))) {
    GlobalOptions <- NA
    # could be in global options
    GlobalOptions <-
      jsonlite::toJSON(Sys.getenv("PYDARWIN_OPTIONS"))
    if (!is.null(GlobalOptions$engine_adapter)) {
      engine_adapter <- GlobalOptions$engine_adapter
    }
  }

  if (is.null(engine_adapter)) {
    engine_adapter <- "nlme"
  }

  # --- Construct Command and Arguments ---
  cmd_args <- c(
    Flags,
    "darwin.run_search",
    shQuote(TemplatePath, type = "cmd"),
    shQuote(TokensPath, type = "cmd"),
    shQuote(OptionsPath, type = "cmd")
  )

  # Clean previous log files for background runs if they exist
  if (file.exists(StdOutLogFile))
    file.remove(StdOutLogFile)
  if (file.exists(StdErrLogFile))
    file.remove(StdErrLogFile)


  # Use system2 for execution
  tryCatch({
    ProcessValue <- system2(
      command = InterpreterPath,
      args = cmd_args,
      # Redirect stdout/stderr to files only for background runs
      stdout = if (!Wait) {
        StdOutLogFile
      } else {
        ""
      },
      stderr = if (!Wait) {
        StdErrLogFile
      } else {
        ""
      },
      wait = Wait
    )
  }, error = function(e) {
    # Catch errors during the system2 call itself (e.g., command not found)
    stop("Error executing system2 command '",
         InterpreterPath,
         "': ",
         e$message)
  })

  if (ProcessValue != 0) {
    stderr_content <- ""

    try({
      # Attempt to read stderr content if available
      if (file.exists(StdErrLogFile)) {
        stderr_content <- paste(
          "\n--- Stderr Log Content (if available) ---",
          paste(readLines(StdErrLogFile, warn = FALSE), collapse = "\n"),
          "--- End Stderr Log ---",
          sep = "\n"
        )
      }
    }, silent = TRUE)

    stdout_content <- ""
    try({
      # Attempt to read stdout content if available
      if (file.exists(StdOutLogFile)) {
        stdout_content <- paste(
          "\n--- Stdout Log Content (if available) ---",
          paste(readLines(StdOutLogFile, warn = FALSE), collapse = "\n"),
          "--- End Stdout Log ---",
          sep = "\n"
        )
      }
    }, silent = TRUE)

    stop(
      "Python call finished with exit code ",
      ProcessValue,
      ".\n",
      "Check the main log file: ",
      MessagesFile,
      "\n",
      stderr_content,
      stdout_content,
      "Or try running the command directly in a terminal:\n",
      paste(
        shQuote(InterpreterPath, type = "cmd"),
        paste(cmd_args, collapse = " ")
      )
    )
  }


  if (Wait) {
    # Process successful results if Wait=TRUE
    message("pyDarwin process completed. Reading results from: ",
            output_dir)
    ReturnedList <- list(
      results = data.frame(),
      FinalResultFile = character(),
      FinalControlFile = character()
    )
    NotFoundFiles <- c()

    # Ensure output_dir exists before trying to read from it
    if (!dir.exists(output_dir)) {
      warning(
        "Specified output directory does not exist: ",
        output_dir,
        ". Cannot read result files."
      )
    } else {
      resultsPath <- file.path(output_dir, "results.csv")
      if (file.exists(resultsPath)) {
        tryCatch({
          ReturnedList$results <-
            utils::read.csv(resultsPath)
        }, error = function(e) {
          warning("Failed to read results file '",
                  resultsPath,
                  "': ",
                  e$message)
          NotFoundFiles <<- c(NotFoundFiles, resultsPath)
        })
      } else {
        NotFoundFiles <- c(NotFoundFiles, resultsPath)
      }


      if (!pyDarwinOptions$algorithm  %in% c("MOGA", "MOGA3")) {
        # Determine expected final file names based on adapter
        if (engine_adapter == "nlme") {
          FinalResultFilePath <- file.path(output_dir, "FinalResultFile.txt")
          FinalControlFilePath <-
            file.path(output_dir, "FinalControlFile.mmdl")
        } else {
          FinalResultFilePath <-
            file.path(output_dir, "FinalResultFile.lst")
          FinalControlFilePath <-
            file.path(output_dir, "FinalControlFile.mod")
        }
        # Read final files
        if (file.exists(FinalResultFilePath)) {
          ReturnedList$FinalResultFile <-
            readLines(FinalResultFilePath, warn = FALSE)
        } else {
          NotFoundFiles <- c(NotFoundFiles, FinalResultFilePath)
        }

        if (file.exists(FinalControlFilePath)) {
          ReturnedList$FinalControlFile <-
            readLines(FinalControlFilePath, warn = FALSE)
        } else {
          NotFoundFiles <- c(NotFoundFiles, FinalControlFilePath)
        }
      }

    } # end if(dir.exists(output_dir))

    # Report missing files
    if (length(NotFoundFiles) > 0) {
      warning(
        "The following expected result files were not found in '",
        output_dir,
        "':\n",
        paste(NotFoundFiles, collapse = "\n")
      )
    }

    # Determine final return value
    if (nrow(ReturnedList$results) == 0 &&
        length(ReturnedList$FinalResultFile) == 0 &&
        length(ReturnedList$FinalControlFile) == 0) {
      # No structured results found
      if (file.exists(MessagesFile)) {
        warning(
          "Standard output result files not found in '",
          output_dir,
          "'.\n",
          "Returning the content of the main log file: ",
          MessagesFile
        )
        return(readLines(MessagesFile, warn = FALSE))
      } else {
        # No results and no message file - this shouldn't happen if exit code was 0
        warning(
          "Standard output result files not found and main log file '",
          MessagesFile,
          "' also not found."
        )
        return(list()) # Return empty list
      }
    } else {
      return(ReturnedList) # Return results found
    }

  } else {
    # If Wait=FALSE, return the path to the main log file
    message("pyDarwin launched in background. Monitor logs in: ",
            working_dir)
    return(MessagesFile)
  }
}

#' Stop pyDarwin Model Search
#'
#' This function stops a pyDarwin model search.
#'
#' @param InterpreterPath Path to the Python interpreter executable.
#' @param Flags Flags to pass to the Python interpreter. Refer to Python
#'   documentation for details. Note that `-m` is essential (runs library module
#'   as a script and terminates option list).
#' @param ForceStop Logical. If `TRUE`, `-f` flag is added to force stop search
#'   immediately.- Default is `FALSE`.
#' @param DirectoryPath the `DirectoryPath` argument of [run_pyDarwin()] or the
#'   parent folder of options file passed to that function.
#'   Default is current working directory.
#'
#' @return Returned code of [system2()] call.
#'
#' @examples
#' \dontrun{
#' stop_pyDarwin(
#'   InterpreterPath = "~/darwin/venv/bin/python",
#'   DirectoryPath = "~/project_folder")
#' }
#'
#' @export
stop_pyDarwin <- function(InterpreterPath,
                          Flags = c("-u", "-m"),
                          ForceStop = FALSE,
                          DirectoryPath = ".") {
  if (missing(InterpreterPath)) {
    stop("Cannot start without InterpreterPath specified.")
  } else if (!file.exists(InterpreterPath)) {
    stop("InterpreterPath ", InterpreterPath, " not found.")
  }

  stopifnot(is.logical(ForceStop))
  DirectoryPath <-
    shQuote(normalizePath(DirectoryPath, winslash = "/"),
            type = "cmd")

  Args <- DirectoryPath
  if (ForceStop) {
    Args <- c("-f", Args)
  }

  Args <- c(Flags, "darwin.stop_search", Args)

  system2(
    InterpreterPath,
    args = Args,
    stdout = TRUE,
    stderr = TRUE
  )
}
