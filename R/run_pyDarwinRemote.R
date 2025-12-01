#' Run pyDarwin on a Remote Host via SSH
#'
#' Establishes an SSH connection, prepares and uploads project files, executes
#' pyDarwin in the background, and can optionally monitor the job and download
#' results upon completion.
#'
#' @param Host Character string. The hostname or IP address of the remote server.
#' @param User Character string. The username for the SSH connection.
#' @param Password Character string. The password for SSH authentication. Defaults
#'   to `""`, which is appropriate when using key-based authentication. Using
#'   keys is strongly recommended over embedding passwords in scripts.
#' @param KeyPath Character string. The path to your private SSH key file.
#'   Defaults to the path stored in the `SSH_PRIVATE_KEY_PATH` environment variable.
#' @param SshFlags Character vector. Additional flags to pass to the underlying
#'   `ssh::ssh_connect` function.
#' @param LocalTemplatePath Character string. The path to the pyDarwin template
#'   file. If not provided, defaults to `"template.txt"` within `LocalDirectoryPath`.
#' @param LocalTokensPath Character string. The path to the pyDarwin tokens JSON
#'   file. If not provided, defaults to `"tokens.json"` within `LocalDirectoryPath`.
#' @param LocalOptionsPath Character string. The path to the pyDarwin options
#'   JSON file. If not provided, defaults to `"options.json"` within `LocalDirectoryPath`.
#' @param LocalDirectoryPath Character string or `NULL`. The path to the local
#'   project directory that contains the pyDarwin input files. Defaults to the
#'   current working directory (`.`). If `NULL` is provided, the directory
#'   containing `LocalOptionsPath` is used as the project directory.
#' @param RemoteBaseDir Character string. The base directory on the remote host
#'   under which a new project-specific directory will be created.
#' @param RemoteInterpreterPath Character string or `NULL`. The full path to the
#'   Python interpreter on the remote host (e.g., `/usr/bin/python3`). If `NULL`,
#'   the function attempts to find a suitable Python interpreter automatically.
#' @param UseLocalLicense Logical. If `TRUE`, attempts to transfer local Certara
#'   license files to the remote host.
#' @param Wait Logical. If `TRUE` (the default), the function will monitor the
#'   remote job's progress and download the results upon completion. If `FALSE`,
#    the function returns immediately after launching the job.
#' @param Flags Character vector. Command-line flags to pass to the `pyDarwin`
#'   Python module.
#' @param MonitoringInterval Numeric. The interval in seconds between status
#'   checks when monitoring a running job (`Wait = TRUE`).
#'
#' @details
#' This function automates the entire remote execution workflow. It creates a
#' unique project directory on the remote host to ensure run isolation.
#'
#' @return
#' The return value depends on the `Wait` parameter:
#' \item{If `Wait = TRUE`}{On successful completion, a list containing the
#'   parsed results, similar to `run_pyDarwin()`. This may include data frames
#'   like `$results` and character vectors like `$FinalResultFile`.}
#' \item{If `Wait = FALSE`}{An invisible list containing information needed to
#'   reconnect to the job later using `reconnect_pyDarwinJob()`. The list includes:
#'   \itemize{
#'     \item `LocalJobInfoFile`: Path to the local JSON file with job details.
#'     \item `RemoteProjectDir`: The directory on the remote host.
#'     \item `RemoteJobPID`: The Process ID of the job on the remote host.
#'     \item `Host`, `User`, `ProjectName`
#'   }}
#' The function throws an error if a critical step fails.
#'
#' @seealso [create_pyDarwinOptions()], [reconnect_pyDarwinJob()]
#'
#' @export
#' @examples
#' \dontrun{
#' # Example of launching a remote job and waiting for the results
#' remote_results <- run_pyDarwinRemote(
#'   Host = "cluster.mycompany.com",
#'   User = "myuser",
#'   KeyPath = "~/.ssh/id_rsa_cluster",
#'   LocalDirectoryPath = "path/to/my/CovariateSearchProject"
#' )
#'
#' # Example of launching a job in the background
#' job_info <- run_pyDarwinRemote(
#'   Host = "cluster.mycompany.com",
#'   User = "myuser",
#'   KeyPath = "~/.ssh/id_rsa_cluster",
#'   LocalDirectoryPath = "path/to/my/CovariateSearchProject",
#'   Wait = FALSE
#' )
#'
#' # You can later use job_info to reconnect to the job
#' # final_results <- reconnect_pyDarwinJob(JobInfo = job_info)
#' }
run_pyDarwinRemote <- function(Host,
                               User,
                               Password = "",
                               KeyPath = Sys.getenv("SSH_PRIVATE_KEY_PATH"),
                               SshFlags = character(0),
                               LocalTemplatePath,
                               LocalTokensPath,
                               LocalOptionsPath,
                               LocalDirectoryPath = ".",
                               RemoteBaseDir = "~/.rdarwin/",
                               RemoteInterpreterPath = NULL,
                               UseLocalLicense = FALSE,
                               Wait = TRUE,
                               Flags = c("-u", "-m"),
                               MonitoringInterval = 30) {
  # --- Initial Validations ---
  PathDetails <- .ValidateAndResolveLocalPaths(
    Host = Host,
    User = User,
    Password = Password,
    KeyPath = KeyPath,
    LocalDirectoryPath_arg = LocalDirectoryPath,
    LocalOptionsPath_arg   = if (missing(LocalOptionsPath))
      NULL
    else
      LocalOptionsPath,
    LocalTemplatePath_arg  = if (missing(LocalTemplatePath))
      NULL
    else
      LocalTemplatePath,
    LocalTokensPath_arg    = if (missing(LocalTokensPath))
      NULL
    else
      LocalTokensPath
  )

  # Then assign the resolved paths back to your main function's variables:
  LocalDirectoryPath <- PathDetails$LocalDirectoryPath
  LocalOptionsPath   <- PathDetails$LocalOptionsPath
  LocalTemplatePath  <- PathDetails$LocalTemplatePath
  LocalTokensPath    <- PathDetails$LocalTokensPath


  # --- Read Local Options ---
  tryCatch({
    OriginalPyDarwinOptions <- jsonlite::fromJSON(LocalOptionsPath)
  }, error = function(e) {
    stop(
      "Failed to read or parse JSON from LocalOptionsPath '",
      LocalOptionsPath,
      "': ",
      e$message
    )
  })

  ProjectName <- OriginalPyDarwinOptions$project_name
  if (is.null(ProjectName) || ProjectName == "") {
    ProjectName <-
      basename(dirname(LocalOptionsPath)) # Fallback project name
    message("project_name not found in options.json, using fallback: ",
            ProjectName)
  }

  ProjectName <-
    gsub("[^a-zA-Z0-9_.-]", "_", ProjectName) # Sanitize

  PostProcessScriptsInfo <- .prepare_postprocess_scripts(OriginalPyDarwinOptions = OriginalPyDarwinOptions,
                                                         LocalDirectoryPath = LocalDirectoryPath)

  # --- Resolve Data File and Prepare Modified Template ---
  DataFileInfo <- tryCatch({
    .prepare_DataFile(LocalTemplatePath,
                      OriginalPyDarwinOptions,
                      LocalDirectoryPath)
  }, error = function(e) {
    stop("Error resolving data file from template: ", e$message)
  })

  # --- Establish SSH Connection ---
  Session <- .establish_Connection(User, Host, KeyPath, Password)
  on.exit({
    if (!is.null(Session)) {
      ssh::ssh_disconnect(Session)
      message("SSH session disconnected.")
    }
  }, add = TRUE, after = TRUE)

  # --- Determine and Create Remote Directories ---
  RemoteProjectDir <-
    .create_RemoteDir(RemoteBaseDir, ProjectName, Session)

  # --- Modify Options.json for Remote Execution ---
  message("Preparing options for remote execution...")
  RemoteOptions <- OriginalPyDarwinOptions
  RemoteOptions$working_dir <-
    RemoteProjectDir # Absolute path on remote
  RemoteOptions$output_dir  <-
    file.path("{working_dir}", "output", fsep = "/") # Relative to the new working_dir
  RemoteOptions$data_dir <- RemoteProjectDir

  # --- Update Post-Processing Script Paths in Remote Options ---
  if (!is.null(PostProcessScriptsInfo$LocalRScriptPath)) {
    RemoteOptions$postprocess$post_run_r_code <-
      file.path("{project_dir}",
                PostProcessScriptsInfo$RScriptBaseName,
                fsep = "/")
    message(
      "Updating remote options: post_run_r_code set to '",
      RemoteOptions$postprocess$post_run_r_code,
      "'"
    )
  }
  if (!is.null(PostProcessScriptsInfo$LocalPyScriptPath)) {
    RemoteOptions$postprocess$post_run_python_code <-
      file.path("{project_dir}",
                PostProcessScriptsInfo$PyScriptBaseName,
                fsep = "/")
    message(
      "Updating remote options: post_run_python_code set to '",
      RemoteOptions$postprocess$post_run_python_code,
      "'"
    )
  }

  TempRemoteOptionsPath <- tempfile(fileext = ".json")
  write_pyDarwinOptions(
    pyDarwinOptions = RemoteOptions,
    file = TempRemoteOptionsPath,
    auto_unbox = TRUE,
    pretty = TRUE
  )


  # --- Determine Remote Python Interpreter ---
  FinalRemoteInterpreterPath <-
    .which_RemotePython(RemoteInterpreterPath, Session)

  StartJobScriptContent <- .generate_start_job_script_content(
    RemoteInterpreterPath = FinalRemoteInterpreterPath,
    # Determined earlier
    PyDarwinFlags = Flags,
    # From function arguments
    DarwinModuleName = "darwin.run_search",
    # Standard
    RemoteTemplateBaseName = "template.txt",
    RemoteTokensBaseName = "tokens.json",
    RemoteOptionsBaseName = "options.json"
  )

  # Write the script content to a temporary local file
  TempLocalStartJobScriptPath <- tempfile(fileext = "_start_job.sh")
  # to avoid Windows \r\n line endings, use "wb" mode
  TempLocalStartJobScriptPathCon <-
    file(TempLocalStartJobScriptPath, "wb")
  writeLines(StartJobScriptContent, TempLocalStartJobScriptPathCon)
  close(TempLocalStartJobScriptPathCon)
  on.exit(unlink(TempLocalStartJobScriptPath),
          add = TRUE,
          after = FALSE)

  # --- Upload Files ---
  UploadedFilesInfo <- .upload_pyDarwinFiles(
    Session = Session,
    RemoteProjectDir = RemoteProjectDir,
    LocalTemplatePath = DataFileInfo$TempLocalModifiedTemplatePath,
    LocalDataFilePath = DataFileInfo$ResolvedLocalDataPath,
    LocalTokensPath = LocalTokensPath,
    LocalOptionsPath = TempRemoteOptionsPath,
    LocalStartJobScriptPath = TempLocalStartJobScriptPath,
    LocalRScriptPath = PostProcessScriptsInfo$LocalRScriptPath,
    LocalPyScriptPath = PostProcessScriptsInfo$LocalPyScriptPath
  )

  if (UseLocalLicense) {
    LicenseTransferStatus <-
      .transfer_LicenseFiles(Session = Session,
                             ClientId = "wnl")
    if (any(grepl("failed", LicenseTransferStatus)) ||
        LicenseTransferStatus$auth_json == "not_found_local" ||
        LicenseTransferStatus$license_jwt == "not_found_local") {
      warning(
        "There were issues transferring license files. ",
        "pyDarwin may fail if licenses are not correctly set up on the remote.",
        "set `verbose = TRUE` to inspect the transfer status.",
        call. = FALSE
      )
    }
  }

  JobLaunchCommandArgs <- c(
    FinalRemoteInterpreterPath,
    Flags,
    "darwin.run_search",
    basename(UploadedFilesInfo$RemoteTemplate),
    basename(UploadedFilesInfo$RemoteTokens),
    basename(UploadedFilesInfo$RemoteOptions)
  )

  FullRemoteCommandToLaunchJob <- paste(
    UploadedFilesInfo$RemoteStartJobScriptPath,
    paste(JobLaunchCommandArgs, collapse = " ")
  )

  message(
    "Launching pyDarwin job in background on remote host via: ",
    FullRemoteCommandToLaunchJob
  )

  # Execute start_job.sh. This script itself handles nohup and PID.
  # We need its stdout to get the "PID launched" message, and its exit status.
  # ssh_exec_internal might be better here to get stdout/stderr/status of start_job.sh
  StartJobResult <-
    ssh::ssh_exec_internal(Session, command = FullRemoteCommandToLaunchJob, error = FALSE)
  RemoteJobPID <- NA # Initialize

  # exit if status is not OK
  if (StartJobResult$status != 0) {
    message("start_job.sh failed on remote. Exit code: ",
            StartJobResult$status)
    if (length(StartJobResult$stdout) > 0) {
      message("Stdout from start_job.sh:\n",
              rawToChar(StartJobResult$stdout))
    }

    if (length(StartJobResult$stderr) > 0) {
      message("Stderr from start_job.sh:\n",
              rawToChar(StartJobResult$stderr))
    }

    # Attempt to download pyDarwin_stdout.log and pyDarwin_stderr.log for more clues
    .print_RemoteLog(
      Session,
      file.path(RemoteProjectDir, "pydarwin_stdout.log", fsep = "/"),
      "pyDarwin Launch STDOUT"
    )
    .print_RemoteLog(
      Session,
      file.path(RemoteProjectDir, "pydarwin_stderr.log", fsep = "/"),
      "pyDarwin Launch STDERR"
    )
    stop(
      "Failed to launch pyDarwin job via start_job.sh. Check remote logs in ",
      RemoteProjectDir
    )
  }

  message("start_job.sh executed successfully on remote.")
  # Try to parse PID from stdout of start_job.sh
  StdoutOfStartJob <- rawToChar(StartJobResult$stdout)
  if (getOption("verbose", default = FALSE)) {
    message("Stdout from start_job.sh:\n", StdoutOfStartJob)
  }

  # Example: "pyDarwin background process launched successfully with PID 12345."
  pid_match <-
    regexpr("PID\\s+(\\d+)", StdoutOfStartJob, perl = TRUE)
  if (pid_match == -1) {
    stop("Could not parse PID from start_job.sh output, though script reported success.")
  }

  RemoteJobPID <-
    sub(".*PID\\s+(\\d+).*", "\\1", StdoutOfStartJob)
  message("Remote pyDarwin process believed to be started with PID: ",
          RemoteJobPID)

  if (is.na(RemoteJobPID) || !nzchar(RemoteJobPID)) {
    # This handles the case where start_job.sh reported success (status 0)
    # but we couldn't parse the PID from its stdout.
    stop("Cannot monitor remote job as PID was not determined.")
  }

  # --- Define Local Job Info File Path ---
  # This file will store essential info for reconnecting or managing the job.
  LocalJobInfoFileName <-
    paste0(ProjectName, "_remote_job_info.json")
  LocalJobInfoFilePath <-
    file.path(LocalDirectoryPath, LocalJobInfoFileName) # LocalDirectoryPath is validated earlier

  if (!Wait) {
    Status <-  "Launched"
  } else {
    Status <-    "LaunchedInBackground"
  }

  if (!is.na(RemoteJobPID) &&
      nzchar(RemoteJobPID)) {
    RemoteJobPID <-    RemoteJobPID
  } else {
    RemoteJobPID <- NULL
  }

  JobInfoForSave <- list(
    Host = Host,
    User = User,
    RemoteProjectDir = RemoteProjectDir,
    RemoteJobPID = RemoteJobPID,
    ProjectName = ProjectName,
    LaunchTime = as.character(Sys.time()),
    Status = Status,
    RemoteStdOutLog = file.path(RemoteProjectDir, "pydarwin_stdout.log", fsep = "/"),
    RemoteStdErrLog = file.path(RemoteProjectDir, "pydarwin_stderr.log", fsep = "/"),
    RemoteMessagesLog = file.path(RemoteProjectDir, "messages.txt", fsep = "/"),
    RemoteJobInfoFileOnRemote = file.path(RemoteProjectDir, "pydarwin_job.info", fsep = "/")
  )

  tryCatch({
    jsonlite::write_json(
      JobInfoForSave,
      LocalJobInfoFilePath,
      auto_unbox = TRUE,
      pretty = TRUE
    )
    message("Job information saved locally to: ", LocalJobInfoFilePath)
    message(
      "Use this file or the returned information to reconnect and check status/results later."
    )
  }, error = function(e) {
    warning("Failed to save local job info file '",
            LocalJobInfoFilePath,
            "': ",
            e$message)
    message("pyDarwin launched in background. Manual tracking of details required.")
  })

  # --- Handle Wait = FALSE ---
  if (!Wait) {
    message("pyDarwin launched in background on remote host.")
    message("Remote Project Directory: ", RemoteProjectDir)
    message("Remote Job PID: ", RemoteJobPID)

    # Return a list of useful information for the user/reconnect function
    return(invisible(
      list(
        LocalJobInfoFile = LocalJobInfoFilePath,
        # Primary identifier for user
        RemoteProjectDir = RemoteProjectDir,
        RemoteJobPID = RemoteJobPID,
        Host = Host,
        User = User,
        ProjectName = ProjectName
      )
    ))
  }

  message(
    "pyDarwin job (PID: ",
    RemoteJobPID,
    ") launched successfully on remote. Proceeding to monitor."
  )

  MonitoringResult <- .monitor_pyDarwinJob(
    Session = Session,
    RemoteJobPID = RemoteJobPID,
    RemoteProjectDir = RemoteProjectDir,
    MonitoringInterval = MonitoringInterval,
    InitialMessagesByteOffset = 0
  )

  # --- Post-monitoring processing (after job finished or monitoring stopped) ---
  # At this point, the SSH 'Session' is still active.

  LocalResultsDownloadBase <- LocalDirectoryPath

  DownloadedResultInfo <- .download_pyDarwinResults(
    Session = Session,
    RemoteProjectDir = RemoteProjectDir,
    OriginalPyDarwinOptions = OriginalPyDarwinOptions,
    LocalBaseDownloadDir = LocalResultsDownloadBase,
    ProjectName = NULL # Download directly
  )

  # --- Attempt to parse results ---
  ProcessedResults <- .process_pyDarwinResults(
    DownloadedResultInfo = DownloadedResultInfo,
    OriginalPyDarwinOptions = OriginalPyDarwinOptions,
    LocalResultsDownloadBase = LocalResultsDownloadBase,
    Session = Session,
    RemoteProjectDir = RemoteProjectDir,
    MonitoringResult = MonitoringResult,
    RemoteJobPID = RemoteJobPID
  )

  # --- Final Decision and Return Value ---
  if (is.character(ProcessedResults) &&
      length(ProcessedResults) > 0) {
    # This means .process_downloaded_pyDarwinResults returned messages.txt content as a fallback.
    # This implies primary result files were not found/parsed successfully.
    # The warnings about missing files would have been issued by .process_downloaded_pyDarwinResults.
    message(
      "Primary result files were not successfully parsed. Returning content of downloaded messages.txt."
    )
    return(ProcessedResults)
  } else if (is.list(ProcessedResults)) {
    # This is the structured list (ReturnedList from the helper)
    # Check if it contains meaningful data (results, FinalResultFile, FinalControlFile)
    if (nrow(ProcessedResults$results) == 0 &&
        length(ProcessedResults$FinalResultFile) == 0 &&
        length(ProcessedResults$FinalControlFile) == 0) {
      # Even if structured list is returned, it might be empty of primary data.
      # .process_downloaded_pyDarwinResults would have warned and possibly printed remote logs.
      warning(
        "No structured pyDarwin results (results.csv, FinalResultFile, etc.) were found or parsed from downloaded files.",
        call. = FALSE
      )
      message("Returning basic information about download attempts and locations.")
      return(ProcessedResults) # Contains DownloadedResultsDir and DownloadedItems
    } else {
      # Successfully parsed some structured results
      message("Successfully processed downloaded pyDarwin results.")
      return(ProcessedResults)
    }
  } else {
    # Should not happen if .process_downloaded_pyDarwinResults behaves as expected
    warning("Unexpected format returned from result processing. Returning empty list.",
            call. = FALSE)
    return(list())
  }
}

#' Process Downloaded pyDarwin Results
#'
#' This function takes the information about downloaded pyDarwin results and
#' attempts to parse standard output files (results.csv, FinalResultFile, FinalControlFile).
#' It handles cases where files might be missing and provides appropriate warnings
#' or fallbacks.
#'
#' @param DownloadedResultInfo List: The output from `.download_pyDarwinResults`,
#'        containing local paths to downloaded items (e.g., `LocalOutputDirPath`,
#'        `LocalMessagesPath`) and a list of `SuccessDownloads`.
#' @param OriginalPyDarwinOptions List: The parsed content of the original
#'        options.json file, used to determine engine_adapter for result file names.
#' @param LocalResultsDownloadBase Character string: The base local directory where results
#'        were downloaded. Used for constructing messages.
#' @param Session An active SSH session object (optional). Only required if fallback
#'        diagnostic log printing is needed when no results are found.
#' @param RemoteProjectDir Character string (optional). Only required if Session is provided,
#'        for locating remote diagnostic logs.
#' @param MonitoringResult List (optional). Only required if Session is provided,
#'        for constructing more informative messages in case of no results. Contains
#'        `Status` and `ProcessValue`.
#' @param RemoteJobPID Character or numeric (optional). Only required if Session is provided,
#'        for constructing more informative messages in case of no results.
#'
#' @return A list containing parsed results (`results`, `FinalResultFile`, `FinalControlFile`),
#'         or the content of `messages.txt` as a character vector if primary result
#'         files are not found. Includes `DownloadedResultsDir` and `DownloadedItems`.
#'         If all attempts fail, returns a list that includes `DownloadedResultsDir`
#'         and `DownloadedItems` but empty primary results.
#'
#' @noRd
.process_pyDarwinResults <- function(DownloadedResultInfo,
                                     OriginalPyDarwinOptions,
                                     LocalResultsDownloadBase,
                                     Session = NULL,
                                     RemoteProjectDir = NULL,
                                     MonitoringResult = NULL,
                                     RemoteJobPID = NULL) {
  message("Processing downloaded results from: ",
          LocalResultsDownloadBase)

  ReturnedList <- list(
    results = data.frame(),
    FinalResultFile = character(),
    FinalControlFile = character(),
    DownloadedResultsDir = LocalResultsDownloadBase,
    DownloadedItems = DownloadedResultInfo$SuccessDownloads
  )
  NotFoundFiles <- c()

  LocalActualOutputDir <- DownloadedResultInfo$LocalOutputDirPath

  # Check if the LocalActualOutputDir itself exists (it should if 'output_dir' was in SuccessDownloads)
  if (!dir.exists(LocalActualOutputDir)) {
    warning(
      "Downloaded 'output' directory ('",
      LocalActualOutputDir,
      "') not found locally. Cannot parse result files.",
      call. = FALSE
    )
    # Even if output_dir is missing, messages.txt might have been downloaded.
    # Proceed to the final check for messages.txt.
  } else {
    resultsPath <- file.path(LocalActualOutputDir, "results.csv")
    if (file.exists(resultsPath)) {
      tryCatch({
        ReturnedList$results <- utils::read.csv(resultsPath)
      }, error = function(e) {
        warning("Failed to read results file '",
                resultsPath,
                "': ",
                e$message,
                call. = FALSE)
        NotFoundFiles <- c(NotFoundFiles, basename(resultsPath))
      })
    } else {
      NotFoundFiles <-
        c(NotFoundFiles, "results.csv") # Report as missing relative to output dir
    }

    # Check the algorithm from the original options.
    algorithm <- OriginalPyDarwinOptions$algorithm

    # Only look for FinalResultFile and FinalControlFile if the algorithm is not MOGA or MOGA3.
    if (is.null(algorithm) || !algorithm %in% c("MOGA", "MOGA3")) {
      EngineAdapter <- OriginalPyDarwinOptions$engine_adapter
      if (is.null(EngineAdapter))
        EngineAdapter <- "nlme"

      if (EngineAdapter == "nlme") {
        FinalResultFileName <- "FinalResultFile.txt"
        FinalControlFileName <- "FinalControlFile.mmdl"
      } else {
        # Assuming nonmem
        FinalResultFileName <- "FinalResultFile.lst"
        FinalControlFileName <- "FinalControlFile.mod"
      }
      FinalResultFilePath <-
        file.path(LocalActualOutputDir, FinalResultFileName)
      FinalControlFilePath <-
        file.path(LocalActualOutputDir, FinalControlFileName)

      if (file.exists(FinalResultFilePath)) {
        ReturnedList$FinalResultFile <-
          readLines(FinalResultFilePath, warn = FALSE)
      } else {
        NotFoundFiles <- c(NotFoundFiles, FinalResultFileName)
      }

      if (file.exists(FinalControlFilePath)) {
        ReturnedList$FinalControlFile <-
          readLines(FinalControlFilePath, warn = FALSE)
      } else {
        NotFoundFiles <- c(NotFoundFiles, FinalControlFileName)
      }
    }
  } # End if(dir.exists(LocalActualOutputDir))

  if (length(NotFoundFiles) > 0) {
    warning(
      "The following expected result files were not found or readable in the downloaded 'output' directory ('",
      LocalActualOutputDir,
      "'):\n",
      paste(NotFoundFiles, collapse = "\n"),
      call. = FALSE
    )
  }

  # Determine final return value
  if (nrow(ReturnedList$results) == 0 &&
      length(ReturnedList$FinalResultFile) == 0 &&
      length(ReturnedList$FinalControlFile) == 0) {
    LocalMessagesPathFromDownload <-
      DownloadedResultInfo$LocalMessagesPath
    if (file.exists(LocalMessagesPathFromDownload) &&
        file.size(LocalMessagesPathFromDownload) > 0) {
      warning(
        "Standard output result files (results.csv, etc.) not found or empty in downloaded 'output' directory ('",
        LocalActualOutputDir,
        "').\n",
        "Returning the content of the downloaded main log file: ",
        LocalMessagesPathFromDownload,
        call. = FALSE
      )
      return(readLines(LocalMessagesPathFromDownload, warn = FALSE))
    } else {
      warning(
        "Standard output result files not found/empty in '",
        LocalActualOutputDir,
        "' and downloaded main log file '",
        LocalMessagesPathFromDownload,
        "' also not found or empty.",
        call. = FALSE
      )

      # The diagnostic printing part requires Session and other info
      if (!is.null(Session) &&
          !is.null(RemoteProjectDir) &&
          !is.null(MonitoringResult) && !is.null(RemoteJobPID)) {
        RemotePyDarwinStdOutLog <-
          file.path(RemoteProjectDir, "pydarwin_stdout.log", fsep = "/")
        RemotePyDarwinStdErrLog <-
          file.path(RemoteProjectDir, "pydarwin_stderr.log", fsep = "/")
        RemoteJobInfoFileFromScript <-
          file.path(RemoteProjectDir, "pydarwin_job.info", fsep = "/")

        message(
          "Attempting to print additional diagnostic logs from remote (as results parsing failed)..."
        )
        # Ensure .print_RemoteLog is available in the environment where this helper is defined/called
        try(.print_RemoteLog(
          Session,
          RemotePyDarwinStdOutLog,
          "pyDarwin Process STDOUT (pydarwin_stdout.log)"
        ),
        silent = TRUE)
        try(.print_RemoteLog(
          Session,
          RemotePyDarwinStdErrLog,
          "pyDarwin Process STDERR (pydarwin_stderr.log)"
        ),
        silent = TRUE)
        try(.print_RemoteLog(Session,
                             RemoteJobInfoFileFromScript,
                             "Job Info Log (pydarwin_job.info)"),
            silent = TRUE)

        # The SpecificStopMessage would typically be generated by the calling function
      }

      return(ReturnedList) # Return the list with DownloadedResultsDir, even if empty of primary results
    }
  } else {
    return(ReturnedList) # Return results found
  }
}

# Helper function to download and print a remote log file
.print_RemoteLog <- function(Session, RemoteLogPath, LogName) {
  TempDir <- tempdir()
  Success <- FALSE
  tryCatch({
    LocalLogFilePath <- file.path(TempDir, basename(RemoteLogPath))
    if (file.exists(LocalLogFilePath)) {
      unlink(LocalLogFilePath, force = TRUE)
    }

    ssh::scp_download(
      Session,
      RemoteLogPath,
      to = TempDir,
      verbose = getOption("verbose", default = FALSE)
    )
    LogContent <- readLines(LocalLogFilePath, warn = FALSE)
    if (length(LogContent) > 0) {
      message("--- Content of Remote ",
              LogName,
              " (",
              RemoteLogPath,
              ") ---")
      cat(paste(LogContent, collapse = "\n"), "\n")
    }

    Success <- TRUE
    message("--- End of Remote ", LogName, " ---")
  }, error = function(e) {
    # message("Error downloading or reading remote ", LogName, " '", RemoteLogPath, "': ", e$message)
  })

  return(invisible(Success))
}

.prepare_DataFile <-
  function(LocalTemplatePath,
           OriginalPyDarwinOptions,
           LocalDirectoryPath) {
    template_content <- readLines(LocalTemplatePath, warn = FALSE)
    data_line_pattern <- "^##DATA\\s+(.*)$" # Using ##DATA prefix
    data_line_index <-
      grep(data_line_pattern, template_content, ignore.case = TRUE)

    if (length(data_line_index) == 0) {
      stop("No '##DATA data_file_name' line found in the template file: ",
           LocalTemplatePath)
    }
    if (length(data_line_index) > 1) {
      warning("Multiple '##DATA' lines found in template. Using the first one: ",
              template_content[data_line_index[1]])
      data_line_index <- data_line_index[1]
    }

    # Extract the raw data file entry (e.g., "{data_dir}/file.csv" or "file.csv")
    raw_data_file_entry <-
      sub(data_line_pattern, "\\1", template_content[data_line_index], ignore.case = TRUE)
    raw_data_file_entry <- trimws(raw_data_file_entry)

    if (raw_data_file_entry == "") {
      stop("Data file name in '##DATA' line is empty in template: ",
           LocalTemplatePath)
    }

    # Resolve {data_dir}
    data_dir_alias <- "{data_dir}"
    DataDirActualPath <-
      LocalDirectoryPath # Default if data_dir not in options or alias not used

    if (!is.null(OriginalPyDarwinOptions$data_dir) &&
        OriginalPyDarwinOptions$data_dir != "") {
      DataDirActualPath <-
        normalizePath(OriginalPyDarwinOptions$data_dir, mustWork = FALSE)
    } else {
      # data_dir not in options, default to LocalDirectoryPath
      DataDirActualPath <-
        normalizePath(LocalDirectoryPath, mustWork = TRUE)
    }


    ResolvedDataFilePath <- raw_data_file_entry
    if (grepl(data_dir_alias, raw_data_file_entry, fixed = TRUE)) {
      if (!dir.exists(DataDirActualPath)) {
        stop(
          "The 'data_dir' specified or defaulted to ('",
          DataDirActualPath,
          "') does not exist."
        )
      }
      ResolvedDataFilePath <-
        sub(data_dir_alias,
            DataDirActualPath,
            ResolvedDataFilePath,
            fixed = TRUE)
    }

    ResolvedDataFilePath <-
      normalizePath(ResolvedDataFilePath,
                    mustWork = FALSE,
                    winslash = "/")

    if (!file.exists(ResolvedDataFilePath)) {
      stop(
        "Resolved data file does not exist: '",
        ResolvedDataFilePath,
        "'. Derived from template entry: '",
        raw_data_file_entry,
        "', with data_dir resolved to: '",
        DataDirActualPath,
        "'."
      )
    }

    DataFileBaseName <- basename(ResolvedDataFilePath)

    # Create a temporary modified template file
    TempLocalModifiedTemplatePath <-
      tempfile(fileext = "_template.txt")
    modified_template_content <- template_content
    modified_template_content[data_line_index] <-
      paste0("##DATA {data_dir}/", DataFileBaseName)
    writeLines(modified_template_content, TempLocalModifiedTemplatePath)

    message("Data file resolved to: ", ResolvedDataFilePath)
    message(
      "Modified template for remote use (data file '",
      DataFileBaseName,
      "') will be: ",
      TempLocalModifiedTemplatePath
    )

    return(
      list(
        ResolvedLocalDataPath = ResolvedDataFilePath,
        DataFileBaseName = DataFileBaseName,
        TempLocalModifiedTemplatePath = TempLocalModifiedTemplatePath
      )
    )
  }

.subst_Tilda <- function(RemoteBaseDir, Session) {
  CmdHOME <- "echo $HOME"
  HOME_Out_Str <- tryCatch({
    HOME_Out_Raw <-
      ssh::ssh_exec_internal(Session, command = CmdHOME, error = FALSE)
    HOME_Out_Str <- trimws(rawToChar(HOME_Out_Raw$stdout))

    Exit_Status_HOME <- HOME_Out_Raw$status

    if (Exit_Status_HOME != 0 ||
        HOME_Out_Str == "" ||
        length(HOME_Out_Raw$stderr) > 0) {
      stop(
        "Cannot find $HOME on remote host using 'echo $HOME' (exit status: ",
        Exit_Status_HOME,
        ", stderr: ",
        rawToChar(HOME_Out_Raw$stderr),
        "). Please expand `~/` to the full path."
      )
    }

    HOME_Out_Str
  }, error = function(e) {
    stop(
      "Error trying to find $HOME on remote host: ",
      e$message,
      ". Please expand `~/` to the full path."
    )
  })

  if (is.null(RemoteBaseDir) || RemoteBaseDir == "") {
    stop(
      "$HOME not found on remote host (path was empty after check). Please expand `~/` to the full path."
    )
  }

  RemoteBaseDir <- gsub("^~", HOME_Out_Str, RemoteBaseDir)
}

# --- Determine Remote Python Interpreter ---
.which_RemotePython <- function(RemoteInterpreterPath, Session) {
  FinalRemoteInterpreterPath <- RemoteInterpreterPath
  if (is.null(FinalRemoteInterpreterPath) ||
      FinalRemoteInterpreterPath == "") {
    message(
      "RemoteInterpreterPath not specified, attempting to find 'python3' on remote host..."
    )
    CmdWhich <- "which python3"
    FinalRemoteInterpreterPath <- tryCatch({
      path_out_raw <-
        ssh::ssh_exec_internal(Session, command = CmdWhich, error = FALSE)
      path_out_str <- trimws(rawToChar(path_out_raw$stdout))

      exit_status_which <- path_out_raw$status

      if (exit_status_which != 0 ||
          path_out_str == "" ||
          grepl("not found", path_out_str, ignore.case = TRUE) ||
          grepl("no python3 in", path_out_str, ignore.case = TRUE) ||
          length(path_out_raw$stderr) > 0) {
        stop(
          "python3 not found on remote host using 'which python3' (exit status: ",
          exit_status_which,
          ", stderr: ",
          rawToChar(path_out_raw$stderr),
          "). Please specify RemoteInterpreterPath."
        )
      }

      path_out_str
    }, error = function(e) {
      stop(
        "Error trying to find python3 on remote host: ",
        e$message,
        ". Please specify RemoteInterpreterPath."
      )
    })

    if (is.null(FinalRemoteInterpreterPath) ||
        FinalRemoteInterpreterPath == "") {
      stop(
        "python3 not found on remote host (path was empty after check). Please specify RemoteInterpreterPath."
      )
    }

    message("Using remote interpreter: ", FinalRemoteInterpreterPath)
  }

  FinalRemoteInterpreterPath
}

#' Generate the content for the start_job.sh script
#'
#' This script will be run on the remote host to launch pyDarwin in the background.
#'
#' @param RemoteInterpreterPath Character string: Absolute path to the python interpreter on the remote.
#' @param PyDarwinFlags Character vector: Flags for pyDarwin (e.g., c("-u", "-m")).
#' @param DarwinModuleName Character string: The module to run (e.g., "darwin.run_search").
#' @param RemoteTemplateBaseName Character string: Basename of the template file on remote.
#' @param RemoteTokensBaseName Character string: Basename of the tokens file on remote.
#' @param RemoteOptionsBaseName Character string: Basename of the options file on remote.
#' @return A character string containing the shell script.
#' @noRd
.generate_start_job_script_content <-
  function(RemoteInterpreterPath,
           PyDarwinFlags,
           DarwinModuleName,
           RemoteTemplateBaseName,
           RemoteTokensBaseName,
           RemoteOptionsBaseName) {
    script_content <- sprintf(
      '\t#!/bin/bash
  # This script is generated by RDarwin to launch pyDarwin in the background.

  # Determine the script\'s own absolute directory
  SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

  # Change current working directory to the script\'s directory
  cd "$SCRIPT_DIR" || { echo "Failed to cd to $SCRIPT_DIR. Exiting." >&2; exit 1; }

  # Now that we\'ve successfully cd\'d, PROJECT_DIR_PATH can reliably be "."
  PROJECT_DIR_PATH="."

  # Define file paths relative to the (now certain) current working directory
  PID_FILE="${PROJECT_DIR_PATH}/pydarwin.pid"
  PYDARWIN_STDOUT_LOG="${PROJECT_DIR_PATH}/pydarwin_stdout.log"
  PYDARWIN_STDERR_LOG="${PROJECT_DIR_PATH}/pydarwin_stderr.log"
  JOB_INFO_FILE="${PROJECT_DIR_PATH}/pydarwin_job.info"

  # Arguments to this script
  ARGS_TO_SCRIPT=("$@")
  REMOTE_INTERPRETER="${ARGS_TO_SCRIPT[0]}"
  PYDARWIN_ARGS=("${ARGS_TO_SCRIPT[@]:1}")

  # --- OS Detection and Conditional Environment Variable Setting ---
  OS_DETECTION_MSG=""
  PML_BIN_DIR_VALUE="" # Store the value if set

  if [ -f /etc/os-release ]; then
      # Source /etc/os-release to get variables like ID, VERSION_ID
      . /etc/os-release
      if [ "$ID" = "ubuntu" ]; then
          # You can also check $VERSION_ID here if specific Ubuntu versions need different values
          # For example: if [ "$ID" = "ubuntu" ] && [ "$VERSION_ID" = "22.04" ]; then
          PML_BIN_DIR="UBUNTU"
          export PML_BIN_DIR
          PML_BIN_DIR_VALUE=$PML_BIN_DIR # For logging
          OS_DETECTION_MSG="Detected Ubuntu ($NAME $VERSION_ID). Set PML_BIN_DIR=${PML_BIN_DIR}."
      else
          OS_DETECTION_MSG="OS is $NAME ($ID $VERSION_ID), not Ubuntu. PML_BIN_DIR not set by this script."
      fi
  else
      OS_DETECTION_MSG="/etc/os-release not found. Cannot determine OS for PML_BIN_DIR specific setting."
  fi
  # --- End OS Detection ---

  echo "Starting pyDarwin job at $(date) by RDarwin" > "${JOB_INFO_FILE}"
  echo "Project Directory: $(pwd)" >> "${JOB_INFO_FILE}"
  echo "Script Arguments: ${ARGS_TO_SCRIPT[*]}" >> "${JOB_INFO_FILE}" # Log all received args
  echo "Interpreter: ${REMOTE_INTERPRETER}" >> "${JOB_INFO_FILE}"
  echo "pyDarwin Arguments: ${PYDARWIN_ARGS[*]}" >> "${JOB_INFO_FILE}"
  echo "PID File: ${PID_FILE}" >> "${JOB_INFO_FILE}"
  echo "pyDarwin Stdout Log: ${PYDARWIN_STDOUT_LOG}" >> "${JOB_INFO_FILE}"
  echo "pyDarwin Stderr Log: ${PYDARWIN_STDERR_LOG}" >> "${JOB_INFO_FILE}"
  echo "OS Detection: ${OS_DETECTION_MSG}" >> "${JOB_INFO_FILE}"
  if [ -n "$PML_BIN_DIR_VALUE" ]; then # If PML_BIN_DIR_VALUE is not empty
      echo "Effective PML_BIN_DIR for pyDarwin process: $PML_BIN_DIR_VALUE" >> "${JOB_INFO_FILE}"
  fi

  # Execute pyDarwin with nohup.
  # Environment variables (like PML_BIN_DIR if exported) will be inherited.
  nohup "${REMOTE_INTERPRETER}" "${PYDARWIN_ARGS[@]}" > "${PYDARWIN_STDOUT_LOG}" 2> "${PYDARWIN_STDERR_LOG}" &
  PROCESS_PID=$!

  echo "${PROCESS_PID}" > "${PID_FILE}"
  sleep 2

  if ps -p "${PROCESS_PID}" > /dev/null; then
    echo "pyDarwin background process launched successfully with PID ${PROCESS_PID}." >> "${JOB_INFO_FILE}"
    echo "pyDarwin background process launched successfully with PID ${PROCESS_PID}."
    exit 0
  else
    echo "Error: pyDarwin background process (PID ${PROCESS_PID}) not found shortly after launch." >> "${JOB_INFO_FILE}"
    echo "It may have failed to start or exited prematurely." >> "${JOB_INFO_FILE}"
    echo "Check ${PYDARWIN_STDOUT_LOG} and ${PYDARWIN_STDERR_LOG} in $(pwd) for details." >> "${JOB_INFO_FILE}"
    echo "Error: pyDarwin background process (PID ${PROCESS_PID}) not found. Check remote logs." >&2
    exit 1
  fi
'
    )

    script_content
  }

#' Uploads necessary files to the remote host for pyDarwin execution.
#'
#' This includes the template, data, tokens, options, postprocessing and the
#' launcher script.
#' @noRd
.upload_pyDarwinFiles <- function(Session,
                                  RemoteProjectDir,
                                  LocalTemplatePath,
                                  LocalDataFilePath,
                                  LocalTokensPath,
                                  LocalOptionsPath,
                                  LocalStartJobScriptPath,
                                  LocalRScriptPath = NULL,
                                  LocalPyScriptPath = NULL) {
  message("Preparing to upload files to remote host at: ", RemoteProjectDir)

  # Define a list of files to upload. Each element is a list with:
  #   local_path: path to the local file
  #   remote_name: desired basename for the file on the remote server
  #   description: for logging messages
  #   file_type: a CamelCase identifier for this file type (used for naming in return list)
  files_to_upload <- list(
    list(
      local_path = LocalTemplatePath,
      remote_name = "template.txt",
      description = "template file",
      file_type = "TemplateFile"
    ),
    list(
      local_path = LocalDataFilePath,
      remote_name = basename(LocalDataFilePath),
      description = "data file",
      file_type = "DataFile"
    ),
    list(
      local_path = LocalTokensPath,
      remote_name = "tokens.json",
      description = "tokens file",
      file_type = "TokensFile"
    ),
    list(
      local_path = LocalOptionsPath,
      remote_name = "options.json",
      description = "remote options file",
      file_type = "OptionsFile"
    ),
    list(
      local_path = LocalStartJobScriptPath,
      remote_name = "start_job.sh",
      description = "job launcher script",
      file_type = "StartJobScript"
    )
  )

  if (!is.null(LocalRScriptPath)) {
    files_to_upload <- append(files_to_upload, list(
      list(
        local_path  = LocalRScriptPath,
        remote_name = basename(LocalRScriptPath),
        description = "post-processing R script",
        file_type   = "PostProcessRScript"
      )
    ))
  }

  # Conditionally add the post-processing Python script to the list of files to upload
  if (!is.null(LocalPyScriptPath)) {
    files_to_upload <- append(files_to_upload, list(
      list(
        local_path  = LocalPyScriptPath,
        remote_name = basename(LocalPyScriptPath),
        description = "post-processing Python script",
        file_type   = "PostProcessPyScript"
      )
    ))
  }

  uploaded_remote_paths <- list()

  for (file_info in files_to_upload) {
    remote_file_path <-
      file.path(RemoteProjectDir, file_info$remote_name, fsep = "/")
    message(
      "Uploading ",
      file_info$description,
      ": '",
      file_info$local_path,
      "' to '",
      remote_file_path,
      "'"
    )

    tryCatch({
      ssh::scp_upload(Session, files = file_info$local_path, to = remote_file_path)
    }, error = function(e) {
      stop(
        "Failed to upload ",
        file_info$description,
        " '",
        file_info$local_path,
        "' to remote: ",
        e$message
      )
    })

    list_name <- paste0("Remote", file_info$file_type, "Path")
    uploaded_remote_paths[[list_name]] <- remote_file_path
  }

  message("All specified files uploaded successfully.")

  # Make the start_job.sh script executable
  remote_start_script_path_name <-
    "RemoteStartJobScriptPath" # The name we just used
  if (remote_start_script_path_name %in% names(uploaded_remote_paths)) {
    remote_start_script <-
      uploaded_remote_paths[[remote_start_script_path_name]]
    message("Making job launcher script executable on remote: ",
            remote_start_script)
    chmod_command <-
      paste("chmod +x", shQuote(remote_start_script, type = "sh"))
    ExecResult <-
      ssh::ssh_exec_wait(Session, command = chmod_command) # Simpler for chmod
    if (ExecResult != 0) {
      stop(
        "Failed to make '",
        remote_start_script,
        "' executable on remote host. Exit code: ",
        ExecResult
      )
    }
    message("Job launcher script is now executable.")
  } else {
    warning("Remote path for start_job.sh was not set after upload. This should not happen.")
  }

  return(uploaded_remote_paths)
}

.create_RemoteDir <- function(RemoteBaseDir, ProjectName, Session) {
  # substitute tilda
  if (grepl("^~/", RemoteBaseDir)) {
    RemoteBaseDir <- .subst_Tilda(RemoteBaseDir, Session)
  }

  RemoteProjectDir <-
    file.path(RemoteBaseDir, ProjectName, fsep = "/")

  message("Setting up remote project directory: ", RemoteProjectDir)
  if (grepl(" ", RemoteProjectDir, fixed = TRUE)) {
    RemoteProjectDirSplitted <- unlist(strsplit(RemoteProjectDir, "/"))
    RemoteProjectDirSplitted <-
      sapply(RemoteProjectDirSplitted, function(x) {
        if (grepl(" ", x, fixed = TRUE)) {
          shQuote(x, type = "sh")
        } else {
          x
        }
      })

    RemoteProjectDir <-
      paste(RemoteProjectDirSplitted, collapse = "/")
  }

  CmdMkdir <- paste("mkdir -p", RemoteProjectDir)
  ExecResult <-
    ssh::ssh_exec_internal(Session, command = CmdMkdir, error = FALSE)

  if (ExecResult$status != 0) {
    if (length(ExecResult$stderr) > 0) {
      StdErrContent <- rawToChar(ExecResult$stderr)
    }

    stop(
      "Failed to create remote directory '",
      RemoteProjectDir,
      "'. Exit code: ",
      ExecResult$status,
      ". Stderr: ",
      StdErrContent
    )
  }

  RemoteProjectDir
}

.ValidateAndResolveLocalPaths <- function(Host,
                                          User,
                                          Password,
                                          KeyPath,
                                          LocalDirectoryPath_arg = NULL,
                                          LocalOptionsPath_arg,
                                          LocalTemplatePath_arg,
                                          LocalTokensPath_arg) {
  # --- Initial Validations (SSH Credentials) ---
  # These checks ensure that Host and User are provided, and at least one of Password or KeyPath is valid.
  if (missing(Host) || !is.character(Host) || Host == "") {
    stop("Host must be specified.")
  }
  if (missing(User) || !is.character(User) || User == "") {
    stop("User must be specified.")
  }

  # Check for presence of either Password or KeyPath
  has_password <-
    !is.null(Password) &&
    nzchar(Password)
  has_keypath <-
    !is.null(KeyPath) &&
    nzchar(KeyPath) &&
    file.exists(KeyPath) # Also check if keypath exists if provided

  if (!has_password && !has_keypath) {
    if (!is.null(KeyPath) && nzchar(KeyPath) && !file.exists(KeyPath)) {
      stop(
        "KeyPath '",
        KeyPath,
        "' was provided but does not exist. Either a valid Password or a valid KeyPath must be provided."
      )
    }
    stop("Either Password or KeyPath must be provided for SSH connection.")
  }

  # --- Resolve Paths ---
  # Use temporary variables to mirror the snippet's logic including potential shadowing.
  # These will hold the arguments as they are, or be updated by defaulting logic.

  # 1. Determine the definitive LocalDirectoryPath
  #    The logic from the snippet is:
  #    - If LocalDirectoryPath_arg is NULL, derive it from LocalOptionsPath_arg.
  #    - Otherwise, use LocalDirectoryPath_arg.
  resolvedLocalDirectoryPath <- LocalDirectoryPath_arg

  if (is.null(resolvedLocalDirectoryPath)) {
    # If LocalDirectoryPath_arg is NULL, LocalOptionsPath_arg is used to determine it.
    # This implies LocalOptionsPath_arg must be a character string (path or filename)
    # if LocalDirectoryPath_arg is NULL.
    if (missing(LocalOptionsPath_arg) ||
        is.null(LocalOptionsPath_arg)) {
      stop(
        "If LocalDirectoryPath is NULL, LocalOptionsPath must be provided to derive LocalDirectoryPath."
      )
    }
    # dirname("somefile.json") gives "."
    # dirname("/path/to/somefile.json") gives "/path/to"
    resolvedLocalDirectoryPath <- dirname(LocalOptionsPath_arg)
  }
  # Normalize the LocalDirectoryPath. If it was ".", normalizePath turns it into getwd().
  # mustWork = TRUE ensures the directory exists.
  resolvedLocalDirectoryPath <-
    normalizePath(resolvedLocalDirectoryPath,
                  mustWork = TRUE,
                  winslash = "/")


  # 2. Default individual file paths if they were missing, using the resolvedLocalDirectoryPath.
  #    If they were provided, use the provided value (which could be a full path or relative).
  resolvedLocalOptionsPath <-
    if (missing(LocalOptionsPath_arg) ||
        is.null(LocalOptionsPath_arg)) {
      file.path(resolvedLocalDirectoryPath, "options.json")
    } else {
      LocalOptionsPath_arg
    }

  resolvedLocalTemplatePath <-
    if (missing(LocalTemplatePath_arg) ||
        is.null(LocalTemplatePath_arg)) {
      file.path(resolvedLocalDirectoryPath, "template.txt")
    } else {
      LocalTemplatePath_arg
    }

  resolvedLocalTokensPath <-
    if (missing(LocalTokensPath_arg) ||
        is.null(LocalTokensPath_arg)) {
      file.path(resolvedLocalDirectoryPath, "tokens.json")
    } else {
      LocalTokensPath_arg
    }

  resolvedLocalOptionsPath <-
    normalizePath(resolvedLocalOptionsPath,
                  mustWork = TRUE,
                  winslash = "/")
  resolvedLocalTemplatePath <-
    normalizePath(resolvedLocalTemplatePath,
                  mustWork = TRUE,
                  winslash = "/")
  resolvedLocalTokensPath <-
    normalizePath(resolvedLocalTokensPath,
                  mustWork = TRUE,
                  winslash = "/")

  return(
    list(
      LocalDirectoryPath = resolvedLocalDirectoryPath,
      LocalOptionsPath   = resolvedLocalOptionsPath,
      LocalTemplatePath  = resolvedLocalTemplatePath,
      LocalTokensPath    = resolvedLocalTokensPath
    )
  )
}

.establish_Connection <- function(User,
                                  Host,
                                  KeyPath,
                                  Password) {
  message("Establishing SSH connection to ", User, "@", Host, "...")
  Session <- NULL # Initialize to ensure it's in scope for on.exit
  tryCatch({
    Session <- ssh::ssh_connect(
      paste0(User, "@", Host),
      keyfile = if (!is.null(KeyPath) &&
                    KeyPath != "")
        KeyPath
      else
        NULL,
      passwd = if (!is.null(Password))
        Password
      else
        "",
      verbose = getOption("verbose", default = FALSE)
    )

    message("SSH connection established.")
  }, error = function(e) {
    stop("SSH connection failed: ", e$message)
  })

  Session
}

#' Transfer Local NLME/RsNLME License Files to Remote Host
#'
#' This function attempts to find local Certara license files ('auth.json' and
#' 'license.jwt') and upload them to the corresponding standard locations on a
#' remote Linux host if they do not already exist there.
#'
#' @param Session An active SSH session object.
#' @param ClientId Character string: The client ID, typically "wnl".
#' @param verbose Logical: If TRUE, prints more detailed messages.
#'
#' @return Invisibly returns a list with status for each file:
#'   `list(auth_json = "uploaded" / "exists_remote" / "not_found_local" / "failed",
#'        license_jwt = "uploaded" / "exists_remote" / "not_found_local" / "failed")`
#'
#' @noRd
.transfer_LicenseFiles <-
  function(Session,
           ClientId = "wnl",
           verbose = getOption("verbose", default = FALSE)) {
    # --- Function to get CAD_AUTH_PATH from remote ---
    .get_remote_cad_auth_path <-
      function(Sess,
               verbose = getOption("verbose", default = FALSE)) {
        CmdCADAuth <- "echo $CAD_AUTH_PATH"
        CadAuthOut <- tryCatch({
          ssh::ssh_exec_internal(Sess, command = CmdCADAuth, error = FALSE)
        }, error = function(e) {
          if (verbose)
            message("  Error checking $CAD_AUTH_PATH on remote: ", e$message)
          return(NULL)
        })

        if (!is.null(CadAuthOut) &&
            CadAuthOut$status == 0 &&
            length(CadAuthOut$stderr) == 0) {
          PathStr <- trimws(rawToChar(CadAuthOut$stdout))
          if (nzchar(PathStr)) {
            if (verbose)
              message(paste("  Remote $CAD_AUTH_PATH resolved to:", PathStr))
            return(PathStr)
          }
        }
        if (verbose)
          message("  Remote $CAD_AUTH_PATH not found or empty.")
        return(NULL) # Not set or empty
      }

    # --- Function to get local license file path ---
    .get_local_license_path <-
      function(filename,
               client_id,
               verbose = getOption("verbose", default = FALSE)) {
        cad_auth_path_local <- Sys.getenv("CAD_AUTH_PATH")
        appdata_local <-
          Sys.getenv(if (.Platform$OS.type == "windows")
            "APPDATA"
            else
              "") # APPDATA is Windows specific

        if (nzchar(cad_auth_path_local)) {
          return(normalizePath(
            file.path(cad_auth_path_local, filename),
            mustWork = FALSE,
            winslash = "/"
          ))
        } else if (.Platform$OS.type == "windows" &&
                   nzchar(appdata_local)) {
          return(normalizePath(
            file.path(appdata_local, "Certara", "Auth", client_id, filename),
            mustWork = FALSE,
            winslash = "/"
          ))
        } else if (.Platform$OS.type == "unix") {
          # This part is for finding *local* files on a Unix-like system (where R is running)
          return(normalizePath(
            file.path(
              Sys.getenv("HOME"),
              ".certara",
              "auth",
              client_id,
              filename
            ),
            mustWork = FALSE,
            winslash = "/"
          ))
        }
        return(NA_character_) # Should not happen if OS is Windows/Unix
      }

    # --- Function to determine remote target path (Linux specific) ---
    .determine_remote_target_path <-
      function(Sess,
               filename,
               client_id,
               remote_cad_auth_path_val,
               home_dir_val,
               verbose =  getOption("verbose", default = FALSE)) {
        if (!is.null(remote_cad_auth_path_val) &&
            nzchar(remote_cad_auth_path_val)) {
          # CAD_AUTH_PATH is set on remote, use it directly (no tilde expansion needed here by definition)
          return(file.path(remote_cad_auth_path_val, filename, fsep = "/"))
        } else {
          # Use ~/.certara/auth/[client-id]/filename
          # Ensure home_dir_val is not NULL
          if ((is.null(home_dir_val) ||
               !nzchar(home_dir_val)) &&
              verbose) {
            message(
              "Remote HOME directory could not be determined. Cannot construct default license path."
            )
          }

          return(file.path(
            home_dir_val,
            ".certara",
            "auth",
            client_id,
            filename,
            fsep = "/"
          ))
        }
      }

    # --- Main logic ---
    files_to_transfer <- list(
      list(
        local_name = "auth.json",
        remote_name = "auth.json",
        id = "auth_json"
      ),
      list(
        local_name = "license.jwt",
        remote_name = "license.jwt",
        id = "license_jwt"
      )
    )

    results_status <-
      stats::setNames(vector("list", length(files_to_transfer)),
                      sapply(files_to_transfer, `[[`, "id"))

    # Get remote CAD_AUTH_PATH and HOME once
    RemoteCadAuthPathVal <- .get_remote_cad_auth_path(Session)
    RemoteHomeDirVal <- .subst_Tilda("~", Session)

    for (file_info in files_to_transfer) {
      RemoteTargetPath <-
        .determine_remote_target_path(
          Session,
          file_info$remote_name,
          ClientId,
          RemoteCadAuthPathVal,
          RemoteHomeDirVal
        )

      if (verbose)
        message(paste(
          "  Determined remote target path:",
          RemoteTargetPath,
          "for",
          file_info$id
        ))

      # Check if file exists on remote
      CheckExistsCmd <-
        paste("[ -f",
              shQuote(RemoteTargetPath, type = "sh"),
              "] && echo exists")
      RemoteFileExists <- FALSE
      tryCatch({
        RemoteCheckOut <-
          ssh::ssh_exec_internal(Session, command = CheckExistsCmd, error = FALSE)
        if (RemoteCheckOut$status == 0 &&
            grepl("exists", rawToChar(RemoteCheckOut$stdout), fixed = TRUE)) {
          RemoteFileExists <- TRUE
        }
      }, error = function(e) {
        if (verbose)
          message(paste(
            "  Error checking if remote file exists",
            RemoteTargetPath,
            ":",
            e$message
          ))
        results_status[[file_info$id]] <-
          "failed_check_remote" # Or just "failed"
      })

      if (RemoteFileExists) {
        if (verbose)
          message(
            paste(
              "  Remote file already exists:",
              RemoteTargetPath,
              ". Skipping upload for",
              file_info$id,
              "."
            )
          )
        results_status[[file_info$id]] <- "exists_remote"
        next
      }

      LocalFilePath <-
        .get_local_license_path(file_info$local_name, ClientId)

      if (is.na(LocalFilePath) || !file.exists(LocalFilePath)) {
        if (verbose)
          message(paste(
            "  Local license file not found:",
            LocalFilePath,
            "for",
            file_info$id
          ))
        results_status[[file_info$id]] <- "not_found_local"
        next
      }

      if (verbose)
        message(paste(
          "  Found local license file:",
          LocalFilePath,
          "for",
          file_info$id
        ))

      if (verbose)
        message(
          paste(
            "  Remote file",
            RemoteTargetPath,
            "does not exist. Attempting upload for",
            file_info$id,
            "."
          )
        )
      # Ensure remote directory exists before uploading
      RemoteTargetDir <- dirname(RemoteTargetPath)
      MkdirCmd <-
        paste("mkdir -p", shQuote(RemoteTargetDir, type = "sh"))
      tryCatch({
        MkdirResult <-
          ssh::ssh_exec_wait(Session, command = MkdirCmd) # Simpler for mkdir
        if (MkdirResult != 0) {
          if (verbose)
            message(paste("Failed to create remote directory:", RemoteTargetDir))
          next
        }

        ssh::scp_upload(Session,
                        files = LocalFilePath,
                        to = RemoteTargetPath,
                        verbose = verbose)
        if (verbose)
          message(paste(
            "  Successfully uploaded",
            LocalFilePath,
            "to",
            RemoteTargetPath
          ))
        results_status[[file_info$id]] <- "uploaded"
      }, error = function(e) {
        if (verbose)
          message(paste(
            "  Failed to upload",
            file_info$id,
            "to",
            RemoteTargetPath,
            ":",
            e$message
          ))
        results_status[[file_info$id]] <- "failed_upload"
      })

    }

    return(invisible(results_status))
  }

#' @title Prepare Post-Processing Scripts for Remote Execution
#' @description Identifies and validates local post-processing scripts specified
#'   in pyDarwin options.
#' @param OriginalPyDarwinOptions A list representing the parsed content of the
#'   local options.json file.
#' @param LocalDirectoryPath The primary local directory of the project, used
#'   as a base for resolving relative paths.
#' @return A list containing the resolved local paths for the R and Python
#'   scripts (`LocalRScriptPath`, `LocalPyScriptPath`) and their respective
#'   basenames. Returns NULL for paths that are not found or not specified.
#' @noRd
.prepare_postprocess_scripts <-
  function(OriginalPyDarwinOptions,
           LocalDirectoryPath) {
    if (is.null(OriginalPyDarwinOptions$postprocess)) {
      return(
        list(
          LocalRScriptPath = NULL,
          LocalPyScriptPath = NULL,
          RScriptBaseName = NULL,
          PyScriptBaseName = NULL
        )
      )
    }

    postprocess_opts <- OriginalPyDarwinOptions$postprocess
    resolved_paths <-
      list(
        LocalRScriptPath = NULL,
        LocalPyScriptPath = NULL,
        RScriptBaseName = NULL,
        PyScriptBaseName = NULL
      )

    # --- Handle R Script ---
    if (!is.null(postprocess_opts$use_r) && postprocess_opts$use_r) {
      if (is.null(postprocess_opts$post_run_r_code) ||
          postprocess_opts$post_run_r_code == "") {
        warning(
          "'use_r' is TRUE but 'post_run_r_code' is not specified in options.json. No R script will be uploaded.",
          call. = FALSE
        )
      } else {
        # The path might contain "{project_dir}" which we resolve to the local project directory
        local_path <-
          sub(
            "{project_dir}",
            LocalDirectoryPath,
            postprocess_opts$post_run_r_code,
            fixed = TRUE
          )
        local_path <- normalizePath(local_path, mustWork = FALSE)

        if (!file.exists(local_path)) {
          stop(
            "Post-processing R script specified in options.json does not exist at resolved path: '",
            local_path,
            "'"
          )
        }
        resolved_paths$LocalRScriptPath <- local_path
        resolved_paths$RScriptBaseName <- basename(local_path)
        message("Found post-processing R script to upload: ", local_path)
      }
    }

    # --- Handle Python Script ---
    if (!is.null(postprocess_opts$use_python) &&
        postprocess_opts$use_python) {
      if (is.null(postprocess_opts$post_run_python_code) ||
          postprocess_opts$post_run_python_code == "") {
        warning(
          "'use_python' is TRUE but 'post_run_python_code' is not specified in options.json. No Python script will be uploaded.",
          call. = FALSE
        )
      } else {
        local_path <-
          sub(
            "{project_dir}",
            LocalDirectoryPath,
            postprocess_opts$post_run_python_code,
            fixed = TRUE
          )
        local_path <- normalizePath(local_path, mustWork = FALSE)

        if (!file.exists(local_path)) {
          stop(
            "Post-processing Python script specified in options.json does not exist at resolved path: '",
            local_path,
            "'"
          )
        }
        resolved_paths$LocalPyScriptPath <- local_path
        resolved_paths$PyScriptBaseName <- basename(local_path)
        message("Found post-processing Python script to upload: ",
                local_path)
      }
    }

    return(resolved_paths)
  }
