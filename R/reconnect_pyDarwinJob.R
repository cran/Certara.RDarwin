#' Reconnect, Monitor, and Retrieve Results from a Remote pyDarwin Job
#'
#' This function reconnects to a pyDarwin job previously launched in the background
#' on a remote host. It monitors the job until completion (if a PID is available),
#' then downloads and processes the results.
#'
#' @param LocalDirectoryPath Character string: The base local directory associated
#'        with the pyDarwin job. This directory is used to:
#'        1. Locate the job information file (if `LocalJobInfoFilePath` is NULL),
#'           expected as `{ProjectName}_remote_job_info.json`.
#'        2. Locate the original `options.json` file (if `OriginalOptionsPath` is NULL).
#'        3. Serve as the base location for downloading results into a subdirectory
#'           (e.g., `{LocalDirectoryPath}/{ProjectName}_Results/`).
#' @param LocalJobInfoFilePath Character string (optional): Explicit path to the local
#'        JSON file containing information about the remote job (e.g., as created by
#'        `RunPyDarwinRemote(Wait = FALSE)`). If NULL (default), the path is constructed
#'        using `LocalDirectoryPath` and `ProjectName` (derived from `OriginalOptionsPath`).
#' @param OriginalOptionsPath Character string (optional): Explicit path to the original
#'        local `options.json` file that was used when the job was first launched.
#'        This is needed to correctly parse results (e.g., `engine_adapter`) and
#'        to derive `ProjectName` if not available from `LocalJobInfoFilePath`.
#'        If NULL (default), the function attempts to find `options.json` within
#'        `LocalDirectoryPath`. If not found, the operation will stop.
#' @param verbose Logical: Passed to helper functions for verbose output during
#'        SSH connection and file downloads.
#'        Default: `getOption("verbose", default = FALSE)`.
#' @inheritParams run_pyDarwinRemote
#'
#' @return A list containing parsed results similar to `RunPyDarwinRemote(Wait = TRUE)`
#'         (i.e., `results` data.frame, `FinalResultFile`, `FinalControlFile`,
#'         `DownloadedResultsDir`, `DownloadedItems`), or the content of the
#'         downloaded `messages.txt` as a character vector if primary result
#'         files are not found or parsed successfully. If essential information
#'         (like job info or options file) is missing, the function will stop.
#'
#' @details
#' This function requires a job information JSON file (typically created by
#' `RunPyDarwinRemote` when `Wait = FALSE`) to obtain details like the remote host,
#' user, remote project directory, and optionally the remote process ID (PID).
#'
#' The `ProjectName` is crucial. It's primarily derived from the `project_name`
#' field in the original options file (located via `OriginalOptionsPath` or within
#' `LocalDirectoryPath`). If not present in the options file, a fallback derivation
#' uses the parent directory name of the options file. This `ProjectName` is then
#' used to find the job info file (as `{ProjectName}_remote_job_info.json` in
#' `LocalDirectoryPath`) if `LocalJobInfoFilePath` is not directly provided.
#' If the job info file itself contains a `ProjectName`, that value may take precedence.
#' Downloaded results are organized locally using this determined `ProjectName`.
#'
#' If `RemoteJobPID` is available in the job info file, the function will actively
#' monitor the process. If the PID is not available, it will skip active monitoring
#' and proceed directly to attempt downloading any available results.
#'
#' @seealso [run_pyDarwin()], [run_pyDarwinRemote()]
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_project_job_info.json' and 'options.json' exist in '~/darwin_runs/my_project_run'
#' # and 'my_project_job_info.json' was created by a previous RunPyDarwinRemote(Wait=FALSE) call.
#'
#' # Example 1: Specifying only the local directory path
#' # ProjectName will be derived from options.json within that path.
#' # Job info file will be sought as {ProjectName}_remote_job_info.json.
#' try({
#'   reconnect_pyDarwinJob(
#'     LocalDirectoryPath = "~/darwin_runs/my_project_run"
#'   )
#' })
#'
#' # Example 2: Specifying paths explicitly
#' try({
#'   reconnect_pyDarwinJob(
#'     LocalDirectoryPath = "~/darwin_runs/my_project_run", # Still used for downloads
#'     LocalJobInfoFilePath = "~/darwin_runs/my_project_run/my_project_remote_job_info.json",
#'     OriginalOptionsPath = "~/darwin_runs/my_project_run/options.json",
#'     KeyPath = "~/.ssh/id_rsa_remote_server"
#'   )
#' })
#' }
#'
#'
#' @export
reconnect_pyDarwinJob <- function(LocalDirectoryPath = ".",
                                           LocalJobInfoFilePath = NULL,
                                           OriginalOptionsPath = NULL,
                                           Password = NULL,
                                           KeyPath = NULL,
                                           MonitoringInterval = 30,
                                           verbose = getOption("verbose", default = FALSE)) {

  # --- 1. Get Remote Job Details using the helper ---
  JobDetails <- tryCatch(
    .get_RemoteJobDetails(
      LocalDirectoryPath = LocalDirectoryPath,
      LocalJobInfoFilePath = LocalJobInfoFilePath,
      OriginalOptionsPath = OriginalOptionsPath,
      ProjectNameFromArgs = NULL # Reconnect uses ProjectName from options/job file primarily
    ),
    error = function(e) {
      stop("Could not retrieve remote job details: ", e$message, call. = FALSE)
    }
  )

  Host <- JobDetails$Host
  User <- JobDetails$User
  RemoteProjectDir <- JobDetails$RemoteProjectDir
  RemoteJobPID <- JobDetails$RemoteJobPID
  ProjectName <- JobDetails$ProjectName # Use the consistently derived ProjectName
  OriginalPyDarwinOptions <- JobDetails$OriginalPyDarwinOptions

  FinalKeyPath <- KeyPath
  if (is.null(FinalKeyPath)) {
    FinalKeyPath <- Sys.getenv("SSH_PRIVATE_KEY_PATH")
  }

  # --- 2. Determine Local Base Directory for Downloaded Results ---
  LocalResultsDownloadBase <- LocalDirectoryPath

  # --- 3. Establish SSH Connection ---
  message("Attempting to reconnect to ", User, "@", Host, "...")
  Session <- NULL
  tryCatch({
    Session <- ssh::ssh_connect(
      paste0(User, "@", Host),
      keyfile = if (!is.null(FinalKeyPath) && nzchar(FinalKeyPath) && file.exists(FinalKeyPath)) FinalKeyPath else NULL,
      passwd = if (!is.null(Password)) Password else NULL,
      verbose = verbose
    )
    message("SSH connection re-established.")
  }, error = function(e) {
    stop("SSH reconnection failed: ", e$message)
  })
  on.exit({
    if (!is.null(Session) && ssh::ssh_session_info(Session)$connected) {
      message("Disconnecting SSH session...")
      ssh::ssh_disconnect(Session)
      if (verbose) message("SSH session disconnected.") # Keep verbose check for disconnect msg
    }
  }, add = TRUE)

  # --- 4. Monitor Job (if PID was valid) ---
  MonitoringResult <- NULL
  CanMonitorPID <- !is.null(RemoteJobPID) && !is.na(RemoteJobPID) && nzchar(as.character(RemoteJobPID))

  if (CanMonitorPID) {
    message("Attempting to monitor remote job (PID: ", RemoteJobPID, ").")
    InitialMessagesByteOffsetForReconnect <- 0

    MonitoringResult <- .monitor_pyDarwinJob(
      Session = Session,
      RemoteJobPID = RemoteJobPID,
      RemoteProjectDir = RemoteProjectDir,
      MonitoringInterval = MonitoringInterval,
      InitialMessagesByteOffset = InitialMessagesByteOffsetForReconnect
    )
  } else {
    warning("RemoteJobPID not found in job info file or is invalid. Cannot actively monitor PID.",
            " Will proceed directly to attempt downloading results.", call. = FALSE)
    MonitoringResult <- list(
      Status = "PID_Unknown_Skipping_Monitor",
      ProcessValue = -1
    )
  }

  # --- 5. Download Results ---
  message("Monitoring/pre-check finished. Status from monitoring: ", MonitoringResult$Status,
          ". Attempting to download results from: ", RemoteProjectDir)

  DownloadedResultInfo <- .download_pyDarwinResults(
    Session = Session,
    RemoteProjectDir = RemoteProjectDir,
    OriginalPyDarwinOptions = OriginalPyDarwinOptions,
    LocalBaseDownloadDir = LocalResultsDownloadBase,
    ProjectName = NULL, # Download directly into LocalResultsDownloadBase
    verbose = verbose
  )

  # --- 6. Process Downloaded Results ---
  ProcessedResults <- .process_pyDarwinResults(
    DownloadedResultInfo = DownloadedResultInfo,
    OriginalPyDarwinOptions = OriginalPyDarwinOptions,
    LocalResultsDownloadBase = LocalResultsDownloadBase,
    Session = Session,
    RemoteProjectDir = RemoteProjectDir,
    MonitoringResult = MonitoringResult,
    RemoteJobPID = RemoteJobPID
  )

  # --- 7. Final Return Value ---
  # (This logic remains the same as your previous Reconnect function)
  if (is.character(ProcessedResults) && length(ProcessedResults) > 0) {
    message("Primary result files were not successfully parsed. Returning content of downloaded messages.txt.")
    return(ProcessedResults)
  } else if (is.list(ProcessedResults)) {
    if (nrow(ProcessedResults$results) == 0 &&
        length(ProcessedResults$FinalResultFile) == 0 &&
        length(ProcessedResults$FinalControlFile) == 0) {
      warning("No structured pyDarwin results were found or parsed from downloaded files after reconnect.", call. = FALSE)
      message("Returning basic information about download attempts and locations.")
      return(ProcessedResults)
    } else {
      message("Successfully processed downloaded pyDarwin results after reconnect.")
      return(ProcessedResults)
    }
  } else {
    warning("Unexpected format returned from result processing after reconnect. Returning empty list.", call. = FALSE)
    return(list())
  }
}

#' Monitors a Remote pyDarwin Job (Without Exit Status File)
#'
#' This function periodically checks the status of a running pyDarwin job on a
#' remote server by monitoring its PID and streams log updates. Job completion
#' is determined by the disappearance of the PID.
#'
#' @param Session An active SSH session object.
#' @param RemoteJobPID Character or numeric: The PID of the pyDarwin process on the remote host.
#' @param RemoteProjectDir Character string: The path to the project directory on the remote host.
#' @param MonitoringInterval Numeric: Time in seconds to wait between status checks. Default: 30.
#' @param InitialMessagesByteOffset Numeric: The starting byte offset for reading `messages.txt`.
#'        Useful for resuming monitoring. Default: 0.
#' @param MaxJobTimeoutSeconds Numeric (optional): Maximum total time in seconds to monitor the job.
#'        If NULL (default), no timeout is applied by this loop.
#'
#' @return A list containing:
#'   \item{ProcessValue}{Integer: -99 if the PID disappeared (assumed crash/killed/finished without explicit success signal).
#'                       Note: A value of 0 (success) would typically be determined by the calling
#'                       function after successfully parsing results post-monitoring.}
#'   \item{LastMessagesByteOffset}{Numeric: The final byte offset read from `messages.txt`.}
#'   \item{MonitoringDurationSeconds}{Numeric: Total duration of the monitoring in seconds.}
#'   \item{Status}{Character: A string indicating "PID_Gone" (implying finished, crashed, or killed) or "TimedOut".}
#'
#' @noRd
.monitor_pyDarwinJob <- function(Session,
                                 RemoteJobPID,
                                 RemoteProjectDir,
                                 MonitoringInterval = 30,
                                 InitialMessagesByteOffset = 0) {
  message(
    "Monitoring remote pyDarwin job (PID: ",
    RemoteJobPID,
    ") in directory: ",
    RemoteProjectDir
  )
  JobConsideredFinishedByMonitoring <- FALSE
  ProcessValue <- -1 # Default, should be overwritten
  MonitoringStartTime <- Sys.time()
  LastMessagesByteOffset <- InitialMessagesByteOffset

  # Define path to messages.txt for streaming
  RemoteMessagesFile <-
    file.path(RemoteProjectDir, "messages.txt", fsep = "/")
  FinalStatusString <- "Monitoring" # Initial status

  # Verify RemoteJobPID is not NA or empty before starting
  if (is.na(RemoteJobPID) || !nzchar(as.character(RemoteJobPID))) {
    warning("Invalid RemoteJobPID provided to .monitor_pyDarwinJob. Cannot monitor.")
    return(
      list(
        ProcessValue = -100,
        # Special error code for bad PID input
        LastMessagesByteOffset = InitialMessagesByteOffset,
        MonitoringDurationSeconds = 0,
        Status = "InvalidPID"
      )
    )
  }


  while (!JobConsideredFinishedByMonitoring) {
    # 1. Check if the PID is still running
    IsProcessRunning <- .check_RemotePID(Session, RemoteJobPID)

    # 2. Stream updates from messages.txt
    NewOffsetAndLines <- .stream_RemoteFileUpdates(
      Session = Session,
      RemoteFilePath = RemoteMessagesFile,
      LastKnownByteOffset = LastMessagesByteOffset,
      StreamToConsole = TRUE
    )
    LastMessagesByteOffset <- NewOffsetAndLines$NewByteOffset

    if (!IsProcessRunning) {
      if (getOption("verbose", default = FALSE)) {
        message("Remote process (PID: ",
                RemoteJobPID,
                ") is no longer running.")
      }
      ProcessValue <- -99 # Indicates PID disappeared
      JobConsideredFinishedByMonitoring <- TRUE
      FinalStatusString <- "PID_Gone"
    } else {
      # Job still appears to be running.
      if (getOption("verbose", default = FALSE)) {
        message(
          "Job (PID: ",
          RemoteJobPID,
          ") still running. Last messages log size: ",
          LastMessagesByteOffset,
          " bytes. Waiting for ",
          MonitoringInterval,
          "s..."
        )
      }

      Sys.sleep(MonitoringInterval)
    }

  } # End while(!JobConsideredFinishedByMonitoring)

  MonitoringDurationSeconds <-
    as.numeric(difftime(Sys.time(), MonitoringStartTime, units = "secs"))
  if (getOption("verbose", default = FALSE)) {
    message(
      "Monitoring loop finished for PID ",
      RemoteJobPID,
      ". Duration: ",
      round(MonitoringDurationSeconds, 1),
      "s. Final Status from monitoring: ",
      FinalStatusString
    )
  }

  return(
    list(
      ProcessValue = ProcessValue,
      LastMessagesByteOffset = LastMessagesByteOffset,
      MonitoringDurationSeconds = MonitoringDurationSeconds,
      Status = FinalStatusString
    )
  )
}

.get_RemoteFileContent <-
  function(Session, RemoteFilePath, silent = TRUE) {
    TempFile <- tempfile()
    on.exit(unlink(TempFile), add = TRUE)
    tryCatch({
      ssh::scp_download(Session,
                        files = RemoteFilePath,
                        to = TempFile,
                        verbose = FALSE)
      return(readLines(TempFile, warn = FALSE))
    }, error = function(e) {
      if (!silent)
        warning("Error downloading or reading remote file '",
                RemoteFilePath,
                "': ",
                e$message)
      return(NULL)
    })
  }

.stream_RemoteFileUpdates <-
  function(Session,
           RemoteFilePath,
           LastKnownByteOffset = 0,
           StreamToConsole = TRUE) {
    NewLines <- character(0)
    CurrentFileSize <- 0

    # Get current file size (more reliable than just checking existence for tail -c)
    SizeCmd <-
      paste("stat -c%s",
            shQuote(RemoteFilePath, type = "sh"),
            "2>/dev/null || echo 0") # Get size, or 0 if error/not found

    CurrentFileSize <- tryCatch({
      SizeRaw <- ssh::ssh_exec_internal(Session, command = SizeCmd)$stdout
      as.numeric(rawToChar(SizeRaw))
    },
    warning = function(w)
      0,
    error = function(e)
      0)

    if (CurrentFileSize > LastKnownByteOffset) {
      BytesToRead <- CurrentFileSize - LastKnownByteOffset
      # tail -c expects positive arg for last N bytes, or +N for Nth byte to end
      # So we want to start reading from LastKnownByteOffset + 1
      NextByteToRead <- LastKnownByteOffset + 1
      TailCmd <-
        paste0("tail -c +",
               NextByteToRead,
               " ",
               shQuote(RemoteFilePath, type = "sh"))

      NewContentRaw <-
        tryCatch(
          ssh::ssh_exec_internal(Session, command = TailCmd)$stdout,
          error = function(e)
            raw(0)
        )

      if (length(NewContentRaw) > 0) {
        # Handle potential partial last line: R's readLines from raw connection might be better
        # For simplicity with ssh_exec_stdout, split by newline.
        # This might print a partial last line if no trailing newline yet.
        # Convert raw to character, being mindful of encoding
        NewContentStr <-
          rawToChar(NewContentRaw) # Default conversion
        # If specific encoding: iconv(rawToChar(NewContentRaw), from="", to=encoding) but ssh_exec_stdout is just bytes.

        NewLines <- strsplit(NewContentStr, "\n", fixed = TRUE)[[1]]
        # If the content ended with a newline, the last element of strsplit will be ""
        # If it didn't (partial line), the last element is that partial line.
        # We print all of them.

        if (StreamToConsole && length(NewLines) > 0) {
          # Avoid printing a final "" if it's just due to trailing newline
          # but print if it's the only content (e.g. just a "\n")
          PrintableLines <- NewLines
          if (length(NewLines) > 1 &&
              NewLines[length(NewLines)] == "" &&
              grepl("\n$", NewContentStr)) {
            PrintableLines <- NewLines[-length(NewLines)]
          }

          if (length(PrintableLines) > 0 &&
              any(nzchar(PrintableLines))) {
            cat(paste(PrintableLines, collapse = "\n"), "\n", sep = "") # Ensure a final newline from cat
          }
        }
      }
    }

    return(list(NewLines = NewLines, NewByteOffset = CurrentFileSize))
  }

#' Download pyDarwin Result Files and Directories
#'
#' Attempts to download standard output directories and the messages.txt file
#' from a remote pyDarwin project directory. Failures to download individual
#' items are handled silently (no errors or warnings propagated).
#'
#' @param Session An active SSH session object.
#' @param RemoteProjectDir Character string: The absolute path to the project
#'        directory on the remote host.
#' @param OriginalPyDarwinOptions List: The parsed content of the original
#'        options.json file, used to determine specific directory names.
#' @param LocalBaseDownloadDir Character string: The local directory into which
#'        the 'output', 'key_models', 'non_dominated_models' subdirectories
#'        and 'messages.txt' will be downloaded.
#' @param ProjectName Character string (optional): The project name, used for
#'        creating a subdirectory within LocalBaseDownloadDir if LocalBaseDownloadDir
#'        itself is generic. If NULL, LocalBaseDownloadDir is used directly.
#' @param verbose Logical: whether to output diagnostic messages
#'
#' @return A list containing:
#'   \item{LocalMessagesPath}{Path to the downloaded messages.txt.}
#'   \item{LocalOutputDirPath}{Local path for the 'output' directory.}
#'   \item{LocalKeyModelsDirPath}{Local path for the 'key_models' directory.}
#'   \item{LocalNonDominatedModelsDirPath}{Local path for the 'non_dominated_models' directory.}
#'   \item{SuccessDownloads}{A character vector listing items successfully downloaded
#'                           (e.g., "messages.txt", "output_dir").}
#'   \item{AttemptedDownloads}{A list of all items that were attempted, with their
#'                             resolved remote and local paths.}
#'
#' @noRd
#' @importFrom utils untar
.download_pyDarwinResults <- function(Session,
                                      RemoteProjectDir,
                                      OriginalPyDarwinOptions,
                                      LocalBaseDownloadDir,
                                      ProjectName = NULL,
                                      verbose = getOption("verbose", default = FALSE)) {
  if (!is.null(ProjectName) && nzchar(ProjectName)) {
    TargetLocalBaseDir <-
      normalizePath(
        file.path(LocalBaseDownloadDir, ProjectName),
        mustWork = FALSE,
        winslash = "/"
      )
  } else {
    TargetLocalBaseDir <-
      normalizePath(LocalBaseDownloadDir,
                    mustWork = FALSE,
                    winslash = "/")
  }

  if (!dir.exists(TargetLocalBaseDir)) {
    dir.create(TargetLocalBaseDir,
               recursive = TRUE,
               showWarnings = FALSE)
  }

  .resolve_remote_path <-
    function(option_value,
             default_subdir_name,
             base_remote_dir) {
      path <- default_subdir_name
      if (!is.null(option_value) && nzchar(option_value)) {
        path <- option_value
      }
      path <-
        gsub("{working_dir}", base_remote_dir, path, fixed = TRUE)
      path <-
        gsub("{project_dir}", base_remote_dir, path, fixed = TRUE)
      if (!startsWith(path, "/")) {
        return(file.path(base_remote_dir, path, fsep = "/"))
      }
      return(path)
    }

  items_to_download_config <- list(
    list(
      item_id = "messages.txt",
      remote_path = file.path(RemoteProjectDir, "messages.txt", fsep = "/"),
      local_path = file.path(TargetLocalBaseDir, "messages.txt"),
      is_directory = FALSE
    ),
    list(
      item_id = "output_dir",
      remote_path = .resolve_remote_path(
        OriginalPyDarwinOptions$output_dir,
        "output",
        RemoteProjectDir
      ),
      local_path = file.path(TargetLocalBaseDir, "output"),
      is_directory = TRUE
    ),
    list(
      item_id = "key_models_dir",
      remote_path = .resolve_remote_path(
        OriginalPyDarwinOptions$key_models_dir,
        "key_models",
        RemoteProjectDir
      ),
      local_path = file.path(TargetLocalBaseDir, "key_models"),
      is_directory = TRUE
    ),
    list(
      item_id = "non_dominated_models_dir",
      remote_path = .resolve_remote_path(
        OriginalPyDarwinOptions$non_dominated_models_dir,
        "non_dominated_models",
        RemoteProjectDir
      ),
      local_path = file.path(TargetLocalBaseDir, "non_dominated_models"),
      is_directory = TRUE
    )
  )

  successful_downloads <- character(0)
  attempted_downloads_info <- list()

  if (verbose)
    message("Attempting to download results to overall base: ",
            TargetLocalBaseDir)

  for (item in items_to_download_config) {
    if (verbose)
      message(paste0("--- Processing download for: ", item$item_id, " ---"))
    if (verbose)
      message(
        paste(
          "  Attempting to download from:",
          item$remote_path,
          "to (final target for contents):",
          item$local_path
        )
      )

    attempted_downloads_info[[item$item_id]] <-
      list(remote = item$remote_path,
           local = item$local_path)
    download_success_for_item <- FALSE
    # Explicitly track if "No such file or directory" warning occurred
    scp_no_such_file_warning_occurred <- FALSE

    tryCatch({
      # Use withCallingHandlers to specifically catch the warning from scp
      withCallingHandlers({
        if (item$is_directory) {
          if (!dir.exists(item$local_path)) {
            dir.create(item$local_path,
                       recursive = TRUE,
                       showWarnings = verbose)
            if (verbose)
              message(paste(
                "  Created local target directory for contents:",
                item$local_path
              ))
          }
          TempContainerDir <-
            tempfile(pattern = paste0("scp_", item$item_id, "_"))
          # Ensure TempContainerDir is cleaned up even if scp_download warns/errors or subsequent steps error
          on.exit(unlink(
            TempContainerDir,
            recursive = TRUE,
            force = TRUE
          ),
          add = TRUE)

          dir.create(TempContainerDir,
                     showWarnings = verbose,
                     recursive = TRUE)
          if (verbose)
            message(paste(
              "  Created temp container for download:",
              TempContainerDir
            ))

          ssh::scp_download(
            session = Session,
            files = item$remote_path,
            to = TempContainerDir,
            verbose = verbose
          )
          # If scp_no_such_file_warning_occurred is TRUE here, scp_download didn't transfer anything.

          DownloadedDirInTempContainer <-
            file.path(TempContainerDir, basename(item$remote_path))

          if (!scp_no_such_file_warning_occurred &&
              dir.exists(DownloadedDirInTempContainer)) {
            if (verbose)
              message(
                paste(
                  "  Directory downloaded into temp location:",
                  DownloadedDirInTempContainer
                )
              )
            ItemsToMove <-
              list.files(
                DownloadedDirInTempContainer,
                all.files = TRUE,
                full.names = TRUE,
                no.. = TRUE,
                recursive = FALSE
              )
            if (length(ItemsToMove) > 0) {
              if (verbose)
                message(
                  paste(
                    "  Moving",
                    length(ItemsToMove),
                    "item(s) from",
                    DownloadedDirInTempContainer,
                    "to",
                    item$local_path
                  )
                )
              CopySuccessFlags <-
                file.copy(
                  from = ItemsToMove,
                  to = item$local_path,
                  recursive = TRUE,
                  overwrite = TRUE,
                  copy.mode = TRUE,
                  copy.date = TRUE
                )
              if (all(CopySuccessFlags)) {
                download_success_for_item <- TRUE
              } else {
                if (verbose)
                  message(
                    paste(
                      "  Warning: Not all items moved successfully from temp for",
                      item$item_id,
                      "to",
                      item$local_path
                    )
                  )
                if (any(CopySuccessFlags))
                  download_success_for_item <- TRUE
              }
            } else {
              if (verbose)
                message(
                  paste(
                    "  Downloaded directory",
                    DownloadedDirInTempContainer,
                    "was empty."
                  )
                )
              download_success_for_item <- TRUE
            }
          } else if (scp_no_such_file_warning_occurred) {
            if (verbose)
              message(
                paste(
                  "  Remote source",
                  item$remote_path,
                  "not found (as per scp warning)."
                )
              )
          } else {
            if (verbose)
              message(
                paste(
                  "  Downloaded directory not found in temp or remote source did not exist:",
                  DownloadedDirInTempContainer
                )
              )
          }
          unlink(TempContainerDir,
                 recursive = TRUE,
                 force = TRUE) # Clean up temp container
          on.exit(NULL, add = FALSE) # Remove the specific on.exit for this TempContainerDir

        } else {
          # It's a file
          ParentDirLocalFile <- dirname(item$local_path)
          if (!dir.exists(ParentDirLocalFile)) {
            dir.create(ParentDirLocalFile,
                       recursive = TRUE,
                       showWarnings = verbose)
            if (verbose)
              message(paste(
                "  Created parent directory for file:",
                ParentDirLocalFile
              ))
          }
          ssh::scp_download(
            session = Session,
            files = item$remote_path,
            to = ParentDirLocalFile,
            verbose = verbose
          )
          if (!scp_no_such_file_warning_occurred) {
            # If no warning, assume success (file might be 0 byte)
            download_success_for_item <- TRUE
          } else {
            if (verbose)
              message(paste(
                "  Remote file",
                item$remote_path,
                "not found (as per scp warning)."
              ))
          }
        }
      }, warning = function(w) {
        # Check if this specific warning is the "No such file or directory" from scp
        if (grepl("No such file or directory", w$message, ignore.case = TRUE) &&
            grepl("scp:", w$message, ignore.case = TRUE)) {
          scp_no_such_file_warning_occurred <<-
            TRUE # Assign to the flag in the outer scope
          if (verbose)
            message(
              paste(
                "  Caught scp warning for",
                item$item_id,
                "(remote:",
                item$remote_path,
                "):",
                w$message
              )
            )
          # We muffle this specific warning here because we are handling it by checking scp_no_such_file_warning_occurred
          invokeRestart("muffleWarning")
        } else {
          # For any other warnings, let them propagate or handle them differently
          if (verbose)
            message(paste(
              "  Caught other warning for",
              item$item_id,
              ":",
              w$message
            ))
          warning(w) # Re-issue other warnings
        }
      }) # End of withCallingHandlers
    }, error = function(e) {
      # This catches actual R errors from scp_download or file.copy
      if (verbose) {
        message(
          paste(
            "  Error during download/processing for",
            item$item_id,
            "(remote:",
            item$remote_path,
            ") :",
            gsub("\\s+", " ", e$message)
          )
        )
      }
      # If TempContainerDir was defined and exists (from a directory item attempt), ensure cleanup
      if (item$is_directory &&
          exists("TempContainerDir") &&
          !is.null(TempContainerDir) && dir.exists(TempContainerDir)) {
        unlink(TempContainerDir,
               recursive = TRUE,
               force = TRUE)
      }
    }) # End of tryCatch

    if (download_success_for_item) {
      successful_downloads <- c(successful_downloads, item$item_id)
      if (verbose)
        message(paste("  Successfully processed download for item:", item$item_id))
    }
    if (verbose)
      message(paste0("--- Finished processing for: ", item$item_id, " ---"))
  } # End for loop

  get_local_path_for_item_id <- function(id) {
    idx <-
      which(sapply(items_to_download_config, `[[`, "item_id") == id)
    if (length(idx) > 0)
      items_to_download_config[[idx]]$local_path
    else
      NA_character_
  }

  returned_paths <- list(
    LocalMessagesPath = get_local_path_for_item_id("messages.txt"),
    LocalOutputDirPath = get_local_path_for_item_id("output_dir"),
    LocalKeyModelsDirPath = get_local_path_for_item_id("key_models_dir"),
    LocalNonDominatedModelsDirPath = get_local_path_for_item_id("non_dominated_models_dir")
  )

  final_message <- paste0(
    "Finished download attempts. Successfully downloaded items: ",
    if (length(successful_downloads) > 0)
      paste(successful_downloads, collapse = ", ")
    else
      "None",
    "."
  )
  message(final_message)

  return(c(
    returned_paths,
    list(
      SuccessDownloads = successful_downloads,
      AttemptedDownloads = attempted_downloads_info
    )
  ))
}

#' Check if a Process is Running on a Remote Host
#'
#' Uses `kill -0 <PID>` to check if a process with the given PID exists and
#' the current user has permissions to signal it (which implies it's running
#' and owned by a related user or is a system process accessible for this check).
#'
#' @param Session An active SSH session object.
#' @param PID Character or numeric: The Process ID to check on the remote host.
#'
#' @return Logical: `TRUE` if the process is running (kill -0 succeeds),
#'         `FALSE` otherwise (process not found, or permission denied which
#'         can also imply it's not a relevant running process for the user,
#'         or if PID is invalid).
#'
#' @noRd
.check_RemotePID <- function(Session, PID) {
  if (is.null(PID) || is.na(PID) || !nzchar(as.character(PID))) {
    return(FALSE)
  }

  Command <- paste("kill -0", shQuote(PID, type = "sh"))

  tryCatch({
    ExitStatus <-
      ssh::ssh_exec_wait(Session,
                         command = Command,
                         std_out = FALSE,
                         std_err = FALSE)
    return(ExitStatus == 0)
  }, error = function(e) {
    if (getOption("verbose", default = FALSE)) {
      message("SSH command execution error in .check_RemotePID: ",
              e$message)
    }

    return(FALSE)
  })
}
