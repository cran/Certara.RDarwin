#' Stop a Remote pyDarwin Job
#'
#' Attempts to gracefully stop a running pyDarwin job on a remote host by
#' creating a 'stop.darwin' file in the remote project directory.
#'
#' @param LocalDirectoryPath Character string: The base local directory associated
#'        with the pyDarwin job. This directory is used to locate the job
#'        information file and the original options file.
#' @param LocalJobInfoFilePath Character string (optional): Explicit path to the local
#'        JSON file containing remote job information. If NULL (default), it's
#'        constructed using `LocalDirectoryPath` and `ProjectName` (derived from
#'        `OriginalOptionsPath`).
#' @param OriginalOptionsPath Character string (optional): Explicit path to the original
#'        local `options.json` file. If NULL (default), it's sought in
#'        `LocalDirectoryPath`. This is used to derive `ProjectName` if needed.
#' @param verbose Logical: Passed to `ssh::ssh_connect` for verbose SSH output.
#'        Default: `getOption("verbose", default = FALSE)`.
#' @inheritParams run_pyDarwinRemote
#'
#' @return Invisibly returns `TRUE` if the stop signal file was successfully created
#'         (or already existed), and `FALSE` if there was an SSH error or an error
#'         creating the file. This only signals the intent to stop; it does not
#'         confirm the pyDarwin process has actually stopped.
#'
#' @examples
#' \dontrun{
#' # Assuming 'my_project_remote_job_info.json' and 'options.json'
#' # exist in '~/darwin_runs/my_project_run'.
#' try({
#'   stop_pyDarwinRemote(
#'     LocalDirectoryPath = "~/darwin_runs/my_project_run",
#'     KeyPath = "~/.ssh/id_rsa_remote"
#'   )
#' })
#'
#' # Explicitly providing paths
#' try({
#'  stop_pyDarwinRemote(
#'    LocalDirectoryPath = "~/darwin_runs/my_project_run", # Still used as a base if needed
#'    LocalJobInfoFilePath = "~/darwin_runs/my_project_run/my_project_remote_job_info.json",
#'    OriginalOptionsPath = "~/darwin_runs/my_project_run/options.json"
#'  )
#' })
#' }
#' @export
stop_pyDarwinRemote <- function(LocalDirectoryPath,
                                LocalJobInfoFilePath = NULL,
                                OriginalOptionsPath = NULL,
                                Password = NULL,
                                KeyPath = NULL,
                                verbose = getOption("verbose", default = FALSE)) {
  # --- 1. Get Remote Job Details using the helper ---
  # ProjectNameFromArgs is not directly needed by Stop function itself beyond resolving paths if options are used
  # The helper will derive ProjectName primarily from options or job file.
  JobDetails <- tryCatch(
    .get_RemoteJobDetails(
      LocalDirectoryPath = LocalDirectoryPath,
      LocalJobInfoFilePath = LocalJobInfoFilePath,
      OriginalOptionsPath = OriginalOptionsPath,
      ProjectNameFromArgs = NULL # Or pass a ProjectName if known it's needed for path finding by helper
    ),
    error = function(e) {
      stop("Could not retrieve remote job details: ", e$message, call. = FALSE)
    }
  )

  Host <- JobDetails$Host
  User <- JobDetails$User
  RemoteProjectDir <- JobDetails$RemoteProjectDir
  # We don't need RemoteJobPID or OriginalPyDarwinOptions for stopping via stop.darwin file.

  FinalKeyPath <- KeyPath
  if (is.null(FinalKeyPath)) {
    FinalKeyPath <- Sys.getenv("SSH_PRIVATE_KEY_PATH")
  }

  # --- 2. Establish SSH Connection ---
  if (verbose) {
    message(
      "Attempting to connect to ",
      User,
      "@",
      Host,
      " to signal job stop for project in ",
      RemoteProjectDir
    )
  }

  Session <- NULL
  tryCatch({
    Session <- ssh::ssh_connect(
      paste0(User, "@", Host),
      keyfile = if (!is.null(FinalKeyPath) &&
                    nzchar(FinalKeyPath) &&
                    file.exists(FinalKeyPath))
        FinalKeyPath
      else
        NULL,
      passwd = if (!is.null(Password))
        Password
      else
        NULL,
      verbose = verbose
    )
    if (verbose)
      message("SSH connection established.")
  }, error = function(e) {
    # Specific error for stop function if SSH fails
    stop(
      "SSH connection failed while attempting to stop job. Cannot signal stop. Error: ",
      e$message,
      call. = FALSE
    )
  })

  on.exit({
    if (!is.null(Session) &&
        ssh::ssh_session_info(Session)$connected) {
      ssh::ssh_disconnect(Session)
      if (verbose)
        message("SSH session disconnected.")
    }
  }, add = TRUE)

  # --- 3. Create the 'stop.darwin' file ---
  RemoteStopFilePath <-
    file.path(RemoteProjectDir, "stop.darwin", fsep = "/")
  TouchCommand <-
    paste("touch", shQuote(RemoteStopFilePath, type = "sh"))

  if (verbose)
    message("Attempting to create stop signal file: ", RemoteStopFilePath)

  StopSignalSuccess <- FALSE
  tryCatch({
    ExitStatus <-
      ssh::ssh_exec_wait(Session,
                         command = TouchCommand,
                         std_out = FALSE,
                         std_err = TRUE) # Capture stderr

    if (ExitStatus == 0) {
      message(
        "Stop signal file 'stop.darwin' successfully created (or already existed) in ",
        RemoteProjectDir
      )
      StopSignalSuccess <- TRUE
    } else {
      warning(
        "Failed to create 'stop.darwin' file on remote host. 'touch' command exited with status: ",
        ExitStatus,
        ". The pyDarwin job may not stop. Check permissions for ",
        RemoteProjectDir,
        call. = FALSE
      )
      StopSignalSuccess <- FALSE
    }
  }, error = function(e) {
    warning("Error executing 'touch' command on remote host: ",
            e$message,
            call. = FALSE)
    StopSignalSuccess <- FALSE
  })

  return(invisible(StopSignalSuccess))
}

#' Retrieve and Validate Remote Job Details
#'
#' Reads job information from local files (options.json and the job info JSON)
#' to get necessary details for connecting to and interacting with a remote pyDarwin job.
#'
#' @param LocalDirectoryPath Character string: The base local directory associated
#'        with the pyDarwin job.
#' @param LocalJobInfoFilePath Character string (optional): Explicit path to the
#'        local JSON file containing remote job information. If NULL, it's
#'        constructed using `LocalDirectoryPath` and `ProjectName`.
#' @param OriginalOptionsPath Character string (optional): Explicit path to the
#'        original local `options.json` file. If NULL, it's sought in
#'        `LocalDirectoryPath`.
#' @param ProjectNameFromArgs Character string (optional): Project name passed directly
#'        as an argument to the calling function. Used if it cannot be derived
#'        from options or job info file.
#'
#' @return A list containing: `Host`, `User`, `RemoteProjectDir`, `RemoteJobPID` (can be NULL),
#'         `ProjectName`, and `OriginalPyDarwinOptions` (parsed content of options.json).
#'         Stops if critical files are not found or are malformed.
#'
#' @noRd
#' @import jsonlite
.get_RemoteJobDetails <- function(LocalDirectoryPath,
                                  LocalJobInfoFilePath = NULL,
                                  OriginalOptionsPath = NULL,
                                  ProjectNameFromArgs = NULL) {
  # --- 1. Validate LocalDirectoryPath ---
  if (missing(LocalDirectoryPath) ||
      is.null(LocalDirectoryPath) || !dir.exists(LocalDirectoryPath)) {
    stop(
      "LocalDirectoryPath '",
      LocalDirectoryPath,
      "' not found, not specified, or is not a directory.",
      call. = FALSE
    )
  }
  LocalDirectoryPath <-
    normalizePath(LocalDirectoryPath,
                  winslash = "/",
                  mustWork = TRUE)

  # --- 2. Determine and Read Original pyDarwin Options & ProjectName ---
  ActualOriginalOptionsPath <- OriginalOptionsPath
  if (is.null(ActualOriginalOptionsPath)) {
    DefaultOptionsPath <- file.path(LocalDirectoryPath, "options.json")
    # Fallback to project-specific subdir for options if top-level one not found AND ProjectNameFromArgs exists
    ProjectSpecificOptionsPath <- NULL
    if (!is.null(ProjectNameFromArgs) &&
        nzchar(ProjectNameFromArgs)) {
      ProjectSpecificOptionsPath <-
        file.path(LocalDirectoryPath,
                  ProjectNameFromArgs,
                  "options.json")
    }

    if (file.exists(DefaultOptionsPath)) {
      ActualOriginalOptionsPath <- DefaultOptionsPath
    } else if (!is.null(ProjectSpecificOptionsPath) &&
               file.exists(ProjectSpecificOptionsPath)) {
      ActualOriginalOptionsPath <- ProjectSpecificOptionsPath
    } else {
      stop(
        "OriginalOptionsPath not specified and 'options.json' not found in default locations (e.g., '",
        LocalDirectoryPath,
        "/options.json' or '",
        LocalDirectoryPath,
        "/",
        ProjectNameFromArgs,
        "/options.json').",
        call. = FALSE
      )
    }
  }

  if (!file.exists(ActualOriginalOptionsPath)) {
    stop("Resolved options file '",
         ActualOriginalOptionsPath,
         "' does not exist.",
         call. = FALSE)
  }
  OriginalPyDarwinOptions <- tryCatch(
    jsonlite::fromJSON(ActualOriginalOptionsPath),
    error = function(e) {
      stop(
        "Failed to read or parse options file '",
        ActualOriginalOptionsPath,
        "': ",
        e$message,
        call. = FALSE
      )
    }
  )

  ProjectName <- OriginalPyDarwinOptions$project_name
  if (is.null(ProjectName) || !nzchar(ProjectName)) {
    ProjectName <-
      basename(dirname(ActualOriginalOptionsPath)) # Fallback from options file's parent dir
    if (ProjectName == "." ||
        ProjectName == LocalDirectoryPath) {
      # Further fallback if dirname is not insightful
      ProjectName <-
        ProjectNameFromArgs # Use arg if path-derived name isn't good
    }
    if (is.null(ProjectName) || !nzchar(ProjectName)) {
      stop(
        "Could not determine ProjectName from options file ('",
        ActualOriginalOptionsPath,
        "') or arguments.",
        call. = FALSE
      )
    }
    message(
      "ProjectName derived as: '",
      ProjectName,
      "' (from options file path or arguments as fallback)"
    )
  }


  # --- 3. Determine and Read Local Job Info File ---
  ActualJobInfoFilePath <- LocalJobInfoFilePath
  if (is.null(ActualJobInfoFilePath)) {
    # ProjectName is now guaranteed to be non-NULL and non-empty here
    ActualJobInfoFilePath <-
      file.path(LocalDirectoryPath,
                paste0(ProjectName, "_remote_job_info.json"))
  }

  if (!file.exists(ActualJobInfoFilePath)) {
    stop(
      "Job info file not found at '",
      ActualJobInfoFilePath,
      "'. This file is created by RunPyDarwinRemote(Wait=FALSE).",
      call. = FALSE
    )
  }
  JobInfo <- tryCatch(
    jsonlite::fromJSON(ActualJobInfoFilePath),
    error = function(e) {
      stop(
        "Failed to read or parse job info file '",
        ActualJobInfoFilePath,
        "': ",
        e$message,
        call. = FALSE
      )
    }
  )

  # --- 4. Validate and Extract Required Fields from JobInfo ---
  required_fields_in_job_info <-
    c("Host", "User", "RemoteProjectDir")
  missing_fields <-
    setdiff(required_fields_in_job_info, names(JobInfo))
  if (length(missing_fields) > 0) {
    stop(
      "Job info file '",
      ActualJobInfoFilePath,
      "' is missing required fields: ",
      paste(missing_fields, collapse = ", "),
      call. = FALSE
    )
  }

  # Reconcile ProjectName from options with JobInfo if both exist. Prefer JobInfo's if different.
  if (!is.null(JobInfo$ProjectName) &&
      nzchar(JobInfo$ProjectName) &&
      JobInfo$ProjectName != ProjectName) {
    # This case might indicate a mismatch if user manually tampered or complex scenario
    warning(
      "ProjectName from options file ('",
      ProjectName,
      "') differs from ProjectName in job info file ('",
      JobInfo$ProjectName,
      "'). Using value from job info file.",
      call. = FALSE
    )
    ProjectName <- JobInfo$ProjectName
  }


  return(
    list(
      Host = JobInfo$Host,
      User = JobInfo$User,
      RemoteProjectDir = JobInfo$RemoteProjectDir,
      RemoteJobPID = JobInfo$RemoteJobPID,
      ProjectName = ProjectName,
      OriginalPyDarwinOptions = OriginalPyDarwinOptions,
      ActualJobInfoFilePath = ActualJobInfoFilePath,
      ActualOriginalOptionsPath = ActualOriginalOptionsPath # Return for reference
    )
  )
}
