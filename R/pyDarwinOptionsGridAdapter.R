#' Grid Adapter Options for pyDarwin
#'
#' This function creates a list of grid adapter options for pyDarwin, which are
#' used to configure the interaction between pyDarwin and grid computing
#' environments.
#'
#' @param python_path Required. Path to Python interpreter, preferably to the
#'   instance of the interpreter located in the virtual environment where
#'   pyDarwin is deployed. The path must be available to all grid nodes that run
#'   jobs.
#' @param submit_search_command Required. A command that submits a search job to
#'   the grid queue. This command is used for the entire search.
#' @param submit_command Required. A command that submits individual runs to the
#'   grid queue. The actual command submitted to the queue is constructed by
#'   pyDarwin. It should not include `<python_path> -m darwin.run_model`.
#' @param submit_job_id_re Required. A regular expression pattern to extract the
#'   job ID after submission. The job ID must be captured with the first
#'   capturing group.
#' @param poll_command Required. A command that retrieves finished jobs from the
#'   grid controller. If the controller/setup allows to specify ids/patterns in
#'   polling commands, do it. Otherwise, all  finished jobs should be polled
#'   using commands `qstat -s z`.
#' @param poll_job_id_re Required. A regular expression pattern to find a job ID
#'   in every line of the poll_command output. Similar to submit_job_id_re.
#' @param poll_interval Optional. How often to poll jobs (in seconds). Default
#'   is 10 seconds.
#' @param delete_command Optional. A command that deletes all unfinished jobs
#'   related to the search when you stop it. It may delete all of them by ID
#'   (e.g., `qdel {job_ids}`) or by mask (e.g., `qdel {project_stem}-*`)..
#'
#' @return A list containing the configured grid adapter options.
#'
#' @examples
#' grid_options <- pyDarwinOptionsGridAdapter(
#'   python_path = "~/darwin/venv/bin/python",
#'   submit_search_command =
#'     "qsub -b y -cwd -o {project_stem}_out.txt -e {project_stem}_err.txt -N '{project_name}'",
#'   submit_command =
#'     "qsub -b y -o {results_dir}/{run_name}.out -e {results_dir}/{run_name}.err -N {job_name}",
#'   submit_job_id_re = "Your job (\\w+) \\(\".+?\"\\) has been submitted",
#'   poll_command = "qstat -s z",
#'   poll_job_id_re = "^\\s+(\\w+)",
#'   poll_interval = 10,
#'   delete_command = "qdel {project_stem}-*"
#' )
#'
#' @keywords pyDarwin pyDarwinOptions
#'
#' @export
pyDarwinOptionsGridAdapter <-
  function(
    python_path =  "~/darwin/venv/bin/python",
    submit_search_command = paste(
      "qsub -b y -cwd -o {project_stem}_out.txt",
      "-e {project_stem}_err.txt -N '{project_name}'"
    ),
    submit_command = paste(
      "qsub -b y -o {results_dir}/{run_name}.out",
      "-e {results_dir}/{run_name}.err -N {job_name}"
    ),
    submit_job_id_re = "Your job (\\w+) \\(\".+?\"\\) has been submitted",
    poll_command = "qstat -s z",
    poll_job_id_re = "^\\s+(\\w+)",
    poll_interval = 10,
    delete_command = "qdel {project_stem}-*") {

    stopifnot(is.character(python_path))
    stopifnot(is.character(submit_search_command))
    stopifnot(is.character(submit_command))
    stopifnot(is.character(submit_job_id_re))
    stopifnot(is.character(poll_command))
    stopifnot(is.character(poll_job_id_re))
    stopifnot(is.numeric(poll_interval))
    stopifnot(is.character(delete_command))

    list(
      python_path =  python_path,
      submit_search_command = submit_search_command,
      submit_command = submit_command,
      submit_job_id_re = submit_job_id_re,
      poll_command = poll_command,
      poll_job_id_re = poll_job_id_re,
      poll_interval = poll_interval,
      delete_command = delete_command
    )
  }
