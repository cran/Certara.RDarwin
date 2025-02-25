.strictify_FunctionArgs <- function() {
  # remove argument values from the function
  # since matching already happened
  parenv <- parent.frame()  # environment of the calling function
  rm(list = ls(parenv), envir = parenv)  # clear that environment

  # get the arguments
  scall <- sys.call(-1)  # 'call' of the calling function
  callingfun <- scall[[1]]
  scall[[1]] <- quote(`list`)
  args <-
    eval.parent(scall, 2)  # 'args' is now a list with all arguments

  # if none of the argument are named, we need to set the
  # names() of args explicitly
  if (is.null(names(args))) {
    names(args) <- rep("", length(args))
  }

  # get the function header ('formals') of the calling function
  callfun.object <- eval.parent(callingfun, 2)
  callfun.header <- formals(callfun.object)
  # create a dummy function that just gives us a link to its environment.
  # We will use this environment to access the parameter values. We
  # are not using the parameter values directly, since the default
  # parameter evaluation of R is pretty complicated.
  # (Consider fun <- function(x=y, y=x) { x } -- fun(x=3) and
  # fun(y=3) both return 3)
  dummyfun <- call("function", callfun.header, quote(environment()))
  dummyfun <- eval(dummyfun, envir = environment(callfun.object))
  parnames <- names(callfun.header)

  # Sort out the parameters that didn't match anything
  argsplit <- split(args, names(args) %in% c("", parnames))
  matching.args <- c(list(), argsplit$`TRUE`)
  nonmatching.arg.names <- names(argsplit$`FALSE`)

  # collect all arguments that match something (or are just
  # positional) into 'parenv'. If this includes '...', it will
  # be overwritten later.
  source.env <- do.call(dummyfun, matching.args)
  for (varname in ls(source.env, all.names = TRUE)) {
    parenv[[varname]] <- source.env[[varname]]
  }

  if (!"..." %in% parnames) {
    # Check if some parameters did not match. It is possible to get
    # here if an argument only partially matches.
    if (length(nonmatching.arg.names)) {
      stop(sprintf(
        "Nonmatching arguments: %s",
        paste(nonmatching.arg.names, collapse = ", ")
      ))
    }
  } else {
    # we manually collect all arguments that fall into '...'. This is
    # not trivial. First we look how many arguments before the '...'
    # were not matched by a named argument:
    open.args <- setdiff(parnames, names(args))
    taken.unnamed.args <- min(which(open.args == "...")) - 1
    # We throw all parameters that are unmatched into the '...', but we
    # remove the first `taken.unnamed.args` from this, since they go on
    # filling the unmatched parameters before the '...'.
    unmatched <- args[!names(args) %in% parnames]
    unmatched[which(names(unmatched) == "")[seq_len(taken.unnamed.args)]] <-
      NULL
    # we can just copy the '...' from a dummy environment that we create
    # here.
    dotsenv <- do.call(function(...)
      environment(), unmatched)
    parenv[["..."]] <- dotsenv[["..."]]
  }
}
