#' @noRd
#' @keywords internal NONMEM
output_PrintTemplateFunctions <-
  function(outputFile = "create_pydarwin_template.R") {
    packageEnvironment <- getNamespace("Certara.RDarwin")
    allFunctions <- ls(packageEnvironment)

    includedFunctions <-
      allFunctions[grepl(
        "^(add_)|(check_)|(create_Advan)|(gen_)|(modify_)|(omit_)|(paste_)|(print_)",
        allFunctions
      )]

    # exlude NLME fuinction
    includedFunctions <- includedFunctions[!grepl("NLME", includedFunctions)]
    includedFunctions <-
      includedFunctions[!grepl("(add_Covariate)|(add_StParm)", includedFunctions)]
    includedFunctions <-
      includedFunctions[!grepl("(modify_Observation)|(modify_Omega)|(modify_Theta)", includedFunctions)]
    includedFunctions <- includedFunctions[!grepl("gen_MAP", includedFunctions)]

    dump(
      includedFunctions,
      file = outputFile,
      envir = packageEnvironment,
      control = c("keepInteger", "warnIncomplete", "keepNA"),
      append = F
    )

    cat(
      c(
        "args <- commandArgs(TRUE)",
        "print_templateFromJson(args)",
        ""
      ),
      sep = "\n",
      file = outputFile,
      append = T
    )

  }
