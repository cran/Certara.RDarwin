test_that("Specific mapping of A1 when A1 is main absorption compartment (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_A1_main_mapped.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_A1_main_mapped.json")
            DataFilePath <-
              file.path(TestFolder, "data_A1_main_mapped.csv")

            # PMLParametersSets: 1-cpt, Gamma absorption.
            # Default main dosepoint for Gamma absorption is typically "A1".
            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "Gamma"
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_AMT_A1 = 100,
                # Column specifically for A1
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test specific A1 mapping when A1 is main",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  A1 = "DOSE_AMT_A1",
                  # Explicitly map dosepoint A1
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            testthat::expect_snapshot_value(output$Template$MAP, style = "json2")
          })

test_that("Specific A1 mapping takes precedence over AMT when A1 is main (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_A1_vs_AMT_main.txt")
            # TokensFilePath not strictly needed for map assertion here, but good for consistency
            TokensFilePath <-
              file.path(TestFolder, "tokens_A1_vs_AMT_main.json")
            DataFilePath <-
              file.path(TestFolder, "data_A1_vs_AMT_main.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "Gamma" # Main dosepoint is A1
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_FOR_A1_COL = 100,
                # Column for specific A1 mapping
                DOSE_FOR_AMT_COL = 200,
                # Column for generic AMT mapping
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            # We expect a warning because both A1 (main dosepoint) and AMT are mapped.
            # A1=DOSE_FOR_A1_COL should be used.
            output <- testthat::expect_warning(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test A1 vs AMT mapping when A1 is main",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  A1 = "DOSE_FOR_A1_COL",
                  # Specific mapping for A1
                  AMT = "DOSE_FOR_AMT_COL",
                  # Generic AMT mapping
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Cannot map A1 and reserved word AMT simultaneously. AMT mapping term will be ignored"
            )

            testthat::local_edition(3)
            # The MAP block should reflect A1=DOSE_FOR_A1_COL
            testthat::expect_snapshot_value(output$Template$MAP, style = "json2")
          })

test_that("AMT maps to main dosepoint 'Aa' when Aa not specifically mapped (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_AMT_maps_to_Aa.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_AMT_maps_to_Aa.json")
            DataFilePath <-
              file.path(TestFolder, "data_AMT_maps_to_Aa.csv")

            # PMLParametersSets: 1-cpt, First-Order absorption.
            # Default main dosepoint for First-Order is typically "Aa".
            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order"
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_FOR_MAIN_COMP = 150,
                # Column for generic AMT mapping
                CONC_OBS = 25
              ),
              DataFilePath,
              row.names = FALSE
            )

            # No warning expected here regarding AMT, as Aa is not specifically mapped.
            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test AMT maps to main (Aa) when Aa not specified",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "DOSE_FOR_MAIN_COMP",
                  # Generic AMT mapping
                  CObs = "CONC_OBS"
                  # Note: No 'Aa = "..."' mapping
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            # The MAP block should reflect Aa=DOSE_FOR_MAIN_COMP
            testthat::expect_snapshot_value(output$Template$MAP, style = "json2")
          })

test_that("Secondary dosepoint A1 mapped specifically, AMT maps to main Aa (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_secondary_A1_mapped.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_secondary_A1_mapped.json")
            DataFilePath <-
              file.path(TestFolder, "data_secondary_A1_mapped.csv")

            # PMLParametersSets: 1-cpt, First-Order absorption (main is Aa).
            # Add A1 as a secondary dosepoint.
            baseModel <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order"
              )

            modelPMLCodes <- add_Dosepoint(
              PMLParametersSets = baseModel,
              DosepointName = "A1",
              # Adding A1 as a secondary dosepoint
              # tlag, bioavail, etc., can be default or minimal for this mapping test
              tlag = Expression("TlagA1", ContainedStParms = StParm("TlagA1"))
            )
            # At this point, modelPMLCodes[[1]]$MainDosepoint should be Aa
            # and modelPMLCodes[[1]]$SecondaryDosepoints should contain A1

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_FOR_MAIN_AA = 150,
                # Column for AMT (maps to Aa)
                DOSE_FOR_SECONDARY_A1 = 50,
                # Column for specific A1 mapping
                CONC_OBS = 25
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test mapping secondary A1 and AMT to main Aa",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "DOSE_FOR_MAIN_AA",
                  # Maps to main compartment (Aa)
                  A1 = "DOSE_FOR_SECONDARY_A1",
                  # Maps to secondary dosepoint A1
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            # The MAP block should reflect both Aa=DOSE_FOR_MAIN_AA and A1=DOSE_FOR_SECONDARY_A1
            testthat::expect_snapshot_value(output$Template$MAP, style = "json2")
          })

test_that("AMT and Specific A1/Ext mapping (Multiple Models, different mains)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_multi_model_diff_mains.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_multi_model_diff_mains.json")
            DataFilePath <-
              file.path(TestFolder, "data_multi_model_diff_mains.csv")

            # Model 1: 1-cpt, First-Order (main "Aa")
            model1 <- get_PMLParametersSets(
              CompartmentsNumber = 1,
              Parameterization = "Clearance",
              Absorption = "First-Order"
            )
            names(model1) <-
              "PK1FOC_Aa" # Explicit naming for clarity in tokens

            # Add a secondary dosepoint "Ext" to Model 1
            model1_updated <- suppressWarnings(
              add_Dosepoint(
                PMLParametersSets = model1,
                DosepointName = "Ext",
                tlag = Expression("TlagExt", ContainedStParms = StParm("TlagExt")),
                PMLStructures = "PK1FOC_Aa" # Apply to the correct model structure
              )
            )

            # Model 2: 1-cpt, Gamma (main "A1")
            model2 <- get_PMLParametersSets(
              CompartmentsNumber = 1,
              Parameterization = "Clearance",
              Absorption = "Gamma"
            )
            names(model2) <- "PK1GC_A1" # Explicit naming

            modelPMLCodes <- add_Spaces(model1_updated, model2)

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_MAIN_COL = 200,
                # For AMT
                DOSE_A1_SPECIFIC_COL = 100,
                # For specific A1 mapping
                DOSE_EXT_COL = 50,
                # For specific Ext mapping
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            # Expect a warning for Model 2 (PK1GC_A1) because its main dosepoint "A1"
            # is specifically mapped AND AMT is also present.
            output <- testthat::expect_warning(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test AMT and specific A1/Ext with multiple models",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "DOSE_MAIN_COL",
                  # Generic AMT
                  A1 = "DOSE_A1_SPECIFIC_COL",
                  # Specific mapping for A1
                  Ext = "DOSE_EXT_COL",
                  # Specific mapping for Ext
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              # Regex for the warning related to PK1GC_A1 (where A1 is main)
              regexp = "Cannot map A1 and reserved word AMT simultaneously. AMT mapping term will be ignored  for PK1GC_A1"
            )

            testthat::local_edition(3)
            testthat::expect_snapshot_output(output$Template$MAP)
            # Snapshot the TokensList. This will show the model-specific map segments.
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Generic 'Rate' maps to MainDosepoint_Rate when AMT is used (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_AMT_generic_Rate.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_AMT_generic_Rate.json")
            DataFilePath <-
              file.path(TestFolder, "data_AMT_generic_Rate.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order" # Main dosepoint is Aa
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_MAIN = 1000,
                INF_RATE = 50,
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test generic Rate with AMT mapping",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "DOSE_MAIN",
                  # Maps to Aa
                  Rate = "INF_RATE",
                  # Should map to Aa_Rate
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            # Expect Aa_Rate=INF_RATE in the map statement
            testthat::expect_snapshot_output(output$Template$MAP)
          })

test_that("Generic 'Duration' maps to MainDosepoint_Duration when AMT is used (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_AMT_generic_Duration.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_AMT_generic_Duration.json")
            DataFilePath <-
              file.path(TestFolder, "data_AMT_generic_Duration.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order" # Main dosepoint is Aa
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_MAIN = 1000,
                INF_DUR = 2,
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test generic Duration with AMT mapping",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "DOSE_MAIN",
                  # Maps to Aa
                  Duration = "INF_DUR",
                  # Should map to Aa_Duration
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            # Expect Aa_Duration=INF_DUR in the map statement
            testthat::expect_snapshot_output(output$Template$MAP)
          })

test_that("Error when both generic 'Rate' and 'Duration' are mapped with AMT (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_AMT_generic_RateDur_err.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_AMT_generic_RateDur_err.json")
            DataFilePath <-
              file.path(TestFolder, "data_AMT_generic_RateDur_err.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order" # Main dosepoint is Aa
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_MAIN = 1000,
                INF_RATE = 50,
                INF_DUR = 2,
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            testthat::expect_error(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test error for generic Rate and Duration with AMT",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "DOSE_MAIN",
                  Rate = "INF_RATE",
                  Duration = "INF_DUR",
                  # This combination should cause an error
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Cannot map Rate and Duration simultaneously"
            )
          })

test_that("Specific 'A1_Rate' maps correctly when A1 is main (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_A1_specific_Rate.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_A1_specific_Rate.json")
            DataFilePath <-
              file.path(TestFolder, "data_A1_specific_Rate.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "Gamma" # Main dosepoint is A1
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_A1 = 1000,
                RATE_FOR_A1 = 50,
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test specific A1_Rate mapping",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  A1 = "DOSE_A1",
                  # Maps to main dosepoint A1
                  A1_Rate = "RATE_FOR_A1",
                  # Specific rate for A1
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            # Expect A1_Rate=RATE_FOR_A1 in the map statement
            testthat::expect_snapshot_output(output$Template$MAP)
          })

test_that("Specific 'A1_Duration' maps correctly when A1 is main (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_A1_specific_Duration.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_A1_specific_Duration.json")
            DataFilePath <-
              file.path(TestFolder, "data_A1_specific_Duration.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "Gamma" # Main dosepoint is A1
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_A1 = 1000,
                DUR_FOR_A1 = 2,
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test specific A1_Duration mapping",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  A1 = "DOSE_A1",
                  # Maps to main dosepoint A1
                  A1_Duration = "DUR_FOR_A1",
                  # Specific duration for A1
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              )

            testthat::local_edition(3)
            # Expect A1_Duration=DUR_FOR_A1 in the map statement
            testthat::expect_snapshot_output(output$Template$MAP)
          })

test_that("Error when both 'A1_Rate' and 'A1_Duration' are mapped (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_A1_specific_RateDur_err.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_A1_specific_RateDur_err.json")
            DataFilePath <-
              file.path(TestFolder, "data_A1_specific_RateDur_err.csv")

            modelPMLCodes <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "Gamma" # Main dosepoint is A1
              )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_A1 = 1000,
                RATE_FOR_A1 = 50,
                DUR_FOR_A1 = 2,
                CONC_OBS = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            testthat::expect_error(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test error for specific A1_Rate and A1_Duration",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  A1 = "DOSE_A1",
                  A1_Rate = "RATE_FOR_A1",
                  A1_Duration = "DUR_FOR_A1",
                  # This combination should cause an error
                  CObs = "CONC_OBS"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              # The regexp should match the error from .map_RateDuration
              regexp = "Cannot map A1_Rate and A1_Duration simultaneously"
            )
          })

test_that("Mixed generic/specific Rate/Duration with multiple models and Ext mapped",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder,
                        "template_multi_mixed_rate_dur_ext_mapped.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_multi_mixed_rate_dur_ext_mapped.json")
            DataFilePath <-
              file.path(TestFolder, "data_multi_mixed_rate_dur_ext_mapped.csv")

            # Model 1: Main "Aa", Secondary "Ext"
            model1_base <-
              get_PMLParametersSets(CompartmentsNumber = 1, Absorption = "First-Order")
            names(model1_base) <- "M1_Aa_Main"
            model1_updated <-
              suppressWarnings(
                add_Dosepoint(
                  PMLParametersSets = model1_base,
                  DosepointName = "Ext",
                  # This is our secondary dosepoint
                  tlag = Expression("TlagExt", ContainedStParms = StParm("TlagExt")),
                  PMLStructures = "M1_Aa_Main"
                )
              )

            # Model 2: Main "A1"
            model2 <-
              get_PMLParametersSets(CompartmentsNumber = 1, Absorption = "Gamma")
            names(model2) <- "M2_A1_Main"

            modelPMLCodes <- add_Spaces(model1_updated, model2)

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                MAIN_DOSE_COL = 200,
                # For AMT
                EXT_DOSE_COL = 75,
                # Explicit dose for Ext
                GEN_RATE_COL = 20,
                A1_SPEC_DUR_COL = 2,
                EXT_SPEC_RATE_COL = 5,
                CONC_VAL = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            # With 'Ext' explicitly mapped for its dose, the previous "Cannot map Ext" message
            # for its primary dose should disappear.
            # We still expect the message for M2_A1_Main regarding A1_Duration, because
            # specific A1_Duration should be used instead of generic Rate if AMT also maps to A1.
            # However, gen_MAP logic regarding AMT and explicit dosepoint mapping conflict
            # might also trigger a warning for M2_A1_Main if AMT maps to A1 and A1 is also explicitly mapped.
            # Let's check the `gen_MAP` logic for the warning:
            # It warns if MainDosepoint (A1 for M2) is in MappingNames AND AMT is in MappingNames.
            # Our DataMapping will be c(..., AMT="MAIN_DOSE_COL", A1="???" ...)
            # If A1 is not in DataMapping for its *dose*, then AMT maps to A1, and then A1_Duration is handled. No AMT conflict.
            # If A1 *is* in DataMapping for its dose (e.g. A1="A1_DOSE_COL"), then AMT and A1 conflict.
            # For this test, let's assume AMT maps to A1 in M2 and A1 is not separately dose-mapped.

            # The message "A1_Rate or A1_Duration found in mapping for M2_A1_Main" is expected
            # because A1_Duration is being applied.
            output <- testthat::expect_message(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test mixed Rate/Duration, Ext mapped, multiple models",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "SUBJ_ID",
                  time = "TIME_VAL",
                  AMT = "MAIN_DOSE_COL",
                  # Generic dose for main compartments
                  Ext = "EXT_DOSE_COL",
                  # Explicit dose mapping for Ext
                  Rate = "GEN_RATE_COL",
                  # Generic Rate
                  A1_Duration = "A1_SPEC_DUR_COL",
                  # Specific Duration for A1
                  Ext_Rate = "EXT_SPEC_RATE_COL",
                  # Specific Rate for Ext
                  CObs = "CONC_VAL"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "A1_Rate or A1_Duration found in mapping for M2_A1_Main"
            )


            testthat::local_edition(3)
            testthat::expect_snapshot_output(output$Template$MAP)
            testthat::expect_snapshot_value(output$TokensList, style = "json2")
          })

test_that("Unmapped covariate 'WT' is auto-mapped if data column exists (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_auto_map_cov_WT.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_auto_map_cov_WT.json")
            DataFilePath <-
              file.path(TestFolder, "data_auto_map_cov_WT.csv")

            baseModel <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order"
              )

            modelPMLCodes <- add_Covariate(
              PMLParametersSets = baseModel,
              Name = "WT",
              # Covariate name
              StParmNames = "Cl",
              Type = "Continuous",
              State = "Present"
            )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_COL = 100,
                CONC_VAL = 10,
                WT = 70 # Data column matching the covariate name
              ),
              DataFilePath,
              row.names = FALSE
            )

            # We expect messages/warnings, but the critical outcome is the MAP statement.
            # The function should not error out.
            output <-
              # We can still expect messages if we want to be more thorough
              testthat::expect_message(
                # Expect the "Covariates are mapped from column names"
                testthat::expect_warning(
                  # Expect the "Some covariate(s) are not mapped"
                  write_ModelTemplateTokens(
                    TemplateFilePath = TemplateFilePath,
                    TokensFilePath = TokensFilePath,
                    Description = "Test auto-mapping of unmapped covariate WT",
                    Author = "Test Suite",
                    DataFilePath = DataFilePath,
                    DataMapping = c(
                      id = "SUBJ_ID",
                      time = "TIME_VAL",
                      AMT = "DOSE_COL",
                      CObs = "CONC_VAL"
                      # WT is deliberately omitted here
                    ),
                    PMLParametersSets = modelPMLCodes,
                    EstArgs = ""
                  ),
                  regexp = "Some covariate\\(s\\) are not mapped: WT"
                ),
                regexp = "Covariates are mapped from column names: WT"
              )


            testthat::local_edition(3)
            # The MAP block should include WT=WT due to auto-mapping
            testthat::expect_snapshot_output(output$Template$MAP)
          })

test_that("Error if unmapped covariate 'WT' not in data columns (Single Model)",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_err_unmapped_cov_WT.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_err_unmapped_cov_WT.json")
            DataFilePath <-
              file.path(TestFolder, "data_err_unmapped_cov_WT.csv")

            baseModel <-
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance",
                Absorption = "First-Order"
              )

            modelPMLCodes <- add_Covariate(
              PMLParametersSets = baseModel,
              Name = "WT",
              # Covariate name
              StParmNames = "Cl",
              Type = "Continuous",
              State = "Present"
            )

            write.csv(
              data.frame(
                SUBJ_ID = "S1",
                TIME_VAL = 0,
                DOSE_COL = 100,
                CONC_VAL = 10
                # Column "WT" is deliberately missing
              ),
              DataFilePath,
              row.names = FALSE
            )

            # We expect a warning that "WT" is not mapped, THEN an error because it's not in the data.
            # To test this sequence cleanly:
            # 1. Wrap the call in expect_warning for the initial warning.
            # 2. Wrap that entire expect_warning call in expect_error for the subsequent stop.
            testthat::expect_error(
              testthat::expect_warning(
                write_ModelTemplateTokens(
                  TemplateFilePath = TemplateFilePath,
                  TokensFilePath = TokensFilePath,
                  Description = "Test error for unmapped covariate WT not in data",
                  Author = "Test Suite",
                  DataFilePath = DataFilePath,
                  DataMapping = c(
                    id = "SUBJ_ID",
                    time = "TIME_VAL",
                    AMT = "DOSE_COL",
                    CObs = "CONC_VAL"
                    # WT is deliberately omitted from DataMapping
                  ),
                  PMLParametersSets = modelPMLCodes,
                  EstArgs = ""
                ),
                regexp = "Some covariate\\(s\\) are not mapped: WT" # The warning
              ),
              regexp = "Cannot map covariate\\(s\\): WT" # The error
            )
          })

test_that("gen_MAP stops if DataMapping values contain NA", {
  TestFolder <- tempdir()
  TemplateFilePath <-
    file.path(TestFolder, "template_map_val_na.txt")
  TokensFilePath <- file.path(TestFolder, "tokens_map_val_na.json")
  DataFilePath <- file.path(TestFolder, "data_map_val_na.csv")

  modelPMLCodes <- get_PMLParametersSets(CompartmentsNumber = 1)

  write.csv(
    data.frame(
      ID_COL = "S1",
      TIME_COL = 0,
      DOSE_COL = 100,
      CONC_COL = 10
    ),
    DataFilePath,
    row.names = FALSE
  )

  testthat::expect_error(
    write_ModelTemplateTokens(
      TemplateFilePath = TemplateFilePath,
      TokensFilePath = TokensFilePath,
      Description = "Test NA in DataMapping value",
      Author = "Test Suite",
      DataFilePath = DataFilePath,
      DataMapping = c(id = "ID_COL",
                      time = "TIME_COL",
                      AMT = NA_character_),
      PMLParametersSets = modelPMLCodes,
      EstArgs = ""
    ),
    regexp = "all\\(!is.na\\(Mapping\\)\\) is not TRUE" # Exact error from stopifnot
  )
})

test_that("gen_MAP stops if DataMapping values contain empty string", {
  TestFolder <- tempdir()
  TemplateFilePath <-
    file.path(TestFolder, "template_map_val_empty.txt")
  TokensFilePath <-
    file.path(TestFolder, "tokens_map_val_empty.json")
  DataFilePath <- file.path(TestFolder, "data_map_val_empty.csv")

  modelPMLCodes <- get_PMLParametersSets(CompartmentsNumber = 1)

  write.csv(
    data.frame(
      ID_COL = "S1",
      TIME_COL = 0,
      DOSE_COL = 100,
      CONC_COL = 10
    ),
    DataFilePath,
    row.names = FALSE
  )

  testthat::expect_error(
    write_ModelTemplateTokens(
      TemplateFilePath = TemplateFilePath,
      TokensFilePath = TokensFilePath,
      Description = "Test empty string in DataMapping value",
      Author = "Test Suite",
      DataFilePath = DataFilePath,
      DataMapping = c(id = "ID_COL",
                      time = "TIME_COL",
                      AMT = ""),
      PMLParametersSets = modelPMLCodes,
      EstArgs = ""
    ),
    regexp = "all\\(nchar\\(Mapping\\) > 0\\) is not TRUE" # Exact error from stopifnot
  )
})

test_that("gen_MAP stops if empty name in DataMapping leads to duplicate names after fixup",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_map_name_empty_dupname.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_map_name_empty_dupname.json")
            DataFilePath <-
              file.path(TestFolder, "data_map_name_empty_dupname.csv")

            modelPMLCodes <-
              get_PMLParametersSets(CompartmentsNumber = 1)

            write.csv(
              data.frame(
                ID_COL = "S1",
                OTHER_ID_COL = "S2",
                TIME_COL = 0,
                DOSE_COL = 100,
                CONC_COL = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            testthat::expect_error(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test empty name in DataMapping leading to duplicate name",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c("ID_COL",             # Unnamed, name will become "ID_COL"
                                ID_COL = "OTHER_ID_COL", # This will conflict with the fixed-up name above
                                AMT = "DOSE_COL"),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Duplicated column names in mapping: ID_COL"
            )
          })

test_that("gen_MAP stops if empty name in DataMapping leads to duplicate values after fixup",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_map_name_empty_dupval.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_map_name_empty_dupval.json")
            DataFilePath <-
              file.path(TestFolder, "data_map_name_empty_dupval.csv")

            modelPMLCodes <-
              get_PMLParametersSets(CompartmentsNumber = 1)

            write.csv(
              data.frame(
                ID_COL = "S1",
                TIME_COL = 0,
                DOSE_COL = 100,
                CONC_COL = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            testthat::expect_error(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test empty name in DataMapping leading to duplicate value",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c("DOSE_COL",          # Unnamed, name will become "DOSE_COL", value is "DOSE_COL"
                                time = "TIME_COL",
                                AMT = "DOSE_COL"),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Duplicated model terms in mapping: DOSE_COL" # "model terms" refers to the values of Mapping
            )
          })

test_that("gen_MAP stops if NA name in DataMapping leads to duplicate names after fixup",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_map_name_na_dupname.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_map_name_na_dupname.json")
            DataFilePath <-
              file.path(TestFolder, "data_map_name_na_dupname.csv")

            modelPMLCodes <-
              get_PMLParametersSets(CompartmentsNumber = 1)

            write.csv(
              data.frame(
                ID_COL = "S1",
                OTHER_ID_COL = "S2",
                TIME_COL = 0,
                DOSE_COL = 100,
                CONC_COL = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            mapping_with_na_name <-
              c("ID_COL", ID_COL = "OTHER_ID_COL", AMT = "DOSE_COL")
            names(mapping_with_na_name)[1] <-
              NA_character_ # Set the first name to NA

            testthat::expect_error(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test NA name in DataMapping leading to duplicate name",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = mapping_with_na_name,
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Duplicated column names in mapping: ID_COL"
            )
          })

test_that("gen_MAP stops if mapped data column (with category syntax) does not exist",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_map_catcol_not_exist.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_map_catcol_not_exist.json")
            DataFilePath <-
              file.path(TestFolder, "data_map_catcol_not_exist.csv")

            # Model with a categorical covariate "GRP"
            baseModel <-
              get_PMLParametersSets(CompartmentsNumber = 1)
            modelPMLCodes <- add_Covariate(
              baseModel,
              Name = "GRP",
              StParmNames = "Cl",
              Type = "Categorical",
              Categories = c(0, 1) # Unnamed categories
            )


            # Data file is missing "NON_EXISTENT_GROUP_COL"
            write.csv(
              data.frame(
                ID_COL = "S1",
                TIME_COL = 0,
                DOSE_COL = 100,
                CONC_COL = 10
              ),
              DataFilePath,
              row.names = FALSE
            )

            testthat::expect_error(
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test mapped category data column not in CSV",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "ID_COL",
                  time = "TIME_COL",
                  AMT = "DOSE_COL",
                  CObs = "CONC_COL",
                  # GRP maps to a column defined with category syntax, but the base column name doesn't exist
                  GRP = "NON_EXISTENT_GROUP_COL(A=0, B=1)"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              # The error message should show the full mapping string that failed
              regexp = "Following columns presented in mapping, but not in the dataset:NON_EXISTENT_GROUP_COL\\(A=0, B=1\\)"
            )
          })

test_that("gen_MAP stops if a covariate has multiple types defined", {
  TestFolder <- tempdir()
  TemplateFilePath <-
    file.path(TestFolder, "template_cov_multitype.txt")
  TokensFilePath <-
    file.path(TestFolder, "tokens_cov_multitype.json")
  DataFilePath <-
    file.path(TestFolder, "data_cov_multitype.csv")

  # Base model
  modelPMLCodes <-
    get_PMLParametersSets(CompartmentsNumber = 1)

  # Add "STATUS" as Categorical on Cl
  modelPMLCodes <- add_Covariate(
    PMLParametersSets = modelPMLCodes,
    Name = "STATUS",
    StParmNames = "Cl",
    Type = "Categorical",
    Categories = c(0, 1),
    # Example categories
    State = "Present"
  )

  # Add "STATUS" again, but as Continuous on V
  modelPMLCodes <- add_Covariate(
    PMLParametersSets = modelPMLCodes,
    Name = "STATUS",
    StParmNames = "V",
    Type = "Continuous",
    # Different type for the same covariate name
    State = "Present"
  )

  write.csv(
    data.frame(
      ID_COL = "S1",
      TIME_COL = 0,
      DOSE_COL = 100,
      CONC_COL = 10,
      STATUS_COL = 0 # Data for the covariate
    ),
    DataFilePath,
    row.names = FALSE
  )

  testthat::expect_error(
    write_ModelTemplateTokens(
      TemplateFilePath = TemplateFilePath,
      TokensFilePath = TokensFilePath,
      Description = "Test warning for covariate with multiple types",
      Author = "Test Suite",
      DataFilePath = DataFilePath,
      DataMapping = c(
        id = "ID_COL",
        time = "TIME_COL",
        AMT = "DOSE_COL",
        CObs = "CONC_COL",
        STATUS = "STATUS_COL" # Map the covariate
      ),
      PMLParametersSets = modelPMLCodes,
      EstArgs = ""
    ),
    regexp = "Covariate\\(s\\) with multiple types: STATUS"
  )
})

test_that("Cat Cov (Named) generates correct MAP block", {
  TestFolder <- tempdir()

  pml_base <-
    get_PMLParametersSets(CompartmentsNumber = 2,
                          Parameterization = "Clearance")
  pml_with_cl_cov <-
    add_Covariate(
      pml_base,
      Name = "Sex",
      StParmNames = "Cl",
      Type = "Categorical",
      State = "Searched",
      Categories = c(Male = 0, Female = 1)
    )
  modelPMLCodes <-
    add_Covariate(
      pml_with_cl_cov,
      Name = "Sex",
      StParmNames = "V",
      Type = "Categorical",
      State = "Searched",
      Categories = c(Male = 0, Female = 1)
    )

  TemplateFilePath <-
    file.path(TestFolder, "template_cat_named_data_nopipe.txt")
  TokensFilePath <-
    file.path(TestFolder, "tokens_cat_named_data_nopipe.json")
  DataFilePath <-
    file.path(TestFolder, "data_cat_named_data_nopipe.csv")

  write.csv(
    data.frame(
      Subject = "S1",
      Act_Time = "0",
      Amount = "100",
      Conc = "5",
      Gender = "Male"
    ),
    DataFilePath,
    row.names = FALSE
  )

  output <-
    write_ModelTemplateTokens(
      TemplateFilePath = TemplateFilePath,
      TokensFilePath = TokensFilePath,
      Description = "Test Cat Named DATA block",
      Author = "Certara",
      DataFilePath = DataFilePath,
      DataMapping = c(
        id = "Subject",
        time = "Act_Time",
        A1 = "Amount",
        CObs = "Conc",
        Sex = "Gender" # gen_MAP creates: map(Sex=Gender)
      ),
      PMLParametersSets = modelPMLCodes,
      EstArgs = "ODE = 'MatrixExponent'"
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_output(output$Template$MAP)
})


test_that("Cat Cov (Unnamed/Map Def) generates correct MAP block",
          {
            TestFolder <- tempdir()

            pml_base <-
              get_PMLParametersSets(CompartmentsNumber = 2,
                                    Parameterization = "Clearance")

            pml_with_cl_cov <-
              add_Covariate(
                pml_base,
                Name = "Sex",
                StParmNames = "Cl",
                Type = "Categorical",
                State = "Searched",
                Categories = c(0, 1)
              )

            modelPMLCodes <-
              add_Covariate(
                pml_with_cl_cov,
                Name = "Sex",
                StParmNames = "V",
                Type = "Categorical",
                State = "Searched",
                Categories = c(0, 1)
              )

            TemplateFilePath <-
              file.path(TestFolder, "template_cat_unnamed_data_nopipe.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_cat_unnamed_data_nopipe.json")
            DataFilePath <-
              file.path(TestFolder, "data_cat_unnamed_data_nopipe.csv")

            write.csv(
              data.frame(
                Subject = "S1",
                Act_Time = "0",
                Amount = "100",
                Conc = "5",
                Gender = "female"
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test Cat Unnamed/Map Def DATA block",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "Subject",
                  time = "Act_Time",
                  A1 = "Amount",
                  CObs = "Conc",
                  Sex = "Gender(female = 0, male = 1)"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = "ODE = 'MatrixExponent'"
              )

            testthat::local_edition(3)
            testthat::expect_snapshot_output(output$Template$MAP)
          })


test_that("Occ Cov (Named) generates correct MAP block", {
  TestFolder <- tempdir()

  modelPMLCodes <- add_Covariate(
    get_PMLParametersSets(
      CompartmentsNumber = 1,
      Parameterization = "Clearance"
    ),
    Name = "Period",
    StParmNames = "Cl",
    Type = "Occasion",
    State = "Searched",
    Categories = c(Period1 = 1, Period2 = 2)
  )

  TemplateFilePath <-
    file.path(TestFolder, "template_occ_named_data_nopipe.txt")
  TokensFilePath <-
    file.path(TestFolder, "tokens_occ_named_data_nopipe.json")
  DataFilePath <-
    file.path(TestFolder, "data_occ_named_data_nopipe.csv")

  write.csv(
    data.frame(
      ID = "ID1",
      time = "0",
      Dose = "100",
      CObs = "10",
      StudyPeriod = 1
    ),
    DataFilePath,
    row.names = FALSE
  )

  output <-
    write_ModelTemplateTokens(
      TemplateFilePath = TemplateFilePath,
      TokensFilePath = TokensFilePath,
      Description = "Test Occ Named DATA block",
      Author = "Certara",
      DataFilePath = DataFilePath,
      DataMapping = c(
        id = "ID",
        time = "time",
        AMT = "Dose",
        CObs = "CObs",
        Period = "StudyPeriod" # gen_MAP creates: map(Period=StudyPeriod)
      ),
      PMLParametersSets = modelPMLCodes,
      EstArgs = "ODE = 'MatrixExponent'"
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_output(output$Template$MAP)
})

test_that("Occ Cov (Unnamed/Map Def) generates correct MAP block",
          {
            TestFolder <- tempdir()

            modelPMLCodes <- add_Covariate(
              get_PMLParametersSets(
                CompartmentsNumber = 1,
                Parameterization = "Clearance"
              ),
              Name = "Period",
              StParmNames = "Cl",
              Type = "Occasion",
              State = "Searched",
              Categories = c(1, 2)
            )

            TemplateFilePath <-
              file.path(TestFolder, "template_occ_unnamed_data_nopipe.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_occ_unnamed_data_nopipe.json")
            DataFilePath <-
              file.path(TestFolder, "data_occ_unnamed_data_nopipe.csv")

            write.csv(
              data.frame(
                ID = "ID1",
                time = "0",
                Dose = "100",
                CObs = "10",
                StudyPeriod = "P1"
              ),
              DataFilePath,
              row.names = FALSE
            )

            output <-
              write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test Occ Unnamed/Map Def DATA block",
                Author = "Certara",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "ID",
                  time = "time",
                  AMT = "Dose",
                  CObs = "CObs",
                  # gen_MAP parses this special syntax:
                  Period = "StudyPeriod(P1=1, P2=2)"
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = "ODE = 'MatrixExponent'"
              )

            testthat::local_edition(3)
            testthat::expect_snapshot_output(output$Template$MAP)
          })


test_that("Continuous Cov and IGNORE generate correct MAP block", {
  TestFolder <- tempdir()

  # Add a continuous covariate 'Age' affecting 'Cl'
  modelPMLCodes <- add_Covariate(
    get_PMLParametersSets(
      CompartmentsNumber = 1,
      Parameterization = "Clearance"
    ),
    Name = "Age",
    StParmNames = "Cl",
    Type = "Continuous",
    State = "Searched"
  )

  TemplateFilePath <-
    file.path(TestFolder, "template_cont_ign_data_nopipe.txt")
  TokensFilePath <-
    file.path(TestFolder, "tokens_cont_ign_data_nopipe.json")
  DataFilePath <-
    file.path(TestFolder, "data_cont_ign_data_nopipe.csv")

  write.csv(
    data.frame(
      ID = "ID1",
      time = "0",
      Dose = "100",
      CObs = "10",
      SubjectAge = 35,
      UnusedColumn = "XYZ"
    ),
    DataFilePath,
    row.names = FALSE
  )

  output <-
    write_ModelTemplateTokens(
      TemplateFilePath = TemplateFilePath,
      TokensFilePath = TokensFilePath,
      Description = "Test Continuous/IGNORE DATA block",
      Author = "Certara",
      DataFilePath = DataFilePath,
      DataMapping = c(
        id = "ID",
        time = "time",
        AMT = "Dose",
        CObs = "CObs",
        Age = "SubjectAge",
        # gen_MAP creates: map(Age=SubjectAge)
        IGNORE = "UnusedColumn" # gen_MAP creates: map(IGNORE=UnusedColumn)
      ),
      PMLParametersSets = modelPMLCodes,
      EstArgs = "ODE = 'MatrixExponent'"
    )

  testthat::local_edition(3)
  testthat::expect_snapshot_output(output$Template$MAP)
})

test_that("gen_MAP warns when model's named categories are overridden by mapping definition",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_cat_map_override.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_cat_map_override.json")
            DataFilePath <-
              file.path(TestFolder, "data_cat_map_override.csv")

            modelPMLCodes <- add_Covariate(
              get_PMLParametersSets(CompartmentsNumber = 1),
              Name = "Sex",
              StParmNames = "Cl",
              Type = "Categorical",
              Categories = c(Male = 0, Female = 1),
              # Named categories in model
              State = "Present"
            )

            write.csv(data.frame(
              ID = "S1",
              TIME = 0,
              AMT = 100,
              DV = 10,
              Gender = "M"
            ),
            DataFilePath,
            row.names = FALSE)

            testthat::expect_warning(
              output <- write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test cat cov mapping overrides model names",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "ID",
                  time = "TIME",
                  AMT = "AMT",
                  CObs = "DV",
                  Sex = "Gender(M=0, F=1)" # Mapping provides its own categories
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Categories for covariate Sex are given in the mapping and won't be applied from the categories names"
            )

            testthat::local_edition(3)
            # The map should use Gender(M=0, F=1) from DataMapping
            testthat::expect_snapshot_output(output$Template$MAP)
          })

test_that("gen_MAP warns when model's named categories are overridden by mapping definition",
          {
            TestFolder <- tempdir()
            TemplateFilePath <-
              file.path(TestFolder, "template_cat_map_override.txt")
            TokensFilePath <-
              file.path(TestFolder, "tokens_cat_map_override.json")
            DataFilePath <-
              file.path(TestFolder, "data_cat_map_override.csv")

            modelPMLCodes <- add_Covariate(
              get_PMLParametersSets(CompartmentsNumber = 1),
              Name = "Sex",
              StParmNames = "Cl",
              Type = "Categorical",
              Categories = c(Male = 0, Female = 1),
              # Named categories in model
              State = "Present"
            )

            write.csv(
              data.frame(
                ID = "S1",
                TIME = 0,
                AMT_COL = 100,
                DV_COL = 10,
                Gender_COL = "M"
              ),
              # Changed data column names slightly
              DataFilePath,
              row.names = FALSE
            )

            # Expect warning because model has named categories for Sex, but mapping also defines categories for Sex.
            testthat::expect_warning(
              output <- write_ModelTemplateTokens(
                TemplateFilePath = TemplateFilePath,
                TokensFilePath = TokensFilePath,
                Description = "Test cat cov mapping overrides model names",
                Author = "Test Suite",
                DataFilePath = DataFilePath,
                DataMapping = c(
                  id = "ID",
                  time = "TIME",
                  AMT = "AMT_COL",
                  CObs = "DV_COL",
                  Sex = "Gender_COL(M=0, F=1)" # Mapping provides its own categories
                ),
                PMLParametersSets = modelPMLCodes,
                EstArgs = ""
              ),
              regexp = "Categories for covariate Sex are given in the mapping and won't be applied from the categories names"
            )

            testthat::local_edition(3)
            # The map should use Gender_COL(M=0, F=1) from DataMapping
            testthat::expect_snapshot_output(output$Template$MAP)
          })
