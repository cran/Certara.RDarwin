test_that("gen_InputRow with RATE", {
  DataMappingNamed <- c(ID = "ID", TIME = "TIME", AMT = "AMT", DV = "DV", RATE = "RATE")
  DataMappingNoNamed <- c("ID", "TIME", "AMT", "DV", "RATE")
  expect_snapshot_value(gen_InputRow(DataMappingNamed))
  expect_equal(gen_InputRow(DataMappingNamed),
               gen_InputRow(DataMappingNoNamed))
})

test_that("gen_InputRow with NA", {
  DataMapping <- c(ID = NA, TIME = "TIME", AMT = "AMT", DV = "DV", RATE = "RATE")
  expect_snapshot_error(gen_InputRow(DataMapping))
})

test_that("gen_InputRow with character(0)", {
  DataMapping <- c(ID = "", TIME = "time", AMT = "A1", DV = "CObs", RATE = "R1")
  expect_snapshot_error(gen_InputRow(DataMapping))
})

test_that("gen_InputRow with NULL", {
  DataMapping <- NULL
  expect_snapshot_error(gen_InputRow(DataMapping))
})
