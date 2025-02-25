# gen_InputRow with RATE

    "$INPUT ID TIME AMT DV {RATE}"

# gen_InputRow with NA

    all(!is.na(DataMapping)) is not TRUE

# gen_InputRow with character(0)

    all(nchar(DataMapping) > 0) is not TRUE

# gen_InputRow with NULL

    all(!is.null(DataMapping)) is not TRUE

