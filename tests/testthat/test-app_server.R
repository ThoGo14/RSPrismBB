test_that("app_server initializes translator", {
  testServer(app_server, {
    # Test that translator is initialized
    expect_true(exists("translator"))
    expect_true(is.function(tl))
  })
})

test_that("translation function tl() works", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    
    # Test that tl function returns strings
    result <- tl("app_title")
    expect_type(result, "character")
    expect_true(nchar(result) > 0)
  })
})

test_that("language switching works", {
  testServer(app_server, {
    # Set initial language
    session$setInputs(selected_language = "de")
    app_title_de <- tl("app_title")

    # Switch to English
    session$setInputs(selected_language = "en")
    app_title_en <- tl("app_title")
    
    expect_match(app_title_de, "und")
    expect_match(app_title_en, "and")
    expect_false(app_title_de == app_title_en)
    
  })
})

test_that("download options header renders", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    expect_type(output$download_options_header, "character")
  })
})

test_that("data table title renders", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    expect_type(output$data_table_title_out, "character")
  })
})

test_that("info box texts render", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    expect_type(output$info_box_title_out, "character")
    expect_type(output$info_box_text_1_out, "character")
    expect_type(output$info_box_text_2_out, "character")
  })
})

test_that("version number renders correctly", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    version <- output$version_number
    expect_type(version, "character")
    expect_match(version, "Version")
  })
})


test_that("FunctionColcutNA handles data correctly", {
  testServer(app_server, {
    # Create test dataframe with numeric values
    test_df <- data.frame(
      A = c("Group", "A", "B"),
      B = c("Value1", "10", "20"),
      C = c("Value2", "30", "40"),
      stringsAsFactors = FALSE
    )
    
    session$setInputs(colcut = 1)
    
    result <- FunctionColcutNA(test_df)
    
    # Check that result is a dataframe
    expect_s3_class(result, "data.frame")
    
    # Check that headers are used as column names
    expect_true("Group" %in% colnames(result))
  })
})

test_that("FunctionColcutNA handles NA in headers", {
  testServer(app_server, {
    test_df <- data.frame(
      A = c("Group", "A"),
      B = c(NA, "5"),
      C = c("Value", "10"),
      stringsAsFactors = FALSE
    )
    
    session$setInputs(colcut = 1)
    
    result <- FunctionColcutNA(test_df)

    expect_s3_class(result, "data.frame")
    expect_true("NA" %in% colnames(result))
  })
})

test_that("FunctionColcutNA converts numeric columns with NA", {
  testServer(app_server, {
    test_df <- data.frame(
      Group = c("ColName", "A", "B"),
      Value1 = c("Value", "10", "20"),
      Value2 = c("Value2", "NA", "40"),
      stringsAsFactors = FALSE
    )
    
    session$setInputs(colcut = 1)
    
    # Expect warning when converting character to numeric
    expect_warning(
      result <- FunctionColcutNA(test_df),
      "NAs introduced by coercion"
    )
    
    expect_s3_class(result, "data.frame")
    # Check that numeric columns are converted
    expect_type(result[[2]], "double")
    # Check that character values were converted to NA
    expect_true(is.na(result$Value2[1]))
  })
})

test_that("color palette title renders", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    expect_type(output$color_palette_title_out, "character")
  })
})

test_that("group selection title renders", {
  testServer(app_server, {
    session$setInputs(selected_language = "de")
    expect_type(output$group_selection_title_out, "character")
  })
})


