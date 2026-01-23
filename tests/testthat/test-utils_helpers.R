library(RSPrismBB)

# Testing standard deviation of sample number not population number
x1 <- 1:5
x2 <- c(10, 20, 30)
x3 <- c(5, NA, 15, 25)
x4 <- 1:2
x5 <- c(10, 20)

test_that("Standard deviation of N - basic calculations", {
  expect_equal(sd_n(x1), sd(x1) * sqrt(4/5))
  expect_equal(sd_n(x2), sd(x2) * sqrt(2/3))
  expect_equal(sd_n(x3), sd(na.exclude(x3)) * sqrt(2/3))
  expect_equal(sd_n(x4), 0.5)
  expect_equal(sd_n(x5), 5)
})

test_that("Standard deviation of N - edge cases", {
  # Single value should return NA
  expect_true(is.na(sd_n(1)))
  
  # Empty vector should return NA
  expect_true(is.na(sd_n(numeric(0))))
  
  # All NAs should return NA
  expect_true(is.na(sd_n(c(NA, NA, NA))))
  
  # Vector with all same values should return 0
  expect_equal(sd_n(c(5, 5, 5, 5)), 0)
})

# Color Palette Tests
test_that("Color palette generation - basic functionality", {
  palette_func <- get_color_palette()
  expect_true(is.function(palette_func))
  colors <- palette_func(5)
  expect_equal(length(colors), 5)
})

test_that("Color palette generation - different sizes", {
  palette_func <- get_color_palette()
  
  # Test with 1 color
  colors1 <- palette_func(1)
  expect_equal(length(colors1), 1)
  expect_match(colors1[1], "^#[0-9A-F]{6}$")
  
  # Test with 10 colors
  colors10 <- palette_func(10)
  expect_equal(length(colors10), 10)
  
  # All colors should be valid hex codes
  expect_true(all(grepl("^#[0-9A-F]{6}$", colors10)))
})


# Translator Tests
test_that("Translator initialization - basic structure", {
  skip_if_not(file.exists("inst/translations.json") || 
                file.exists(system.file("translations.json", package = "RSPrismBB")),
              "translations.json not found")
  
  translator <- initialize_translator()
  
  # Check structure (environment instead of list)
  expect_true(is.environment(translator))
  expect_true("translations" %in% names(translator))
  expect_true("current_language" %in% names(translator))
  expect_true("set_translation_language" %in% names(translator))
  expect_true("tl" %in% names(translator))
  
  # Check default language
  expect_equal(translator$current_language, "de")
})

test_that("Translator - language switching", {
  skip_if_not(file.exists("inst/translations.json") || 
                file.exists(system.file("translations.json", package = "RSPrismBB")),
              "translations.json not found")
  
  translator <- initialize_translator()
  
  # Switch to English
  translator$set_translation_language("en")
  expect_equal(translator$current_language, "en")
  
  # Switch back to German
  translator$set_translation_language("de")
  expect_equal(translator$current_language, "de")
  
  # Invalid language should not change
  translator$set_translation_language("fr")
  expect_equal(translator$current_language, "de")
})

test_that("Translator - translation function", {
  skip_if_not(file.exists("inst/translations.json") || 
                file.exists(system.file("translations.json", package = "RSPrismBB")),
              "translations.json not found")
  
  translator <- initialize_translator()
  
  # Test translation function exists and returns something
  result <- translator$tl("download_options")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
  # Non-existent key should return the key itself
  expect_equal(translator$tl("non_existent_key_12345"), "non_existent_key_12345")
})

# Package Management Tests
test_that("Package loading - error handling", {
  # Test with a non-existent package
  # require() produces a warning AND stop() produces an error
  expect_error(
    expect_warning(
      paket_laden("not_a_real_package"),
      "no package called"
    ),
    "Paket konnte nicht geladen werden"
  )
})


