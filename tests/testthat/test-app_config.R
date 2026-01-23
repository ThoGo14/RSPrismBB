# App Configuration Tests
test_that("app_sys returns valid paths", {
  # Test root path
  root <- app_sys()
  expect_type(root, "character")
  
  # Test with existing file
  config_path <- app_sys("golem-config.yml")
  expect_type(config_path, "character")
  
  # Empty string for non-existent files is expected
  fake_path <- app_sys("not_a_file.xyz")
  expect_type(fake_path, "character")
})

test_that("get_golem_config returns values correctly", {
  config_file <- app_sys("golem-config.yml")
  skip_if(config_file == "", "Config file not found")
  
  # Test production config
  prod_value <- get_golem_config(
    "app_prod",
    config = "production",
    file = config_file
  )
  expect_type(prod_value, "logical")
  
  # Test dev config
  dev_value <- get_golem_config(
    "app_prod",
    config = "dev",
    file = config_file
  )
  expect_type(dev_value, "logical")
  
  # Production and dev should be different
  expect_false(identical(prod_value, dev_value))
})

test_that("get_golem_config handles defaults", {
  config_file <- app_sys("golem-config.yml")
  skip_if(config_file == "", "Config file not found")
  
  # Non-existent value should return default
  result <- get_golem_config(
    "non_existent_key",
    config = "default",
    file = config_file,
    default = "my_default"
  )
  expect_equal(result, "my_default")
  
  # NULL default
  result_null <- get_golem_config(
    "non_existent_key",
    config = "default",
    file = config_file,
    default = NULL
  )
  expect_null(result_null)
})

test_that("get_golem_config handles missing config file", {
  # Test with non-existent file
  result <- get_golem_config(
    "some_key",
    file = "path/to/nonexistent/file.yml",
    default = "fallback_value"
  )
  expect_equal(result, "fallback_value")
})
