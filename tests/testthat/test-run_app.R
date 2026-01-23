# Run App Tests
test_that("run_app returns a shiny.appobj", {
  # run_app should return a shiny app object
  app <- run_app()
  expect_s3_class(app, "shiny.appobj")
})

test_that("run_app has correct structure", {
  app <- run_app()
  
  # Should have UI and server components
  expect_true(!is.null(app$httpHandler))
  expect_true(!is.null(app$serverFuncSource))
})

test_that("run_app respects options", {
  # Test that options are passed through
  app <- run_app(options = list(port = 3838))
  expect_s3_class(app, "shiny.appobj")
})

test_that("run_app sets shiny options", {
  # Save current option
  old_max <- getOption("shiny.maxRequestSize")
  
  # Create app (this should set options)
  app <- run_app()
  
  # Check that max request size was set
  new_max <- getOption("shiny.maxRequestSize")
  expect_true(!is.null(new_max))
  expect_true(new_max > 0)
  
  # Restore
  options(shiny.maxRequestSize = old_max)
})
