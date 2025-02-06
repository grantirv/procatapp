test_that("period_input works", {
  pi <- period_input(id = "test", label = "Test", width = "480px")
  expect_s3_class(pi, "shiny.tag")
})

test_that("period module produces expected output dates", {

  shiny_app = period_app(min = "2000-01-01", max = "2024-01-20")
  app = shinytest2::AppDriver$new(shiny_app, name = "period_app")

  app$set_inputs(`period-btns` = "MTD")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "QTD")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "YTD")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "1M")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "3M")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "6M")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "1Y")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "3Y")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "5Y")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "10Y")
  app$expect_values(output = "dates", screenshot_args = FALSE)

  app$set_inputs(`period-btns` = "ALL")
  app$expect_values(output = "dates", screenshot_args = FALSE)
})
