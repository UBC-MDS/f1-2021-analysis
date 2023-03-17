library(shinytest2)

test_that("{shinytest2} recording: select-all-drivers", {
  app <- AppDriver$new(name = "select-all-drivers", height = 664, width = 1065)
  app$click("selectalldrivers")
  app$expect_values()
})


test_that("{shinytest2} recording: race-slider-drivers", {
  app <- AppDriver$new(name = "race-slider-drivers", height = 664, width = 1065)
  app$set_inputs(raceSliderDrivers = "Belgium")
  app$expect_values()
})


test_that("{shinytest2} recording: dropdown-gp", {
  app <- AppDriver$new(name = "dropdown-gp", height = 664, width = 1065)
  app$set_inputs(gp = "Azerbaijan Grand Prix")
  app$expect_values()
})

