library(shinytest2)

test_that("{shinytest2} recording: select-all-drivers", {
  app <- AppDriver$new(name = "select-all-drivers", height = 795, width = 1211)
  app$click("selectalldrivers")
  app$expect_values()
})


test_that("{shinytest2} recording: slider-teams", {
  app <- AppDriver$new(name = "slider-teams", height = 795, width = 1211)
  app$set_inputs(raceSliderTeams = "United States")
  app$expect_values()
})


test_that("{shinytest2} recording: gp-dropdown", {
  app <- AppDriver$new(name = "gp-dropdown", height = 795, width = 1211)
  app$set_inputs(gp = "Monaco Grand Prix")
  app$expect_values()
})
