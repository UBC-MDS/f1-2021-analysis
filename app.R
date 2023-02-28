library(shiny)
library(shinyWidgets)
library(DT)
library(readr)
# library(dplyr)
library(ggplot2)

# Reload when saving the app
options(shiny.autoreload = TRUE)

race_results <- readr::read_csv("data/formula1_2021season_raceResults.csv")
race_results$Track <- factor(race_results$Track, levels = unique(race_results$Track))
race_results <- race_results |>
  dplyr::group_by(Driver) |>
  dplyr::mutate(cumpoints = cumsum(Points))

# Define UI for application that highlights rows in a table
ui <- fluidPage(
    # Add a slider called "highlight" to determine number of rows to highlight
    # sliderInput(inputId = 'highlight',
    #             label = 'Slide to highlight table',
    #             min = 1,
    #             max = 5,
    #             value = 1),
    # 
    # # Output is a datatable called 'table1'
    # dataTableOutput('table1')
  fluidRow(
    column(2,
           checkboxGroupInput(inputId = "driverSelect", 
                              label = "Select drivers:", 
                              choices = unique(race_results$Driver), 
                              selected = c("Lewis Hamilton", "Carlos Sainz")),
           style="overflow-x: scroll; overflow-y: scroll"
    ),
    column(8,
           plotOutput("distPlot"),
           fluidRow(
             sliderTextInput("Month","Select Month" , 
                             choices = c("January", "February", "March", "April"), 
                             selected = c("January", "February", "March", "April"), 
                             animate = FALSE, grid = FALSE, 
                             hide_min_max = FALSE, from_fixed = FALSE,
                             to_fixed = FALSE, from_min = NULL, from_max = NULL, to_min = NULL,
                             to_max = NULL, force_edges = FALSE, width = NULL, pre = NULL,
                             post = NULL, dragRange = TRUE)
           )
    ),
    column(2,
           print("TEST")
    )
  )
)

# Define server logic required to make and highlight a table
server <- function(input, output, session) {
    # Example data
    # df <- faithful[1:5,]
    # 
    # # match data with color
    # vals <- reactiveValues(
    #     eruptions = df[, 1],
    #     row_color = rep('white', 5)
    # )
    # 
    # # Change row color depending on the number of highlighted rows
    # observeEvent(input$highlight, {
    #     vals$eruptions <-
    #         c(vals$eruptions[1:input$highlight],
    #           vals$eruptions[input$highlight+1:length(vals$eruptions)])
    #     vals$row_color <- c(rep('yellow', input$highlight),
    #                         rep('white', length(vals$eruptions) - input$highlight))
    # })
    # 
    # # draw the table with the specified number of rows highlighted
    # output$table1 <- renderDataTable({
    #     datatable(df) |> formatStyle(
    #         'eruptions', target = 'row',
    #         backgroundColor = styleEqual(vals$eruptions,
    #                                      vals$row_color,
    #                                      default = 'white')
    #     )
    # })
  drivers_plotting <- reactive({
    race_results |>
      filter(Driver %in% input$driverSelect)
  })
  output$distPlot <- renderPlot({
    ggplot(drivers_plotting(), aes(x = Track, y = cumpoints, group = Driver, color = Driver)) +
      geom_line() + 
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
