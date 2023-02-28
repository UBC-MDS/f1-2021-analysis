library(shiny)
library(shinyWidgets)
library(DT)
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
  
  # checkbox to filter for drivers
  fluidRow(
    column(2,
           checkboxGroupInput(inputId = "driverSelect", 
                              label = "Select drivers:", 
                              choices = unique(race_results$Driver), 
                              selected = c("Lewis Hamilton", "Carlos Sainz")),
           style="overflow-x: scroll; overflow-y: scroll"
    ),
    # display line chart
    column(8,
           plotOutput("distPlot"),
           fluidRow(
             tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # to hide the minor ticks
             sliderTextInput(inputId = "raceSlider",
                             label = "Select races", 
                             choices = unique(race_results$Track), 
                             selected = c("Bahrain", "Abu Dhabi"),
                             grid = TRUE, 
                             from_fixed = TRUE,
                             width = "100%")
           )
    ),
    column(2,
           print("This section for races!")
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
  
  # filter data frame for drivers based on selection
  drivers_plotting <- reactive({
    race_results |>
      filter(Driver %in% input$driverSelect)
  })
  # draw the cumulative points line chart
  output$distPlot <- renderPlot({
    ggplot2::ggplot(drivers_plotting(), aes(x = Track, y = cumpoints, group = Driver, color = Driver)) +
      ggplot2::geom_line() + 
      ggplot2::geom_point() +
      labs(x = "GP", y = "Cumulative Points") +
      ggtitle("Cumulative points gained over the season") +
      theme(
        plot.title = element_text(size = 31, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 20, vjust = 0.6),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
