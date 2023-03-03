library(shiny)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(reactable)

# Reload when saving the app
options(shiny.autoreload = TRUE)

# Load race results data
race_results <- readr::read_csv("data/formula1_2021season_raceResults.csv")
race_results$Track <- factor(race_results$Track, levels = unique(race_results$Track))
race_results <- race_results |>
  dplyr::group_by(Driver) |>
  dplyr::mutate(cumpoints = cumsum(Points))

# Load race names
# race_table <- race_results |>
#   dplyr::ungroup() |>
#   dplyr::select(Track) |>
#   dplyr::group_by(Track) |>
#   dplyr::distinct() |>
#   dplyr::rename(Race = 'Track')

race_table <- readr::read_csv("data/formula1_2021season_calendar.csv") |>
  dplyr::rename(Race = 'GP Name')
race_table$Race <- factor(race_table$Race, levels = unique(race_table$Race))
race_table <- race_table |>
  dplyr::select(Country, City, Race)

# Define UI for application that highlights rows in a table
ui <- fluidPage(
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
                             choices = unique(race_table$Race), 
                             selected = c("Bahrain", "Italy"),
                             grid = TRUE, 
                             from_fixed = TRUE,
                             width = "100%")
           )
    ),
    # Table of Races that interacts with raceSlider
    column(2,
           reactableOutput("Races")
           # dataTableOutput('Races')
    )
  )
)

# Define server logic required to make and highlight a table
server <- function(input, output, session) {
    # Initialize race names and colors
    highlight_races <- reactiveValues()
    highlight_races$races <- as.character(race_table$Race)
    highlight_races$row_color <- reactive({rep('white', length(highlight_races$races))})
    
    
    # Change row color depending on the slider race selections
    observeEvent(input$raceSlider, {
      start_race <- which(highlight_races$races == input$raceSlider[1])
      end_race <- which(highlight_races$races == input$raceSlider[2])
      highlight_races$races <- c(highlight_races$races[start_race:end_race],
                                 highlight_races$races[!(highlight_races$races %in% highlight_races$races[start_race:end_race])])
      highlight_races$row_color <- c(rep('pink', length(highlight_races$races[start_race:end_race])),
                                     rep('white', length(highlight_races$races) - length(highlight_races$races[start_race:end_race])))
    })

    # Create the table with the specified rows highlighted
    output$Races <- renderReactable({
      reactable(
        race_table,
        columns = list(
          Race = colDef(
            cell = function(value, index) {
              city_name <- race_table$City[index]
              race_index <- which(highlight_races$races == value)
              color <- highlight_races$row_color[race_index]
              image <- htmltools::img(src = sprintf("flags/%s.png", value), 
                           style = "height: 24px; padding: top; margin: top;", 
                           alt = value)
              htmltools::tagList(
                htmltools::div(style = "display: inline-block; float:left; width: 75px; padding-top: 10px; padding-left: 10px;", 
                  image),
                htmltools::div(
                  htmltools::div(style = list(fontWeight = 600), value),
                  htmltools::div(style = list(fontSize = "12px"), city_name),
                style = list(background = color, borderStyle = "solid", 
                             marginBottom = '0px', marginTop = '0px',
                             borderCollapse= 'separate',
                             borderSpacing= '0 0px')
              )
              # value
              )
            }
          ),
          Country = colDef(show = FALSE),
          City = colDef(show = FALSE)
        ),
        pagination = FALSE,
        compact = TRUE,
        style = "padding: 0px; border-collapse: collapse; border-spacing: 0;"
        )
    })
    
    
    
    # output$Races <- renderDataTable({
    #     datatable(race_table, 
    #               options = list("pageLength" = 22,
    #                              "searching" = FALSE,
    #                              "lengthChange"= FALSE)) |> formatStyle(
    #     'Race', target = 'row',
    #     backgroundColor = styleEqual(levels = highlight_races$races,
    #                                  values = highlight_races$row_color,
    #                                  default = "white")
    #     )
    # })

  # filter data frame for drivers based on selection
  drivers_plotting <- reactive({
    race_results |>
      dplyr::filter(Driver %in% input$driverSelect)
  })
  # draw the cumulative points line chart
  output$distPlot <- renderPlot({
    ggplot2::ggplot(drivers_plotting(), aes(x = Track, y = cumpoints, group = Driver, color = Driver)) +
      ggplot2::geom_line() + 
      ggplot2::geom_point() +
      ggplot2::labs(x = "GP", y = "Cumulative Points") +
      ggplot2::ggtitle("Cumulative points gained over the season") +
      ggplot2::theme(
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
