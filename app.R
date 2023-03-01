library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(shinythemes)



options(shiny.autoreload = TRUE)

#Loading the Race results data
race_results <- readr::read_csv("data/formula1_2021season_raceResults.csv")
race_results$Track <- factor(race_results$Track, levels = unique(race_results$Track))

#Mutating the race results data 
race_results_updated <- race_results |>
  dplyr::group_by(Driver) |>
  dplyr::mutate(cumpoints = cumsum(Points))

#Loading the lap info data
laptimes <- readr::read_csv("data/2021_all_laps_info.csv")

# Load race names
race_table <- race_results |>
  dplyr::ungroup() |>
  dplyr::select(Track) |>
  dplyr::group_by(Track) |>
  dplyr::distinct() |>
  dplyr::rename(Race = 'Track')

ui <- navbarPage("Formula 1 Dashboard",
                 theme = shinytheme("lumen"),
                 tabPanel("Panel 1",
                          
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
                                                     selected = c("Bahrain", "Italy"),
                                                     grid = TRUE, 
                                                     from_fixed = TRUE,
                                                     width = "100%")
                                   )
                            ),
                            # Table of Races that interacts with raceSlider
                            column(2,
                                   dataTableOutput('Races')
                            )
                          )),
                 tabPanel('Panel 2',
                          fluidRow(
                            # Dropdown for grand prix
                            column(6, selectInput(inputId = 'gp',
                                                  label = 'Choose Grand Prix',
                                                  choices = unique(race_results$GP),
                                                  selected = "Bahrain Grand Prix",
                            )),
                            # Dropdown for driver
                            column(6, selectInput(inputId = 'driver',
                                                  label = 'Choose Driver',
                                                  choices = unique(race_results$Driver),
                                                  selected = "Lewis Hamilton"))
                          ),
                          
                          fluidRow(
                            # Table output
                            column(6,
                                   DT::DTOutput(outputId = 'race_results_table')),
                            column(6,
                                   # plotOutput("lap_times_plot")
                            )
                            
                          )
                 ),
)

server <- function(input, output, session) {
  
  ##Functions for Panel 1 here
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
  output$Races <- renderDataTable({
    datatable(race_table, 
              options = list("pageLength" = 22,
                             "searching" = FALSE,
                             "lengthChange"= FALSE)) |> formatStyle(
                               'Race', target = 'row',
                               backgroundColor = styleEqual(levels = highlight_races$races,
                                                            values = highlight_races$row_color,
                                                            default = "white")
                             )
  })
  
  # filter data frame for drivers based on selection
  drivers_plotting <- reactive({
    race_results_updated |>
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
  
  
  ##Functions for Panel 2 here
  # Filter data frame for Grand Prix based on selection
  filtered_race_results <- reactive(
    race_results |> 
      dplyr::filter(GP == input$gp) |> 
      dplyr::select(-GP) 
  )
  
  # Filter the data frame for lap times based on selection
  filtered_laptimes <- reactive(laptimes |> 
                                  dplyr::filter(name == input$driver, GP == input$gp)  |> 
                                  mutate(lap_times_sec = 0.001*lap_time_ms))
  
  # Render the race results table
  output$race_results_table <- DT::renderDT({
    
    
    datatable(filtered_race_results())
    
  })
  
  # Draw the lap times for every driver
  output$lap_times_plot <- renderPlot({
    ggplot2::ggplot(filtered_laptimes(), aes(x = lap, y = lap_times_sec)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Lap number", y = "Lap times(in sec)") +
      ggplot2::ggtitle("Lap times For Driver") +
      ggplot2::theme(
        plot.title = element_text(size = 31, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 20, vjust = 0.6),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top")
    
  })
  
}

shinyApp(ui, server)