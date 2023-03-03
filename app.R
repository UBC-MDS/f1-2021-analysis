library(shiny)
library(bslib)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(reactable)




options(shiny.autoreload = TRUE)

# Loading the Race results data
race_results <- readr::read_csv("data/formula1_2021season_raceResults.csv")
race_results <- race_results |>
  dplyr::mutate(Track = dplyr::case_when(Track == "Netherlands" ~ "Dutch",
                                         Track == "Brazil" ~ "Sao Paulo",
                                         TRUE ~ Track))
race_results$Track <- factor(race_results$Track, levels = unique(race_results$Track))


# Get cumulative points of each driver over the season
driver_results <- race_results |>
  dplyr::group_by(Driver) |>
  dplyr::mutate(cumpoints = cumsum(Points))

# Load team cumulative points
team_results <- race_results |>
  dplyr::group_by(Team, Track) |>
  dplyr::summarise(team_pt = sum(Points)) |>
  dplyr::mutate(team_cp = cumsum(team_pt))

# Loading the lap info data
laptimes <- readr::read_csv("data/2021_all_laps_info.csv")

# Load race names
race_table <- readr::read_csv("data/formula1_2021season_calendar.csv") |>
  dplyr::rename(Race = 'GP Name')
race_table$Race <- factor(race_table$Race, levels = unique(race_table$Race))
race_table <- race_table |>
  dplyr::select(Country, City, Race)

ui <- navbarPage("Formula 1 Dashboard",
                 theme = shinytheme("lumen"),
                 tabPanel(
                   "Panel 1",
                   # checkbox to filter for drivers
                   fluidRow(
                     column(10, 
                            tabsetPanel(
                              tabPanel("Driver",
                                       fluidRow(
                                         column(2,
                                                checkboxGroupInput(inputId = "driverSelect", 
                                                                   label = "Select drivers:", 
                                                                   choices = unique(driver_results$Driver), 
                                                                   selected = c("Lewis Hamilton", "Carlos Sainz")),
                                                style="overflow-x: scroll; overflow-y: scroll",
                                         ),
                                         column(10,
                                                plotOutput("distPlot"),
                                                fluidRow(
                                                  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # to hide the minor ticks
                                                  sliderTextInput(inputId = "raceSlider",
                                                                  label = "Select races",
                                                                  choices = unique(driver_results$Track),
                                                                  selected = c("Bahrain", "Abu Dhabi"),
                                                                  grid = TRUE,
                                                                  from_fixed = TRUE,
                                                                  width = "100%")
                                                )
                                          )
                                       ),
                                     ),
                              tabPanel("Teams",
                                       fluidRow(
                                         column(2, 
                                                checkboxGroupInput(inputId = "teamSelect",
                                                                   label = "Select teams:",
                                                                   choices = unique(team_results$Team),
                                                                   selected = c("McLaren Mercedes")),
                                                style="overflow-x: scroll; overflow-y: scroll"
                                         ),
                                         column(10,
                                                plotOutput("teamPointsPlot"),
                                                fluidRow(
                                                         tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # to hide the minor ticks
                                                         sliderTextInput(inputId = "raceSliderTeams",
                                                                         label = "Select races",
                                                                         choices = unique(driver_results$Track),
                                                                         selected = c("Bahrain", "Abu Dhabi"),
                                                                         grid = TRUE,
                                                                         from_fixed = TRUE,
                                                                         width = "100%")
                                                )
                                         )
                                       )
                                     )
                              )
                            ),
                     # Table of Races that interacts with raceSlider
                     column(2,
                            reactableOutput("Races")
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
  
  output$Races <- renderReactable({
    reactable(
      race_table,
      columns = list(
        Race = colDef(
          cell = function(value, index) {
            city_name <- race_table$City[index]
            race_index <- which(highlight_races$races == value)
            color <- highlight_races$row_color[race_index]
            div(
              div(style = list(fontWeight = 600,
                               background = color), value),
              div(style = list(fontSize = "12px",
                               background = color), city_name)
            )
          }
        ),
        Country = colDef(show = FALSE),
        City = colDef(show = FALSE)
      ),
      pagination = FALSE
    )
  })
  
  # filter data frame for drivers based on selection
  drivers_plotting <- reactive({
    driver_results |>
      dplyr::filter(Driver %in% input$driverSelect)
  })
  # draw the cumulative points line chart for drivers
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
  
  # filter data frame for teams based on selection
  teams_plotting <- reactive({
    last_race = input$raceSliderTeams[2]
    team_results |>
      dplyr::filter(Team %in% input$teamSelect) |>
      dplyr::filter(Track %in% highlight_races$races[1:which(highlight_races$races == last_race)])
  })
  
  # draw the cumulative points line chart for teams
  output$teamPointsPlot <- renderPlot({
    ggplot2::ggplot(teams_plotting(), aes(x = Track, y = team_cp, group = Team, color = Team)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "GP", y = "Cumulative Points") +
      ggplot2::ggtitle("Cumulative points gained over the season") +
      ggplot2::scale_x_discrete(limits = unique(race_results$Track)) +
      ggplot2::scale_y_continuous(limits = c(0, 650)) +
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