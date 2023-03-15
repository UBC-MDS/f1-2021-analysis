library(shiny)
library(bslib)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(reactable)
library(tidyverse)
library(shinycssloaders)




options(shiny.autoreload = TRUE)

# Loading the Race results data
race_results <- readr::read_csv("data/formula1_2021season_raceResults.csv", col_types=c(`Fastest Lap`="character"))
race_results <- race_results |>
  dplyr::mutate(Track = dplyr::case_when(Track == "Netherlands" ~ "Dutch",
                                         Track == "Brazil" ~ "Sao Paulo",
                                         TRUE ~ Track))
race_results$Track <- factor(race_results$Track, levels = unique(race_results$Track))
# race_results$`Fastest Lap` = paste0('00:0', race_results$`Fastest Lap`)
# race_results$`Fastest Lap` = lubridate::hms(race_results$`Fastest Lap`)
race_results$flag <- ifelse(race_results$`+1 Pt` =='Yes', 1, 0)


gp_list <- unique(as.character(race_results$Track))

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

# Load calendar
calendar <- as.data.frame(readr::read_csv("data/formula1_2021season_calendar_GP.csv"))

# Load race names
race_table <- readr::read_csv("data/formula1_2021season_calendar.csv") |>
  dplyr::rename(Race = 'GP Name')
race_table$Race <- factor(race_table$Race, levels = unique(race_table$Race))
race_table <- race_table |>
  dplyr::select(Country, City, Race)

ui <- navbarPage("Formula 1 Dashboard",
                 theme = shinytheme("lumen"),
                 tabPanel(
                   "Season Highlights",
                   # checkbox to filter for drivers
                   fluidRow(
                     tabsetPanel(
                       tabPanel("Driver",
                                fluidRow(
                                  column(2,
                                    checkboxGroupInput(inputId = "driverSelect",
                                                       label = "Select drivers:",
                                                       choices = unique(driver_results$Driver),
                                                       selected = c("Lewis Hamilton", "Carlos Sainz"))
                                  ),
                                  column(8,
                                         plotOutput("distPlot", height = "480px") |> withSpinner(color="#0dc5c1"),
                                         fluidRow(
                                           tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # to hide the minor ticks
                                           sliderTextInput(inputId = "raceSliderDrivers",
                                                           label = "Select races",
                                                           choices = unique(driver_results$Track),
                                                           selected = c("Bahrain", "Abu Dhabi"),
                                                           grid = TRUE,
                                                           from_fixed = TRUE,
                                                           width = "100%")
                                         )
                                  ),
                                  # Table of Races that interacts with raceSliderDrivers
                                  column(2,
                                         reactableOutput("Races") |> withSpinner(color="#0dc5c1")
                                  )
                                ),
                       ),
                       tabPanel("Teams",
                                fluidRow(
                                  column(2,
                                         checkboxGroupInput(inputId = "teamSelect",
                                                            label = "Select teams:",
                                                            choices = unique(team_results$Team),
                                                            selected = c("McLaren Mercedes"))
                                  ),
                                  column(8,
                                         plotOutput("teamPointsPlot", height = "480px") |> withSpinner(color="#0dc5c1"),
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
                                  ),
                                  # Table of Races that interacts with raceSliderTeams
                                  column(2,
                                         reactableOutput("RacesTeamsTab") |> withSpinner(color="#0dc5c1")
                                  )
                                )
                       )
                     )
                   )
                 ),
                 tabPanel('Race Information',
                          fluidRow(
                            # Dropdown for grand prix
                            column(3,
                                   fluidRow(column(
                                     12,
                                     selectInput(
                                       inputId = 'gp',
                                       label = 'Choose Race',
                                       choices = unique(race_results$GP),
                                       selected = "Bahrain Grand Prix",
                                     )
                                   )),
                                   fluidRow(column(
                                     12,
                                     align="center",
                                     imageOutput("track_layout", height="200px") |> 
                                       withSpinner(color="#0dc5c1")
                                   )),
                                   fluidRow(# GP facts table
                                     column(
                                       12,
                                       tableOutput("gp_facts_table") |>
                                         withSpinner(color="#0dc5c1")
                                     )),
                                   
                                   ),
                                    
                            # Dropdown for driver
                            #   column(6, selectInput(inputId = 'driver',
                            #                         label = 'Choose Driver',
                            #                         choices = unique(race_results$Driver),
                            #                         selected = "Lewis Hamilton"))
                            # Table output
                            column(8,
                                   shinycssloaders::withSpinner(
                                   DT::DTOutput(outputId = 'race_results_table'),
                                   color="#0dc5c1"
                                   ),
                            )
                            # column(6,
                            #        # plotOutput("lap_times_plot"))

                            ))
                          
)

server <- function(input, output, session) {
  
  ##Functions for Panel 1 here
  # Initialize race names and colors
  highlight_races <- reactiveValues()
  highlight_races$races <- as.character(race_table$Race)
  highlight_races$row_color <- reactive({rep('white', length(highlight_races$races))})
  
  
  # Change row color depending on the slider race selections
  observeEvent(input$raceSliderDrivers, {
    start_race <- which(highlight_races$races == input$raceSliderDrivers[1])
    end_race <- which(highlight_races$races == input$raceSliderDrivers[2])
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
            )
            },
          align = "center",
          headerStyle = list(fontSize = "24px")
        ),
        Country = colDef(show = FALSE),
        City = colDef(show = FALSE)
      ),
      pagination = FALSE,
      compact = TRUE,
      height = 600,
      style = "padding: 0px; border-collapse: collapse; border-spacing: 0;"
    )
  })
  
  # Initialize race names and colour for teams tab
  highlight_races_teams_tab <- reactiveValues()
  highlight_races_teams_tab$races <- as.character(race_table$Race)
  highlight_races_teams_tab$row_color <- reactive({rep('white', length(highlight_races_teams_tab$races))})
  
  # Change row color depending on the slider race selections for the teams tab
  observeEvent(input$raceSliderTeams, {
    start_race <- which(highlight_races_teams_tab$races == input$raceSliderTeams[1])
    end_race <- which(highlight_races_teams_tab$races == input$raceSliderTeams[2])
    highlight_races_teams_tab$races <- c(highlight_races_teams_tab$races[start_race:end_race],
                                           highlight_races_teams_tab$races[!(highlight_races_teams_tab$races %in% 
                                                                                 highlight_races_teams_tab$races[start_race:end_race])])
    highlight_races_teams_tab$row_color <- c(rep('pink', length(highlight_races_teams_tab$races[start_race:end_race])),
                                   rep('white', length(highlight_races_teams_tab$races) - length(highlight_races_teams_tab$races[start_race:end_race])))
  })
  
  # Render table for the teams tab
  output$RacesTeamsTab <- renderReactable({
    reactable(
      race_table,
      columns = list(
        Race = colDef(
          cell = function(value, index) {
            city_name <- race_table$City[index]
            race_index <- which(highlight_races_teams_tab$races == value)
            color <- highlight_races_teams_tab$row_color[race_index]
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
            )
          },
          align = "center",
          headerStyle = list(fontSize = "24px")
        ),
        Country = colDef(show = FALSE),
        City = colDef(show = FALSE)
      ),
      pagination = FALSE,
      compact = TRUE,
      height=600,
      style = "padding: 0px; border-collapse: collapse; border-spacing: 0;"
    )
  })
  
  # filter data frame for drivers based on selection
  drivers_plotting <- reactive({
    last_race = input$raceSliderDrivers[2]
    driver_results |>
      dplyr::filter(Driver %in% input$driverSelect) |>
      dplyr::filter(Track %in% gp_list[1:which(gp_list == last_race)])
  })
  # draw the cumulative points line chart for drivers
  output$distPlot <- renderPlot({
    ggplot2::ggplot(drivers_plotting(), aes(x = Track, y = cumpoints, group = Driver, color = Driver)) +
      ggplot2::geom_line() + 
      ggplot2::geom_point() +
      ggplot2::labs(x = "Race", y = "Cumulative Points") +
      ggplot2::ggtitle("Cumulative points gained over the season") +
      ggplot2::scale_y_continuous(limits = c(0, 400)) +
      ggplot2::theme(
        plot.title = element_text(size = 25, face = "bold"),
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
      dplyr::filter(Track %in% gp_list[1:which(gp_list == last_race)])
  })
  # draw the cumulative points line chart for teams
  output$teamPointsPlot <- renderPlot({
    ggplot2::ggplot(teams_plotting(), aes(x = Track, y = team_cp, group = Team, color = Team)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Race", y = "Cumulative Points") +
      ggplot2::ggtitle("Cumulative points gained over the season") +
      ggplot2::scale_y_continuous(limits = c(0, 650)) +
      ggplot2::theme(
        plot.title = element_text(size = 25, face = "bold"),
        axis.text.x = element_text(size = 10, angle = 20, vjust = 0.6),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 10, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top",
      )
  })
  
  
  ##Functions for Panel 2 here
  output$gp_facts_table <- renderTable({
    Sys.sleep(0.1)
    facts <- subset(calendar, calendar$"GP Name" == input$gp)
    row.names(facts) <- facts$"GP Name"
    facts <-
      facts |> select(
        "Round",
        "Race Date",
        "Country",
        "City",
        "Circuit Name",
        "Circuit Length(km)",
        "Number of Laps",
        "Race Distance(km)",
        "Turns",
        "DRS Zones"
      )
    transpose <-
      data.frame(t(facts)) |>
      rownames_to_column("Race")
    colnames(transpose) <- c("Race", input$gp)
    transpose
  })
  
  output$track_layout <- renderImage({
    Sys.sleep(0.1)
    filename <- normalizePath(file.path('./www/tracks',
                                        paste(input$gp, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$gp))
    
  }, deleteFile = FALSE)
  
  
  # Filter data frame for Grand Prix based on selection
  filtered_race_results <- reactive(
    race_results |> 
      dplyr::filter(GP == input$gp) |> 
      dplyr::select(-GP, -Track)  |> 
      dplyr::mutate(Position = as.integer(Position)) |> 
      dplyr::select(Driver, No, Team, Position, `Time/Retired`, Laps,
                    `Starting Grid`, Points, `Fastest Lap`, flag) 
    
  )
  
  # Filter the data frame for lap times based on selection
  filtered_laptimes <- reactive(laptimes |> 
                                  dplyr::filter(name == input$driver, GP == input$gp)  |> 
                                  mutate(lap_times_sec = 0.001*lap_time_ms))
  
  # Render the race results table
  output$race_results_table <- DT::renderDT({
    Sys.sleep(0.1)
    datatable(filtered_race_results(),
              options = list("pageLength" = 20,
                             "paging" = FALSE,
                             "scrollY" = '800px',
                             "scrollX" = 'TRUE',
                             "autoWidth" = TRUE,
                             "columnDefs" = list(list(visible = FALSE, targets = c("flag")))
              ),
              selection = "none"
    ) |> 
      formatStyle(
        'Fastest Lap', 'flag', 
        backgroundColor = styleEqual(c(1), c('#B138DD'))
      )
    
    
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