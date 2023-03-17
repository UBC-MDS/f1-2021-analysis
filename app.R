library(shiny)
library(bslib)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(ggplot2)
library(reactable)
library(tidyverse)
library(shinycssloaders)
library(ggdark)
library(insight)
library(plotly)



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
race_results$dnf <- ifelse(race_results$`Time/Retired`=='DNF' | race_results$`Time/Retired`=='DNS', 1, 0)
race_results$Driver <- factor(race_results$Driver, levels = unique(race_results$Driver))
race_results$Team <- factor(race_results$Team, levels = unique(race_results$Team))

# create mapping for team color and line type
race_results <- race_results |>
  dplyr::mutate(
    team_color = dplyr::case_when(
      Team == "Mercedes" ~ "#00D2BE",
      Team == "Red Bull Racing Honda" ~ "#0600EF",
      Team == "McLaren Mercedes" ~ "#FF8700",
      Team == "Ferrari" ~ "#DC0000",
      Team == "AlphaTauri Honda" ~ "#2B4562",
      Team == "Aston Martin Mercedes" ~ "#006F62",
      Team == "Alfa Romeo Racing Ferrari" ~ "#900000",
      Team == "Alpine Renault" ~ "#0090FF",
      Team == "Williams Mercedes" ~ "#005AFF",
      Team == "Haas Ferrari" ~ "#FFFFFF"),  
    line_type = dplyr::case_when(
      Driver %in% c("Lewis Hamilton", "Max Verstappen", "Lando Norris", 
                    "Charles Leclerc", "Yuki Tsunoda", "Lance Stroll",
                    "Kimi Raikkönen", "Esteban Ocon", "George Russell") ~ "solid",
      Driver == "Robert Kubica" ~ "dashed",
      TRUE ~ "dotted")
    )

# Add mapping for color to driver
driver_colors <- race_results |>
  dplyr::filter(Track == "Bahrain") |>
  dplyr::select(Driver, team_color) |>
  dplyr::pull(team_color)
driver_colors <- append(driver_colors, "#900000")
names(driver_colors) <- levels(race_results$Driver)

# add mapping for color to team
team_colors <- unique(race_results$team_color)
names(team_colors) <- unique(race_results$Team)

# add mapping for line type to driver
driver_linetype <- race_results |>
  dplyr::filter(Track == "Bahrain") |>
  dplyr::select(Driver, line_type) |>
  dplyr::pull(line_type)
driver_linetype <- append(driver_linetype, "dashed")
names(driver_linetype) <- levels(race_results$Driver)


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

ui <- navbarPage(title = div(img(src = "UI/f1-logo.png",
                                 id = "logo",
                                 # height = "150px",
                                 width = "150px",
                                 style = "position: relative; padding-bottom: 0px; 
                                 margin-right: 5px; display:left-align;"),
                             "Formula 1 Dashboard",
                             style = "margin-top: 30px; font-weight: bold; font-size: 25px"),
tags$head(
  tags$style(HTML(' .navbar {
                          height: 80px;
                          min-height:50px !important;
                        }
                      .navbar-nav > li > a, .navbar-brand {
                            padding-top:3px !important; 
                            padding-bottom:1px !important;
                            height: 20px;
                            }'))),
                 theme = bs_theme(
                   bg = "#101010", 
                   fg = "#ebdddd", 
                   primary = "#e00a07",
                   danger = "#ED79F9",
                   base_font = font_google("Prompt"),
                   # secondary = "#101011",
                 ),
                 tabPanel(
                   "Season Highlights",
                   tags$head(tags$style( HTML(' .nav {margin-top:50px;}'))),
                   # checkbox to filter for drivers
                   fluidRow(
                     tabsetPanel(
                       tabPanel("Driver",
                                tags$head(tags$style( HTML(' .nav {margin-left:10px; margin-top:-10px;}'))),
                                div(style = "margin-left: 20px; margin-right: 20px;",
                                  
                                fluidRow(
                                  column(2,
                                         checkboxGroupInput(
                                           inputId = "driverSelect", 
                                           label = "Select drivers:", 
                                           choices = sort(unique(driver_results$Driver)),
                                           selected = c("Lewis Hamilton", "Carlos Sainz")
                                         ),
                                         fluidRow(
                                           column(6,
                                                  actionButton(inputId = "selectalldrivers", label = "Select All", width = "95%")) ,
                                        column(6,  
                                               actionButton(inputId = "deselectalldrivers", label = "Deselect All"))
                                        ),
                                        
                                  ),
                                  
                                  column(8,
                                         plotlyOutput("distPlot", height = "480px") |> 
                                           withSpinner(color="#FF0000",
                                                       image = "UI/200w.gif"
                                                       ),
                                         fluidRow(
                                           tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # to hide the minor ticks
                                           sliderTextInput(
                                             inputId = "raceSliderDrivers", 
                                             label = "Select races:", 
                                             selected = c("Abu Dhabi"),
                                             grid = TRUE, 
                                             force_edges = TRUE,
                                             choices = unique(driver_results$Track),
                                             width = "100%"
                                           )
                                         ),
                                  ),
                                  # Table of Races that interacts with raceSliderDrivers
                                  column(2,
                                         reactableOutput("Races")
                                  )
                                ),
                                )
                       ),
                       tabPanel("Teams",
                                tags$head(tags$style( HTML(' .nav {margin-left:10px;}'))),
                                div(style = "margin-left: 20px; margin-right: 20px;",
                                fluidRow(
                                  column(2,
                                         checkboxGroupInput(
                                           inputId = "teamSelect", 
                                           label = "Select teams:", 
                                           choices = sort(unique(team_results$Team)),
                                           selected = c("McLaren Mercedes"),
                                           ),
                                         fluidRow(
                                           column(6,
                                                  actionButton(inputId = "selectallteams", label = "Select All", width = "95%")) ,
                                           column(6,  
                                                  actionButton(inputId = "deselectallteams", label = "Deselect All"))
                                         )
                                         
                                         
                                  ),
                                  column(8,
                                         plotlyOutput("teamPointsPlot", height = "480px") |> withSpinner(color="#FF0000",
                                                                                                       image = "UI/200w.gif"),
                                         fluidRow(
                                           tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # to hide the minor ticks
                                           sliderTextInput(inputId = "raceSliderTeams",
                                                           label = "Select races:",
                                                           choices = unique(driver_results$Track),
                                                           selected = c("Abu Dhabi"),
                                                           grid = TRUE,
                                                           width = "100%")
                                         )
                                  ),
                                  # Table of Races that interacts with raceSliderTeams
                                  column(2,
                                         reactableOutput("RacesTeamsTab")
                                  )
                                )
                                )
                       )
                     )
                   )
                 ),
                 tabPanel('Race Information',
                          fluidRow(
                            column(3,
                                   # Dropdown for grand prix
                                   fluidRow(column(
                                     12,
                                     align = "center",
                                     uiOutput("selector")
                                   )),
                                   # Previous and next buttons
                                   fluidRow(column(
                                     5, 
                                     align = "center",
                                     tags$div(class="row", tags$div(uiOutput("prevBin")))
                                     ),
                                   column(3),
                                   column(
                                     4,
                                     align = "center",
                                     tags$div(class="row", tags$div(uiOutput("nextBin")))
                                     )),
                                   # Track map image
                                   fluidRow(column(
                                     12,
                                     align="center",
                                     imageOutput("track_layout", height="150px")
                                     )),
                                   # GP facts table
                                   fluidRow(
                                     column(1),
                                     column(
                                     11,
                                     tableOutput("gp_facts_table")
                                     )),
                                   ),
                            # Table output
                            column(8,
                                   fluidRow(column(
                                     12,
                                     shinycssloaders::withSpinner(
                                     DT::DTOutput(outputId = 'race_results_table'),
                                     color="#FF0000", image = "UI/200w.gif")
                                     )),
                                   # Legend
                                   fluidRow(column(
                                     2, 
                                     align = "center",
                                     style = "background-color:#A83349; padding: 10px; margin-top: 20px; margin-left: 12px;",
                                     span(textOutput("legend1"), style = "color:black;")
                                     ),
                                   column(8),
                                   column(
                                     2,
                                     align = "center",
                                     style = "background-color:#B138DD; margin-top: 20px; margin-left: -24px;",
                                     span(textOutput("legend2"), style = "color:black;")
                                     )), 
                                   style = "margin-left: 20px;"
                                   )
                            ))
                          
)

server <- function(input, output, session) {
  
  ##Functions for Panel 1 here
  # Initialize race names and colors
  highlight_races <- reactiveValues()
  highlight_races$races <- as.character(race_table$Race)
  highlight_races$row_color <- reactive({rep('white', length(highlight_races$races))})
  
  # helper function to add row highlight based on slider race selection
  add_highlight <- function(input_id, reactive_value) {
    start_race <- 1
    end_race <- which(reactive_value$races == input_id[1])
    reactive_value$races <- c(reactive_value$races[start_race:end_race],
                              reactive_value$races[!(reactive_value$races %in% reactive_value$races[start_race:end_race])])
    reactive_value$row_color <- c(rep('pink', length(reactive_value$races[start_race:end_race])),
                                   rep('white', length(reactive_value$races) - length(reactive_value$races[start_race:end_race])))
  }
  
  # helper function to create the table for season's races
  render_race_table <- function(reactive_value) {
    options(reactable.theme = reactableTheme(
      color = "hsl(0, 100%, 0%)",
      backgroundColor = "hsl(233, 9%, 19%)"),
      headerStyle = list(
        background = "hsl(294, 91%, 73%)"
      )
    )
    reactable(
      race_table,
      columns = list(
        Race = colDef(
          cell = function(value, index) {
            city_name <- race_table$City[index]
            race_index <- which(reactive_value$races == value)
            color <- reactive_value$row_color[race_index]
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
          headerStyle = list(fontSize = "24px",
                             background = "#101010",
                             color = "#FDF7F7"
          ),
          sortable = FALSE,
        ),
        Country = colDef(show = FALSE),
        City = colDef(show = FALSE)
      ),
      pagination = FALSE,
      compact = TRUE,
      height=600,
      style = "padding: 0px; border-collapse: collapse; border-spacing: 0;"
    )
  }
  
  
  # Change row color depending on the slider race selections
  observeEvent(input$raceSliderDrivers, add_highlight(input$raceSliderDrivers, highlight_races))

  # Create the table with the specified rows highlighted
  output$Races <- renderReactable(render_race_table(highlight_races))
  
  # Initialize race names and colour for teams tab
  highlight_races_teams_tab <- reactiveValues()
  highlight_races_teams_tab$races <- as.character(race_table$Race)
  highlight_races_teams_tab$row_color <- reactive({rep('white', length(highlight_races_teams_tab$races))})
  
  # Change row color depending on the slider race selections for the teams tab
  observeEvent(input$raceSliderTeams, add_highlight(input$raceSliderTeams, highlight_races_teams_tab))
  
  # Render table for the teams tab
  output$RacesTeamsTab <- renderReactable(render_race_table(highlight_races_teams_tab))
  
  observe({
    if(input$selectallteams == 0) return(NULL) 
    
    else
    {
      updateCheckboxGroupInput(session,"teamSelect","Select teams:",
                               choices = sort(unique(team_results$Team)),
                               selected=unique(team_results$Team))
    }
  })
  
  observe({
    if(input$deselectallteams == 0) return(NULL) 
    else
    {
      updateCheckboxGroupInput(session,"teamSelect","Select teams:",
                               choices = sort(unique(team_results$Team)))
      }
  })
  
  observe({
    if(input$selectalldrivers == 0) return(NULL) 
    else
    {
      updateCheckboxGroupInput(session,"driverSelect","Select drivers:",
                               choices = sort(unique(driver_results$Driver)),
                               selected=unique(driver_results$Driver))
    }
  })
  
  observe({
    if(input$deselectalldrivers == 0) return(NULL) 
    else
    {
      updateCheckboxGroupInput(session,"driverSelect","Select drivers:",
                               choices = sort(unique(driver_results$Driver)))
    }
  })

  
  # filter data frame for drivers based on selection
  drivers_plotting <- reactive({
    last_race = input$raceSliderDrivers[1]
    driver_results |>
      dplyr::filter(Driver %in% input$driverSelect) |>
      dplyr::filter(Track %in% gp_list[1:which(gp_list == last_race)])
    
  })

  # draw the cumulative points line chart for drivers
  output$distPlot <- renderPlotly({
    driver_plot <- ggplot2::ggplot(
      drivers_plotting(), aes(x = Track, y = cumpoints, group = Driver,
                              color = Driver, linetype = Driver, 
                              text = paste("Team:", Team,
                                           "\nCumulative Points:", cumpoints))) 
    
    if (nrow(drivers_plotting()) == 0) {
      driver_plot <- driver_plot + ggplot2::geom_blank()
    } else {
      driver_plot <- driver_plot + ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_color_manual(values = driver_colors) +
        ggplot2::scale_linetype_manual(values = driver_linetype)
    }
      driver_plot <- driver_plot + 
      ggplot2::labs(x = "Race", y = "Cumulative Points") +
      ggplot2::ggtitle("Cumulative points gained over the season") +
      ggplot2::scale_y_continuous(limits = c(0, 400)) +
      ggdark::dark_theme_classic() +
      ggplot2::theme(
        plot.title = element_text(size = 25, face = "bold", family = "Prompt"),
        axis.text.x = element_text(size = 10, angle = 20, vjust = 0.6, family = "Prompt"),
        axis.text.y = element_text(size = 10, family = "Prompt"),
        axis.title = element_text(size = 15, face = "bold", family = "Prompt"),
        legend.text = element_text(size = 10, face = "bold", family = "Prompt"),
        legend.title = element_blank(),
        legend.position = "top",
      )
    driver_plot <- ggplotly(driver_plot, tooltip = c("x", "text", "color")) |>
      layout(legend = list(
        itemclick = FALSE,
        itemdoubleclick = FALSE,
        groupclick = FALSE
      ))
  })
  
  # filter data frame for teams based on selection
  teams_plotting <- reactive({
    last_race = input$raceSliderTeams[1]
    team_results |>
      dplyr::filter(Team %in% input$teamSelect) |>
      dplyr::filter(Track %in% gp_list[1:which(gp_list == last_race)])
  })
  # draw the cumulative points line chart for teams
  output$teamPointsPlot <- renderPlotly({
    teams_plot <- ggplot2::ggplot(
      teams_plotting(), aes(x = Track, y = team_cp, group = Team, color = Team, 
                            text = paste("Cumulative Points:", team_cp))) +
      scale_color_manual(values = team_colors)
    
    if (nrow(teams_plotting()) == 0) {
      teams_plot <- teams_plot + ggplot2::geom_blank()
    } else {
      teams_plot <- teams_plot + ggplot2::geom_line() +
        ggplot2::geom_point()
    }
    teams_plot <- teams_plot + 
      ggplot2::labs(x = "Race", y = "Cumulative Points") +
      ggplot2::ggtitle("Cumulative points gained over the season") +
      ggplot2::scale_y_continuous(limits = c(0, 650)) +
      ggdark::dark_theme_classic() +
      ggplot2::theme(
        plot.title = element_text(size = 25, face = "bold", family = "Prompt"),
        axis.text.x = element_text(size = 10, angle = 20, vjust = 0.6, family = "Prompt"),
        axis.text.y = element_text(size = 10, family = "Prompt"),
        axis.title = element_text(size = 15, face = "bold", family = "Prompt"),
        legend.text = element_text(size = 10, face = "bold", family = "Prompt"),
        legend.title = element_blank(),
        legend.position = "top",
      )
    teams_plot <- ggplotly(teams_plot, tooltip = c("x", "text", "color")) |>
      layout(legend = list(
        itemclick = FALSE,
        itemdoubleclick = FALSE,
        groupclick = FALSE
      ))
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
                    `Starting Grid`, Points, `Fastest Lap`, flag, dnf) 
    
  )
  
  # Filter the data frame for lap times based on selection
  filtered_laptimes <- reactive(laptimes |> 
                                  dplyr::filter(name == input$driver, GP == input$gp)  |> 
                                  mutate(lap_times_sec = 0.001*lap_time_ms))
  
  # Render the race results table
  output$race_results_table <- DT::renderDT({
    Sys.sleep(0.1)
    datatable(filtered_race_results(),
              rownames = F,
              options = list("pageLength" = 15,
                             "paging" = F,
                             "scrollY" = '550px',
                             "scrollX" = 'TRUE',
                             "rownames" = 'FALSE',
                             "columnDefs" = list(list(visible = FALSE, targets = c("flag", "dnf"))),
                             "pagination" = FALSE,
                             "info" = FALSE
              ),
              selection = "none"
    ) |> 
      formatStyle(
        'Fastest Lap', 'flag', 
        target = 'row',
        backgroundColor = styleEqual(c(1), c('#B138DD'))
      ) |>
      formatStyle(
        'dnf', 
        target = 'row',
        backgroundColor = styleEqual(1, c('#A83349'))
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
  
  # Dropdown race select
  output$selector <- renderUI({
    selectInput(
      inputId = 'gp',
      label = 'Choose Race',
      choices = unique(race_results$GP),
      selected = "Bahrain Grand Prix",
    )
  })
  
  # Previous/Next buttons
  output$prevBin <- renderUI({
    actionButton("prevBin", 
                 label = "Previous")
  })
  output$nextBin <- renderUI({
    actionButton("nextBin", 
                 label = "Next")
  })
  
  observeEvent(input$prevBin, {
    current <- which(unique(race_results$GP) == input$gp)
    if(current > 1){
      updateSelectInput(session, "gp",
                        choices = unique(race_results$GP),
                        selected = unique(race_results$GP)[current - 1])
    }
  })
  observeEvent(input$nextBin, {
    current <- which(unique(race_results$GP) == input$gp)
    if(current < length(unique(race_results$GP))){
      updateSelectInput(session, "gp",
                        choices = unique(race_results$GP),
                        selected = unique(race_results$GP)[current + 1])
    }
  })
  
  # Legend 
  output$legend1 <- renderText({"DNF/DNS"})
  output$legend2 <- renderText({"Overall Fastest Lap"})
  
}

shinyApp(ui, server)