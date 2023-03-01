library(shiny)
library(shinyWidgets)
library(DT)
library(ggplot2)

# Reload when saving the app
options(shiny.autoreload = TRUE)

# Load race results data
race_results <- readr::read_csv("data/formula1_2021season_raceResults.csv")
race_results$Track <- factor(race_results$Track, levels = unique(race_results$Track))
race_results <- race_results |>
  dplyr::group_by(Driver) |>
  dplyr::mutate(cumpoints = cumsum(Points))

# Load Race names and index
race_table <- readr::read_csv("data/formula1_2021season_raceResults.csv")
race_table$Track <- factor(race_table$Track, levels = unique(race_table$Track))
race_table <- race_table |>
  dplyr::select(Track) |>
  dplyr::group_by(Track) |>
  dplyr::mutate(Race = dplyr::cur_group_id(), .before=1) |>
  dplyr::distinct() #|>
  # dplyr::ungroup() |>
  # dplyr::slice(1:5)

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
                             selected = c("Bahrain", "Spain"),
                             grid = TRUE, 
                             from_fixed = TRUE,
                             width = "100%")
           )
    ),
    column(2,
           dataTableOutput('Races')
    )
  )
)

# Define server logic required to make and highlight a table
server <- function(input, output, session) {
    # Initialize race names and colors
    highlight_races <- reactiveValues(
      races = as.character(race_table$Track),
      row_color = rep('white', length(races))
    )
    # Get indices of the selected races
    # selected_races <- reactiveValues({
    #   start_race <- which(races == input$raceSlider[1])
    #   end_race <- which(races == input$raceSlider[2])
    #   })
    # end_race <- reactive({
    #   which(races == input$raceSlider[2])
    #   })
    # selected_races <- reactive({highlight_races$races[start_race:end_race]})
    
    num_races <- reactive({end_race - start_race + 1})
    
    # Change row color depending on the slider race selections
    observeEvent(input$raceSlider, {
      start_race <- which(races == input$raceSlider[1])
      end_race <- which(races == input$raceSlider[2])
      highlight_races$races <- c(highlight_races$races[start_race:end_race],
                                 highlight_races$races[!(highlight_races$races %in% highlight_races$races[start_race:end_race])])
      highlight_races$row_color <- c(rep('yellow', length(highlight_races$races[start_race:end_race])),
                                     rep('white', length(highlight_races$races) - length(highlight_races$races[start_race:end_race])))
    })

    # Create the table with the specified rows highlighted
    output$Races <- renderDataTable({
        datatable(race_table) #|> formatStyle(
            # 'Track', target = 'row',
            # backgroundColor = styleEqual(levels = highlight_races$races,
            #                              values = highlight_races$row_color,
            #                              default = "white"),
            # options = list("pageLength" = 22)
        #)
    })

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
