library(shiny)
library(DT)

# Define UI for application that highlights rows in a table

ui <- fluidPage(
    # Add a slider called "highlight" to determine number of rows to highlight
    sliderInput(inputId = 'highlight',
                label = 'Slide to highlight table',
                min = 1,
                max = 5,
                value = 1),
    
    # Output is a datatable called 'table1'
    dataTableOutput('table1')
)

# Define server logic required to make and highlight a table
server <- function(input, output, session) {
    # Example data
    df <- faithful[1:5,]
    
    # match data with color
    vals <- reactiveValues(
        eruptions = df[, 1],
        row_color = rep('white', 5)
    )
    
    # Change row color depending on the number of highlighted rows
    observeEvent(input$highlight, {
        vals$eruptions <-
            c(vals$eruptions[1:input$highlight],
              vals$eruptions[input$highlight+1:length(vals$eruptions)])
        vals$row_color <- c(rep('yellow', input$highlight),
                            rep('white', length(vals$eruptions) - input$highlight))
    })
    
    # draw the table with the specified number of rows highlighted
    output$table1 <- renderDataTable({
        datatable(df) |> formatStyle(
            'eruptions', target = 'row',
            backgroundColor = styleEqual(vals$eruptions,
                                         vals$row_color,
                                         default = 'white')
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
