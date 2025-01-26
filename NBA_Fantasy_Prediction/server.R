#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram









function(input, output, session) {
  
  
  plot_data <- eventReactive(input$update_btn, {
    filtered_data()
  })
  
  
  
  
  filtered_data <- reactive({
    dataFiltered <- nba_data |> 
      filter(`Starters` == nameSelection(input$playerName))
    return(dataFiltered)
  })
  
  
  

    output$nbaData <- renderDataTable(
      datatable(
        plot_data(),
        options = list(pageLength = 5)
      )
    )
    
    output$value <- renderPrint({ input$dates })

}
