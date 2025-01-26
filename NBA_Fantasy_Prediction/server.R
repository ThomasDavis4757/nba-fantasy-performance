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
    nba_data |> 
      filter(`Starters` == nameSelection(input$playerName)) |> 
      filter(Date > input$dates[1]) |> 
      filter(Date < input$dates[2])
  })
  

  update_picture <- eventReactive(input$update_btn, {
    picture_link <- nba_data |> 
      filter(`Starters` == nameSelection(input$playerName)) |> 
      pull(PhotoLink)
    
    if (length(picture_link) > 0) {
      return(picture_link[1])  
    } else {
      return(NULL)
    }
  })
  
  
  player_name <- eventReactive(input$update_btn, {
    nameSelection(input$playerName)
  })
  
  output$titlePlayerName <- renderText({
    player_name()
  })
  
 
  output$nbaData <- renderDataTable({
    req(plot_data())  
    datatable(plot_data(), options = list(pageLength = 5))
  })
  

  output$image_html <- renderUI({
    req(update_picture())  
    HTML(glue('<img src="{update_picture()}" width="150" height="200">'))
  })
  
}
