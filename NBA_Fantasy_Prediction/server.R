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
  
  
  new_player_name1 <- eventReactive(input$update_btn1, {
    nameSelection(input$playerName1)
  })
  

  player_specific_factor <- eventReactive(input$update_btn1, {
    df <- nba_data %>%
      mutate(IsItPlayer = str_detect(Starters, new_player_name1()))
    
    return(df)
  })
  
  
  model <- eventReactive(input$update_btn1, {
    model <- lm("FantasyPoints ~ `OppFantDefense: ALL` + AvgFantPoints + InjTeamateCount + Pos1 + AvgMPTimeLast10 + AvgFantPointsLast10 +  IsItPlayer", data = player_specific_factor())
    return(summary(model))
  })
  
  
  
  next_5_games <- eventReactive(input$update_btn1, {
    year_games |> 
      filter(HomeTeamAbv == initial_player_info()$team | AwayTeamAbv == initial_player_info()$team) |> 
      filter(Date >= as.character(Sys.Date()))
  })
  
  
  initial_player_info <- eventReactive(input$update_btn1, {
    df <- plot_data1()  
    if (nrow(df) > 0) {
      latest_row <- df[which.max(df$Date), ]  
      list(
        playerName = new_player_name1(),
        team = latest_row$TeamAbv, 
        AvgFantPoints = latest_row$AvgFantPoints, 
        AvgFantPointsLast10 = latest_row$AvgFantPointsLast10,
        PrevInjuredCount = latest_row$PrevInjuredCount,
        Position = latest_row$Pos1,
        AvgLast10MP = latest_row$AvgMPTimeLast10
      )
    } else {
      NULL  
    }
  })
  
  plot_data1 <- eventReactive(input$update_btn1, {
    player_specific_factor() |> 
      filter(`Starters` == nameSelection(input$playerName1)) |> 
      filter(Date > input$dates1[1]) |> 
      filter(Date < input$dates1[2])
  })
  

  update_picture1 <- eventReactive(input$update_btn1, {
    picture_link <- nba_data |> 
      filter(`Starters` == nameSelection(input$playerName1)) |> 
      pull(PhotoLink)
    
    if (length(picture_link) > 0) {
      return(picture_link[1])  
    } else {
      return(NULL)
    }
  })
  
  
  player_name1 <- eventReactive(input$update_btn1, {
    nameSelection(input$playerName1)
  })
  
  player_average1 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName1)) |> 
      filter(Date > input$dates1[1]) |> 
      filter(Date < input$dates1[2]) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = mean(FantasyPoints)) |> 
      pull(average_fantasy)
    
    
    return(averageFantPoints)
  })
  
  
  
  
  output$titlePlayerName1 <- renderText({
    player_name1()
  })
  
 
  output$nbaData1 <- renderDataTable({
    req(plot_data1())  
    datatable(plot_data1(), options = list(pageLength = 5))
  })
  

  output$image_html1 <- renderUI({
    req(update_picture1())  
    HTML(glue('<img src="{update_picture1()}" width="150" height="200">'))
  })
  
  
  output$averageBox1 <- renderValueBox({
    valueBox(
      value = player_average1(), 
      subtitle = "Average Fantasy Points", 
      icon = icon("basketball-ball"),
      color = 'navy'
    )
  })
  
  output$regressionPlot1 <- renderPlot({
    nba_data |> 
      filter(`Starters` == new_player_name1()) |> 
      filter(Date > input$dates1[1]) |> 
      filter(Date < input$dates1[2]) |> 
      filter(DidNotPlay == FALSE) |> 
      ggplot(aes(x=.data[["FantasyPoints"]], y=.data[[input$predictVar1]])) +
      geom_point()
      
      
  })
  
  
  
  output$modelSummary <- renderPrint({
    model()
  })
  
  
  output$player_info_table <- renderTable({
    data.frame(
      Field = names(initial_player_info()), 
      Value = unlist(initial_player_info())
    )
  })
  
  
  output$futurePlayerGames <- renderDataTable({
    req(next_5_games())  
    datatable(next_5_games(), options = list(pageLength = 5))
  })
  
  
  
################################################################################
  
  
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
  
  player_average <- eventReactive(input$update_btn, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName)) |> 
      filter(Date > input$dates[1]) |> 
      filter(Date < input$dates[2]) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = mean(FantasyPoints)) |> 
      pull(average_fantasy)
    
    
    return(averageFantPoints)
  })
  
  prediction_variable <- 
    
    
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
  
  
  output$averageBox <- renderValueBox({
    valueBox(
      value = player_average(), 
      subtitle = "Average Fantasy Points", 
      icon = icon("basketball-ball"),
      color = 'navy'
    )
  })
  
  output$regressionPlot <- renderPlot({
    nba_data |> 
      #filter(`Starters` == nameSelection(input$playerName)) |> 
      filter(Date > input$dates[1]) |> 
      filter(Date < input$dates[2]) |> 
      filter(DidNotPlay == FALSE) |> 
      ggplot(aes(x=.data[["FantasyPoints"]], y=.data[[input$predictVar]])) +
      geom_point()
    
    
  })
  
  
  
  
  
}
