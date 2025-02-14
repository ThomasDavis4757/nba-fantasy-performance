#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)











function(input, output, session) {
  
  
  
  
  observeEvent(input$show_input, {
    
    output$conditional_input <- renderUI({
      textInput("dynamic_input", "Enter Text:")
    })
    
    
  })
  
  
  

  player_specific_factor <-  function(name) {
    df <- nba_data %>%
      mutate(IsItPlayer = str_detect(Starters, name)) |> 
      filter(DidNotPlay == FALSE)
    
    return(df)
  }
  
  player_specific_data <- function(name) {
    
    returned_df <- nba_data |> 
      filter(DidNotPlay == FALSE) |> 
      filter(Starters == name) 
      
  }
  
  
  model <- function(name) {
    #model <- lm("FantasyPoints ~  `OppFantDefense: ALL` +  AvgFantPoints + InjTeamateCount + Pos1 + AvgMPTimeLast10 + AvgFantPointsLast10 + IsItPlayer ", data = player_specific_factor())
    model <- lmer("FantasyPoints ~  AvgFantPoints + InjTeamateCount + Pos1 + AvgMPTimeLast10 + AvgFantPointsLast10 + (1|Starters) + (1|OpponentTeamAbv) ", data = player_specific_factor(name))
    return(model)
  }
  
  
  
  model_summary <- function(name) {
    #return(summary(model(name)))
    return(summary(loaded_model))
  }
  
  
  next5gamesStdev <- function(name) {
    model_stdev = loaded_model
    
    init_player_info <- initial_player_info(name)
    
    stdev <- as_tibble(VarCorr(model_stdev))[3,5] |> pull()
    
    average <- year_games |> 
      filter(HomeTeamAbv == init_player_info$team | AwayTeamAbv == init_player_info$team) |> 
      filter(Date >= as.character(Sys.Date())) |> 
      mutate(OpposingTeamAbv = if_else(HomeTeamAbv == init_player_info$team, AwayTeamAbv, HomeTeamAbv)) |> 
      rowwise() |>  
      mutate(OppDefRatingAll = getLatestTeamDefRating(OpposingTeamAbv)) |> 
      mutate(OppWinPercentage = getLatestTeamWinPercentage(OpposingTeamAbv)) |> 
      mutate(FantasyPointPrediction = predict(
        model_stdev, 
        tibble( 
          "OppFantDefense: ALL" = OppDefRatingAll,
          #"OpponentWinPercentage" = OppWinPercentage,
          AvgFantPoints = init_player_info$AvgFantPoints,
          InjTeamateCount = init_player_info$PrevInjuredCount,
          Pos1 = init_player_info$Position,
          AvgMPTimeLast10 = init_player_info$AvgLast10MP,
          AvgFantPointsLast10 = init_player_info$AvgFantPointsLast10,
          Starters = init_player_info$playerName,
          OpponentTeamAbv = OpposingTeamAbv,
          IsItPlayer = TRUE
        )
      )) |> 
      ungroup() |> 
      head(1) |> 
      pull(FantasyPointPrediction)
    
    return(c(stdev,average))
    
  }
  
  graph_normal <- function(average, stdev, name, binwidth = 2) {
    x <- seq(average - 4*stdev, average + 4*stdev, length.out = 100)
    y <- dnorm(x, mean = average, sd = stdev)
    
    normal_df <- data.frame(x, y)
    
    p <- ggplot() +
      geom_histogram(data = player_specific_data(name), aes(x = FantasyPoints, y = ..density..), 
                     binwidth = binwidth, fill = "gray", alpha = 0.5, color = "black") +
      geom_line(data = normal_df, aes(x, y), color = "blue", size = 1) +
      labs(title = paste0(name, " Historical and Predicted Fantasy Points"),
           x = "Fantasy Points",
           y = "Density") +
      theme_minimal() +
      ylim(0, .15)
    
    ggplotly(p)
  }
  
  
  
  graph_normal_new <- function(average1, stdev1, average2, stdev2, average3, stdev3) {
    library(ggplot2)
    library(plotly)
    library(dplyr)
    
   
    x_min <- min(average1 - 4*stdev1, average2 - 4*stdev2, average3 - 4*stdev3)
    x_max <- max(average1 + 4*stdev1, average2 + 4*stdev2, average3 + 4*stdev3)
    x <- seq(x_min, x_max, length.out = 200)
    
    
    df <- data.frame(
      x = rep(x, 3),
      y = c(dnorm(x, mean = average1, sd = stdev1),
            dnorm(x, mean = average2, sd = stdev2),
            dnorm(x, mean = average3, sd = stdev3)) * 100,
      Distribution = rep(c(player_name1(), player_name2(), player_name3()), each = length(x))
    )
    
    
    p <- ggplot(df, aes(x, y, color = Distribution)) +
      geom_line(size = 1) +
      labs(title = "Normal Distributions",
           x = "Fantasy Points",
           y = "Probability Density (%)",
           color = "Distributions") +
      scale_x_continuous(limits = c(-5, 70)) +  
      scale_y_continuous(labels = scales::percent_format(scale = 1))  +
      theme_minimal()
    
    ggplotly(p)  
  }
  
  
  
  next_5_games <- function(name) {
    
    init_play_info  <- initial_player_info(name)
    
    year_games |> 
      filter(HomeTeamAbv == init_play_info$team | AwayTeamAbv == init_play_info$team) |> 
      filter(Date >= as.character(Sys.Date())) |> 
      mutate(OpposingTeamAbv = if_else(HomeTeamAbv == init_play_info$team, AwayTeamAbv, HomeTeamAbv)) |> 
      head(5) |> 
      rowwise() |>  
      mutate(OppDefRatingAll = getLatestTeamDefRating(OpposingTeamAbv)) |> 
      mutate(OppWinPercentage = getLatestTeamWinPercentage(OpposingTeamAbv)) |> 
      mutate(FantasyPointPrediction = predict(
        loaded_model, 
        tibble(
          "OppFantDefense: ALL" = OppDefRatingAll,
          #"OpponentWinPercentage" = OppWinPercentage,
          AvgFantPoints = init_play_info$AvgFantPoints,
          InjTeamateCount = init_play_info$PrevInjuredCount,
          Pos1 = init_play_info$Position,
          AvgMPTimeLast10 = init_play_info$AvgLast10MP,
          AvgFantPointsLast10 = init_play_info$AvgFantPointsLast10,
          Starters = init_play_info$playerName,
          OpponentTeamAbv = OpposingTeamAbv,
          IsItPlayer = TRUE
        )
      )) |> 
      ungroup()
      
  }
  
  
  nextGame <- function(name) {
    
    init_play_info  <- initial_player_info(name)
    
    game_info <- year_games |> 
      filter(HomeTeamAbv == init_play_info$team | AwayTeamAbv == init_play_info$team) |> 
      filter(Date >= as.character(Sys.Date())) |> 
      mutate(OpposingTeam = if_else(HomeTeamAbv == init_play_info$team, AwayTeam, HomeTeam)) |> 
      head(1)  
    
    
    
    game_date <- paste0(game_info$`Month#`, "-", game_info$Day, "-", game_info$Year)
    
    return(c(OpposingTeam = game_info$OpposingTeam, Date = game_date))
  }
  
  
  initial_player_info <- function(name) {
    df <- plot_data1(name)  
    if (nrow(df) > 0) {
      latest_row <- df[which.max(df$Date), ]  
      list(
        playerName = name,
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
  }
  
  plot_data1 <- function(name) {
    player_specific_factor(name) |> 
      filter(`Starters` == name) 
  }
  

  update_picture1 <- function(name) {
    picture_link <- nba_data |> 
      filter(`Starters` == name) |> 
      pull(PhotoLink)
    
    if (length(picture_link) > 0) {
      return(picture_link[1])  
    } else {
      return(NULL)
    }
  }
  
  
  player_name1 <- eventReactive(input$update_btn1, {
    nameSelection(input$playerName1)
  })
  
  player_name2 <- eventReactive(input$update_btn1, {
    nameSelection(input$playerName2)
  })
  
  player_name3 <- eventReactive(input$update_btn1, {
    nameSelection(input$playerName3)
  })
  
  player_average1 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName1)) |> 
      
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = mean(FantasyPoints)) |> 
      pull(average_fantasy)
    
    
    #return(averageFantPoints)
  })
  
  player_average2 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName2)) |> 
     
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = mean(FantasyPoints)) |> 
      pull(average_fantasy)
    
    
    #return(averageFantPoints)
  })
  
  player_average3 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName3)) |> 
      
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = mean(FantasyPoints)) |> 
      pull(average_fantasy)
    
    
    return(averageFantPoints)
  })
  
  player_stdev1 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName1)) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(stdev_fantasy = sd(FantasyPoints)) |> 
      pull(stdev_fantasy)
    
    
    return(averageFantPoints)
  })
  
  player_stdev2 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName2)) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(stdev_fantasy = sd(FantasyPoints)) |> 
      pull(stdev_fantasy)
    
    
    return(averageFantPoints)
  })
  
  player_stdev3 <- eventReactive(input$update_btn1, {
    averageFantPoints <- nba_data |> 
      filter(Starters == nameSelection(input$playerName3)) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(stdev_fantasy = sd(FantasyPoints)) |> 
      pull(stdev_fantasy)
    
    
    return(averageFantPoints)
  })
  
  
  player_played_count1 <- eventReactive(input$update_btn1, {
    played <- nba_data |> 
      filter(Starters == nameSelection(input$playerName1)) |> 
      summarize(
        games_played = sum(!DidNotPlay, na.rm = TRUE),  
        total_games = n()  
      ) |> 
      mutate(result = paste0(games_played, "/", total_games)) |> 
      pull(result)
    
    return(played)
  })
  
  player_played_count2 <- eventReactive(input$update_btn1, {
    played <- nba_data |> 
      filter(Starters == nameSelection(input$playerName2)) |> 
      summarize(
        games_played = sum(!DidNotPlay, na.rm = TRUE),
        total_games = n()
      ) |> 
      mutate(result = paste0(games_played, "/", total_games)) |> 
      pull(result)
    
    return(played)
  })
  
  player_played_count3 <- eventReactive(input$update_btn1, {
    played <- nba_data |> 
      filter(Starters == nameSelection(input$playerName3)) |> 
      summarize(
        games_played = sum(!DidNotPlay, na.rm = TRUE),
        total_games = n()
      ) |> 
      mutate(result = paste0(games_played, "/", total_games)) |> 
      pull(result)
    
    return(played)
  })
  
 
  
  output$normalPlot <-  renderPlotly({
    
    average1 <- next5gamesStdev(player_name1())[2]  
    stdev1 <- next5gamesStdev(player_name1())[1]
    average2 <- next5gamesStdev(player_name2())[2]   
    stdev2 <- next5gamesStdev(player_name2())[1] 
    average3 <- next5gamesStdev(player_name3())[2]   
    stdev3 <- next5gamesStdev(player_name3())[1] 
    
    graph_normal_new(average1, stdev1, average2, stdev2, average3, stdev3 )
  })
  
  output$normalPlot1 <-  renderPlotly({
    
    req(player_name1())  
    req(next5gamesStdev(player_name1())) 
    
    average1 <- next5gamesStdev(player_name1())[2]  
    stdev1 <- next5gamesStdev(player_name1())[1]
    
    
    graph_normal(average1, stdev1, player_name1())
  })
  
  output$normalPlot2 <-  renderPlotly({
    
    req(player_name2())  
    req(next5gamesStdev(player_name2())) 
    
    average2 <- next5gamesStdev(player_name2())[2]  
    stdev2 <- next5gamesStdev(player_name2())[1]
    
    
    graph_normal(average2, stdev2, player_name2())
  })
  
  output$normalPlot3 <-  renderPlotly({
    
    req(player_name3())  
    req(next5gamesStdev(player_name3())) 
    
    average3 <- next5gamesStdev(player_name3())[2]  
    stdev3 <- next5gamesStdev(player_name3())[1]
    
    
    graph_normal(average3, stdev3, player_name3())
  })
  
  output$titlePlayerName1 <- renderText({
    player_name1()
  })
  
  output$titlePlayerName2 <- renderText({
    player_name2()
  })
  
  output$titlePlayerName3 <- renderText({
    player_name3()
  })
  
  output$Player1 <- renderText({
    'Player #1'
  })
  
  output$Player2 <- renderText({
    'Player #2'
  })
  
  output$Player3 <- renderText({
    'Player #3'
  })
 
  
  output$player1_stats <- renderTable({
    data.frame(
      Metric = c("Avg Fantasy Points", "Standard Deviation", "Games Played", "Next Match","Next Match Date"),
      Value = c(round(player_average1(),2), round(player_stdev1(),2), player_played_count1(), nextGame(player_name1())["OpposingTeam"], nextGame(player_name1())["Date"])  
    )
  }, bordered = TRUE, striped = TRUE)
  
  output$player2_stats <- renderTable({
    data.frame(
      Metric = c("Avg Fantasy Points", "Standard Deviation", "Games Played", "Next Match","NextMatchDate"),
      Value = c(round(player_average2(),2), round(player_stdev2(),2), player_played_count2(), nextGame(player_name2())["OpposingTeam"], nextGame(player_name2())["Date"])
    )
  }, bordered = TRUE, striped = TRUE)
  
  output$player3_stats <- renderTable({
    data.frame(
      Metric = c("Avg Fantasy Points", "Standard Deviation", "Games Played", "Next Match","Next Match Date"),
      Value = c(round(player_average3(),2), round(player_stdev3(),2), player_played_count3(), nextGame(player_name3())["OpposingTeam"], nextGame(player_name3())["Date"])
    )
  }, bordered = TRUE, striped = TRUE)

  
  
  #output$nbaData1 <- renderDataTable({
  #  req(plot_data1(player_name1()))  
  #  datatable(plot_data1(player_name1()), options = list(pageLength = 5))
  #})
  
  #output$nbaData2 <- renderDataTable({
  #  req(plot_data1(player_name2()))  
  #  datatable(plot_data1(player_name2()), options = list(pageLength = 5))
  #})
  
  #output$nbaData3 <- renderDataTable({
  #  req(plot_data1(player_name3()))  
  #  datatable(plot_data1(player_name3()), options = list(pageLength = 5))
  #})
  

  output$image_html1 <- renderUI({
    req(update_picture1(player_name1()))  
    HTML(glue('<img src="{update_picture1(player_name1())}" width="150" height="200">'))
  })
  
  output$image_html2 <- renderUI({
    req(update_picture1(player_name2()))  
    HTML(glue('<img src="{update_picture1(player_name2())}" width="150" height="200">'))
  })
  
  output$image_html3 <- renderUI({
    req(update_picture1(player_name3()))  
    HTML(glue('<img src="{update_picture1(player_name3())}" width="150" height="200">'))
  })
  
  
  output$average1 <- renderValueBox({
    valueBox(
      value = round(player_average1(), 2),
      subtitle = "Average Fantasy Points", 
      color = 'navy',
      width = 4  
    )
  })
  
  output$average2 <- renderValueBox({
    valueBox(
      value = round(player_average2(), 2), 
      subtitle = "Average Fantasy Points", 
      color = 'navy',
      width = 4
    )
  })
  
  output$average3 <- renderValueBox({
    valueBox(
      value = round(player_average3(), 2), 
      subtitle = "Average Fantasy Points", 
      color = 'navy',
      width = 4
    )
  })
  
  output$regressionPlot1 <- renderPlot({
    nba_data |> 
      filter(`Starters` == player_name1()) |> 
    
      filter(DidNotPlay == FALSE) |> 
      ggplot(aes(x=.data[["FantasyPoints"]], y=.data[[input$predictVar1]])) +
      geom_point()
      
      
  })
  
  output$hola <- renderText ({
    "Hello"
  })
  
  output$modelSummary1 <- renderPrint({
    model_summary(player_name1())
  })
  
  output$modelSummary2 <- renderPrint({
    model_summary(player_name2())
  })
  
  output$modelSummary3 <- renderPrint({
    model_summary(player_name3())
  })
  
  
  output$modelAvgStd1 <- renderPrint({
    next5gamesStdev(player_name1())
  })
  
  output$modelAvgStd2 <- renderPrint({
    next5gamesStdev(player_name2())
  })
  
  output$modelAvgStd3 <- renderPrint({
    next5gamesStdev(player_name3())
  })
  
  
  output$player_info_table1 <- renderTable({
    data.frame(
      Field = names(initial_player_info(player_name1())), 
      Value = unlist(initial_player_info(player_name1()))
    )
  })
  
  output$player_info_table2 <- renderTable({
    data.frame(
      Field = names(initial_player_info(player_name2())), 
      Value = unlist(initial_player_info(player_name2()))
    )
  })
  
  output$player_info_table3 <- renderTable({
    data.frame(
      Field = names(initial_player_info(player_name3())), 
      Value = unlist(initial_player_info(player_name3()))
    )
  })
  
  
  output$futurePlayerGames1 <- renderDataTable({
    req(next_5_games(player_name1()))  
    datatable(next_5_games(player_name1()), options = list(pageLength = 5))
  })
  
  output$futurePlayerGames2 <- renderDataTable({
    req(next_5_games(player_name2()))  
    datatable(next_5_games(player_name2()), options = list(pageLength = 5))
  })
  
  output$futurePlayerGames3 <- renderDataTable({
    req(next_5_games(player_name3()))  
    datatable(next_5_games(player_name3()), options = list(pageLength = 5))
  })
  
  
  
################################################################################
  
  
  plot_data <- eventReactive(input$update_btn, {
    nba_data |> 
      filter(Date > input$dates[1]) |> 
      filter(Date < input$dates[2]) |> 
      filter(DidNotPlay == FALSE)
  })
  
  
  
  model_alldata <- function(columnname) {
    model_text <- glue("FantasyPoints ~ `", input$predictVar, "`")
    model <- lm(model_text, data = nba_data)
    
  }
  
  model_alldata_summary <- function(columnname) {
    return(summary(model_alldata(columnname)))
  }

  
  get_pvalue <- eventReactive(input$update_btn, {
    summary(model_alldata(input$predictVar))$coefficients[2,4]
  })
  
  output$get_pvalues <- renderPrint({
    p_value <- get_pvalue()
    print(p_value)
    
  })
  
  output$pvalue_significance <- renderText({
    get_pvalue_significance(get_pvalue())$text
  })
  
  output$progress_bar_ui <- renderUI({
    
    var1 <- "FantasyPoints"
    var2 <- input$predictVar
    correlation_value <- cor(plot_data()[[var1]], plot_data()[[var2]], method = "pearson", use = "complete.obs")
    correlation_percent <- abs(correlation_value) * 100  
    bar_status <- ifelse(correlation_value >= 0, "success", "danger") 
    progressBar(id = "correlation_bar", value = correlation_percent, total = 100, display_pct = TRUE, status = bar_status)
  })
  
  get_pvalue_significance <- function(p_value) {
    if (p_value < 0.001) {
      text <- glue("There is an extremely significant relationship. P-value: {round(p_value,2)}")
      fill_percentage <- 100
    } else if (p_value < 0.01) {
      text <- glue("There is a very significant relationship. P-value: {round(p_value,2)}")
      fill_percentage <- 80
    } else if (p_value < 0.05) {
      text <- glue("There is a significant relationship. P-value: {round(p_value,2)}")
      fill_percentage <- 60
    } else if (p_value < 0.1) {
      text <- glue("The relationship is not really significant. P-value: {round(p_value,2)}")
      fill_percentage <- 40
    } else {
      text <- glue("The relationship does not have significance. P-value: {round(p_value,2)}")
      fill_percentage <- 20
    }
    
    return(list(text = text, fill = fill_percentage))
  }
  
  
  output$image_html <- renderUI({
    req(update_picture())  
    HTML(glue('<img src="{update_picture()}" width="150" height="200">'))
  })
  
  
  output$modelSummaryNew1 <- renderPrint({
    model_alldata_summary(input$predictVar)
  })
  
  output$regressionPlot <- renderPlot({
    plot_data() |> 
      ggplot(aes(x=.data[["FantasyPoints"]], y=.data[[input$predictVar]])) +
      geom_point()
    
    
  })
  
  output$correlationOutput <- renderText({
    var1 <- "FantasyPoints"
    var2 <- input$predictVar
    
   
    correlation_value <- cor(plot_data()[[var1]], plot_data()[[var2]], method = "pearson", use = "complete.obs")
    
    paste("Correlation between", var1, "and", var2, "is:", round(correlation_value, 2))
  })

  
  #output$pValue <- renderText({
  #  summary(fit)$coefficients[,4]
  #})
  
  

##################################################################################


  
  
  player_specific_data3_stats <- function(name) {
    
    returned_df <- nba_data |> 
      filter(Starters == name) |> 
      filter(Date > input$dates1[1]) |> 
      filter(Date < input$dates1[2])
    
  }
  
  player_name_stats <- eventReactive(input$update_btn3, {
    nameSelection(input$playerName)
  })
  
  average3_stats <- function(name) {
    
    average <- player_specific_data3_stats(name) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = mean(FantasyPoints)) |> 
      pull(average_fantasy)
      
      
  }
  

  
  average_minutes_played <- function(name) {
    
    avg_time <- player_specific_data3_stats(name) |> 
      filter(DidNotPlay == FALSE) |> 
      mutate(minutes_played = as.duration(hms(TimePlayed))) |>  
      summarize(average_time = mean(minutes_played, na.rm = TRUE)) |>  
      pull(average_time)
    
    
    avg_mins <- as.integer(avg_time %/% dminutes(1))  
    avg_secs <- as.integer(round(avg_time %% dminutes(1) / dseconds(1)))  
    
    return(sprintf("%02d:%02d", avg_mins, avg_secs))  
  }
  
  stdev3_stats <- function(name) {
    
    stdev <- player_specific_data3_stats(name) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(average_fantasy = sd(FantasyPoints)) |> 
      pull(average_fantasy)
    
    
  }
  
  player_played_count_stats <- function(name) {
    played <- player_specific_data3_stats(name) |> 
      summarize(
        games_played = sum(!DidNotPlay, na.rm = TRUE),  
        total_games = n()  
      ) |> 
      mutate(result = paste0(games_played, "/", total_games)) |> 
      pull(result)
    
    return(played)
  }
  
  
  
  player_fantasy_stats <- function(name) {  
    
    data.frame(
      Stat = c("Average Fantasy Points", "Fantasy Points Std Dev", "Games Played / Total"),
      Value = c(
        round(average3_stats(name),2),
        round(stdev3_stats(name),2),
        #average_minutes_played(name),
        player_played_count_stats(name)
      )
    )
  }
  
  main_stats <- function(name) {
    
    main <- player_specific_data3_stats(name) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(
        average_points = mean(PTS),
        average_assists = mean(AST),
        average_rebounds = mean(TRB),
        average_blocks = mean(BLK),
        average_steals = mean(STL),
        average_turnovers = mean(TOV)
      ) 
    
    return(setNames(as.numeric(main), names(main)))  
  }
  
  
  shooting_stats <- function(name) {
    
    main <- player_specific_data3_stats(name) |> 
      filter(DidNotPlay == FALSE) |> 
      summarize(
        average_shots_made = mean(FG),
        average_shots_taken = mean(FGA),
        average_shot_perc = mean(`FG%`),
        average_threep_made = mean(`3P`),
        average_threep_taken = mean(`3PA`),
        average_threep_perc = mean(`3P%`),
        average_freethrow_made = mean(FTA),
        average_freethrow_taken = mean(FT),
        average_freethrow_perc = mean(`FT%`),

      ) 
    
    return(setNames(as.numeric(main), names(main)))  
  }
  
 
  output$player_stats_table <- renderTable({
    data.frame(
      Stat = names(main_stats(player_name_stats())),  
      Value = unlist(main_stats(player_name_stats())) 
    )
  })
  
  output$player_stats_table_shooting <- renderTable({
    data.frame(
      Stat = names(shooting_stats(player_name_stats())),  
      Value = unlist(shooting_stats(player_name_stats())) 
    )
  })
  
  output$player_stats_table_fantasy <- renderTable({
    player_fantasy_stats(player_name_stats())
  })
  

  output$playerNameStats <- renderText ({
    player_name_stats()
  })

  
  
  output$playerImage <- renderUI({
    req(update_picture1(player_name_stats()))  
    HTML(glue('<img src="{update_picture1(player_name_stats())}" width="225" height="300">'))
  })
  
  
}
