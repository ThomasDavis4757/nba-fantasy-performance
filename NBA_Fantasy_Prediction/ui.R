#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)


fluidPage(
  theme = shinythemes::shinytheme('superhero'),
  tags$head(
    tags$style(HTML("
      .nav-tabs > li > a {
        border: 2px solid white !important;
        border-radius: 5px;
      }
      
      .graph-container {
        border: 3px solid #0D47A1;  /* Dark blue border */
        padding: 10px;
        border-radius: 5px;
        margin-bottom: 10px;
      }
      
      .table-container table {
        border: 2px solid white;
      }
    "))
  ),
  titlePanel("NBA Player Fantasy Prediction"),
  tabsetPanel(
    tabPanel("Individual Player Correlation", fluid = TRUE, 
             br(),
             sidebarLayout(
               sidebarPanel(
                 textInput('playerName1', label = h3("Player #1")),
                 textInput('playerName2', label = h3("Player #2")),
                 textInput('playerName3', label = h3("Player #3")),
                 
                 actionButton("update_btn1", "Start Analysis")
               ),
               mainPanel(
                 div(style = "overflow-x: auto; white-space: nowrap; display: flex;",
                     
                     div(style = "min-width: 650px; padding: 10px;",
                         div(style = "display: flex; align-items: center;",  
                             
                             div(style = "flex: 1; text-align: center;",
                                 textOutput("titlePlayerName1"),
                                 br(),
                                 uiOutput("image_html1")
                             ),
                             
                             div(style = "flex: 1;",
                                 tableOutput("player1_stats")  
                             ),
                             div(style= "flex: 1;",
                                 textInput("prevGamePoints1","Enter Fantasy Points to Compare"),
                                 tableOutput("chances of abovetable1"))
                         ),
                         br(),
                         plotlyOutput("normalPlot1", height = "400px", width = "100%")  
                     ),
                     
                     
                     div(style = "min-width: 650px; padding: 10px;",
                         div(style = "display: flex; align-items: center;",
                             div(style = "flex: 1; text-align: center;",
                                 textOutput("titlePlayerName2"),
                                 br(),
                                 uiOutput("image_html2")
                             ),
                             div(style = "flex: 1;",
                                 tableOutput("player2_stats")
                             ),
                             
                             div(style= "flex: 1;",
                                 textInput("prevGamePoints2","Enter Fantasy Points to Compare"),
                                 tableOutput("chances of abovetable2"))
                         ),
                         br(),
                         plotlyOutput("normalPlot2", height = "400px", width = "100%")
                     ),
                     
                     
                     div(style = "min-width: 650px; padding: 10px;",
                         div(style = "display: flex; align-items: center;",
                             div(style = "flex: 1; text-align: center;",
                                 textOutput("titlePlayerName3"),
                                 br(),
                                 uiOutput("image_html3")
                             ),
                             div(style = "flex: 1;",
                                 tableOutput("player3_stats")
                             ),
                             
                             div(style= "flex: 1;",
                                 textInput("prevGamePoints3","Enter Fantasy Points to Compare"),
                                 tableOutput("chances of abovetable3")),
                         ),
                         br(),
                         plotlyOutput("normalPlot3", height = "400px", width = "100%")
                     )
                 )
               )
               
             )
    ),
    tabPanel("Overall Fantasy Correlations", fluid = TRUE, 
             br(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("predictVar", label = "Select Prediction Variable:", choices = selected_columns),
                 dateRangeInput("dates", label = h3("Date range"), start = "2024-10-22", end = as.character(Sys.Date()), min = "2024-10-22", max = as.character(Sys.Date())),
                 #selectInput("predictVar", label = "Select Prediction Variable:", choices = colnames(nba_data)),
                 actionButton("update_btn", "Update Graph")
               ),
               mainPanel(
                 
                 #valueBoxOutput("averageBox"),
                 plotOutput("regressionPlot"),
                 #verbatimTextOutput("modelSummaryNew1"),
                 #div(style = "font-size: 24px; font-weight: bold;", textOutput("correlationOutput")),
                 #verbatimTextOutput("get_pvalues"),
                 div(style = "font-size: 20px;", textOutput("pvalue_significance")),
                 uiOutput("progress_bar_ui")
               )
             )
    ),
    tabPanel("Player Specific Stats", fluid = TRUE, 
             br(),
             sidebarLayout(
               sidebarPanel(
                 textInput('playerName', label = h3("Type in Player Name")),
                 dateRangeInput("dates1", label = h3("Date range"), start = "2024-10-22", end = as.character(Sys.Date()), min = "2024-10-22", max = as.character(Sys.Date())),
                 actionButton("update_btn3", "Submit")
               ),
               mainPanel(
                 textOutput("playerNameStats"),
                 br(),
                 uiOutput("playerImage"),
                 br(),
                 
                 fluidRow(
                   column(6, tableOutput("player_stats_table")),
                   column(6, tableOutput("player_stats_table_shooting"))
                 ),
                 br(),
                 
                 tableOutput("player_stats_table_fantasy")
               )
             )
             
    )
  )
)