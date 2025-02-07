#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(
  titlePanel("NBA Player Fantasy Prediction"),
          tabsetPanel(
            tabPanel("Individual Player Correlation", fluid = TRUE, sidebarLayout(
              sidebarPanel(

                textInput('playerName1', label = h3("Type in Player Name")),
                dateRangeInput("dates1", label = h3("Date range"), start = "2024-10-22", end = as.character(Sys.Date()), min = "2024-10-22", max = as.character(Sys.Date())),
                selectInput("predictVar1", label = "Select Prediction Variable:", choices = colnames(nba_data)),
                actionButton("update_btn1", "Update Graph")
              ),
              mainPanel(
                fluidRow(
                textOutput("titlePlayerName1"),
                uiOutput("image_html1")
                ),
                dataTableOutput("nbaData1"),
                valueBoxOutput("averageBox1"),
                plotOutput("regressionPlot1"),
                verbatimTextOutput("modelSummary"),
                tableOutput("player_info_table"),
                dataTableOutput("futurePlayerGames")
                
              )
              
              )
            ),
            
            
            tabPanel("Total Player Correlation", fluid = TRUE, 
                     sidebarLayout(
                       sidebarPanel(
                         
                         textInput('playerName', label = h3("Type in Player Name")),
                         dateRangeInput("dates", label = h3("Date range"), start = "2024-10-22", end = as.character(Sys.Date()), min = "2024-10-22", max = as.character(Sys.Date())),
                         selectInput("predictVar", label = "Select Prediction Variable:", choices = colnames(nba_data)),
                         actionButton("update_btn", "Update Graph")
                       ),
                       mainPanel(
                         fluidRow(
                           textOutput("titlePlayerName"),
                           uiOutput("image_html")
                         ),
                         dataTableOutput("nbaData"),
                         valueBoxOutput("averageBox"),
                         plotOutput("regressionPlot")
                         
                       )
                       
                     )
            ),
            tabPanel("Other", fluid = TRUE, 
                     h3("This is the Player Stats tab"),
                     p("Content for Player Stats will go here.")
            )
          )
  
          )
  

