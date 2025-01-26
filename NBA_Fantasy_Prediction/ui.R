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
            tabPanel("Map", fluid = TRUE, sidebarLayout(
              sidebarPanel(
                selectInput(
                  "Country",
                  "Select Country",
                  choices = "",
                  selected = ""
                ),
                
                textInput('playerName', label = h3("Type in Player Name")),
                dateRangeInput("dates", label = h3("Date range"), start = "2024-10-22", end = as.character(Sys.Date()), min = "2024-10-22", max = as.character(Sys.Date())),
                
                actionButton("update_btn", "Update Graph")
              ),
              mainPanel(
                fluidRow(
                textOutput("titlePlayerName"),
                uiOutput("image_html")
                ),
                dataTableOutput("nbaData")
              )
              
              )
            )),
            tabPanel("plot", fluid = TRUE, )
          )
  

