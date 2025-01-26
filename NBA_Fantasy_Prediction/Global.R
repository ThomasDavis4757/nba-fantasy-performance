library(tidyverse)
library(shiny)
library(glue)
library(stringdist)
library(shinythemes)
library(rsconnect)
library(plotly)
library(DT)




nba_data <- read_csv('./data/full_nba_data.csv')



nameSelection <- function(inputName) {
  
  playerNames <- nba_data |> 
    distinct(`Starters`) |> 
    pull(`Starters`)
  
  
  closest_match <- playerNames[which.min(stringdist(inputName, playerNames))]
  return(closest_match)
}