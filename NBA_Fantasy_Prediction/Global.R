library(tidyverse)
library(shiny)
library(glue)
library(stringdist)
library(rsconnect)
library(plotly)
library(DT)
library(shinydashboard)
library(car)



nba_data <- read_csv('./data/full_nba_data.csv')

nba_data <- nba_data |> mutate(Date = as.Date(Date))
year_games <- read_csv('./data/nba_game_list.csv')



nameSelection <- function(inputName) {
  
  playerNames <- nba_data |> 
    distinct(`Starters`) |> 
    pull(`Starters`)
  
  
  closest_match <- playerNames[which.min(stringdist(inputName, playerNames))]
  return(closest_match)
}



nba_data_devinb <- nba_data |> 
  filter(Starters == 'Devin Booker')


model <- lm("FantasyPoints ~ AvgFantPoints", data = nba_data)
model <- lm("FantasyPoints ~ `OppFantDefense: ALL` + AvgFantPoints  + InjTeamateCount + Pos1 + AvgMPTimeLast10", data = nba_data)
summary(model)

coeffs <- model$coefficients

new_data <- tibble(
  
  "OppFantDefense: ALL" = 2,
  AvgFantPoints = 40,
  InjTeamateCount = 3,
  Pos1 = 'C',
  AvgMPTimeLast10 = 36.5
  
)

predict(model,new_data)


summary(model)$adj.r.squared

#total_model <- lm(FantasyPoints ~ x + xx + xxx + xxxx, data = nba_data)


#oneVarModelSummaryFantPoints('MP')
