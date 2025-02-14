library(tidyverse)
library(shiny)
library(glue)
library(stringdist)
library(rsconnect)
library(plotly)
library(DT)
library(shinydashboard)
library(car)
library(lme4)
library(shinycssloaders)
library(scales)
library(lubridate)
library(dplyr)
library(shinyWidgets)

nba_data <- read_csv('./data/full_nba_data.csv')

nba_data <- nba_data |> mutate(Date = as.Date(Date)) |> 
  mutate(tens = (PTS >= 10) + (TRB >= 10) + (AST >= 10) + (STL >= 10) + (BLK >= 10)) 

selected_columns <- nba_data |> 
  select(PTS, AST, TRB, BLK, STL, TOV, FT, FTA, `FT%`, FG, FGA, `FG%`,`3P`, `3PA`, `3P%`, TeamAbv, OpponentTeamAbv, InjTeamateCount, OpponentInjTeamateCount, Pos1, `Attend.`, GamePointDiff, `OppFantDefense: ALL`) |>  # Choose the columns you want
  colnames()


year_games <- read_csv('./data/nba_game_list.csv')



nameSelection <- function(inputName) {
  
  playerNames <- nba_data |> 
    distinct(`Starters`) |> 
    pull(`Starters`)
  
  
  closest_match <- playerNames[which.min(stringdist(inputName, playerNames))]
  return(closest_match)
}



getLatestTeamDefRating <- function(teamAbv) {
  nba_data |> 
    filter(OpponentTeamAbv == teamAbv) |> 
    filter(Date == max(Date)) |> 
    pull(`OppFantDefense: ALL`) |> 
    first()
}


getLatestTeamWinPercentage <- function(teamAbv) {
  nba_data |> 
    filter(OpponentTeamAbv == teamAbv) |> 
    filter(Date == max(Date)) |> 
    pull(`OpponentWinPercentage`) |> 
    first()
}


getLatestTeamDefRating('PHO')
getLatestTeamWinPercentage('BOS')



nba_data_devinb <- nba_data |> 
  filter(Starters == 'Devin Booker')



model <- lm("FantasyPoints ~ `OppFantDefense: ALL` + `OppFantDefense: C` +  AvgFantPoints  + InjTeamateCount + Pos1 + AvgMPTimeLast10", data = nba_data)
model <- lm("FantasyPoints ~ AvgFantPoints", data = nba_data)
model <- lm("FantasyPoints ~ `OppFantDefense: ALL` + AvgFantPoints  + InjTeamateCount + Pos1 + AvgMPTimeLast10", data = nba_data)
summary(model)

coeffs <- model$coefficients

new_data <- tibble(
  
  "OppFantDefense: ALL" = 30,
  AvgFantPoints = 40,
  InjTeamateCount = 3,
  Pos1 = 'C',
  AvgMPTimeLast10 = 37.5
  
)

predict(model,new_data)


summary(model)$adj.r.squared

#total_model <- lm(FantasyPoints ~ x + xx + xxx + xxxx, data = nba_data)


#oneVarModelSummaryFantPoints('MP')
