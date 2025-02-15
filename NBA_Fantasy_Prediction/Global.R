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





model_data <- nba_data |> 
  filter(DidNotPlay == FALSE)

model_saved <- lmer("FantasyPoints ~  AvgFantPoints + InjTeamateCount + Pos1 + AvgMPTimeLast10 + AvgFantPointsLast10 + (1|Starters) + (1|OpponentTeamAbv) ", data = model_data)
summary(model_saved)


saveRDS(model_saved, file = "./nba_data_model.rds")



loaded_model <- readRDS("./nba_data_model.rds")

loaded_model

nba_data <- read_csv('./data/full_nba_data.csv')

nba_data <- nba_data |> mutate(Date = as.Date(Date)) |> 
  mutate(tens = (PTS >= 10) + (TRB >= 10) + (AST >= 10) + (STL >= 10) + (BLK >= 10)) 

selected_columns <- c(
  "Points" = "PTS",
  "Assists" = "AST",
  "Rebounds" = "TRB",
  "Blocks" = "BLK",
  "Steals" = "STL",
  "Turnovers" = "TOV",
  "Free Throws Made" = "FT",
  "Free Throws Attempted" = "FTA",
  "Free Throw %" = "FT%",
  "Field Goals Made" = "FG",
  "Field Goals Attempted" = "FGA",
  "Field Goal %" = "FG%",
  "3-Point Makes" = "3P",
  "3-Point Attempts" = "3PA",
  "3-Point %" = "3P%",
  "Team" = "TeamAbv",
  "Opponent Team" = "OpponentTeamAbv",
  "Injured Teammates" = "InjTeamateCount",
  "Opponent Injured Teammates" = "OpponentInjTeamateCount",
  "Position" = "Pos1",
  "Attendance" = "Attend.",
  "Game Point Difference" = "GamePointDiff",
  "Opponent Fantasy Defense" = "OppFantDefense: ALL"
)



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
