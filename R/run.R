rm(list = ls())

### packages
library(dplyr)
library(magrittr)
library(lme4)
library(readr)

### functions
source("R/functions.R")

#### load data
year <- 2004

datasets <- c("acs", "cps", "polls")

data <- datasets %>%
  sprintf("data/%s/%s.csv", year, .) %>%
  lapply(read_csv) %>%
  setNames(datasets)

### load / fit models
model <- getModels(data, year)

acs <- cbind(data$acs, sapply(model, predict, data$acs, allow.new.levels = T))

### predict initial support and turnout models
fits <- acs %>%
  mutate(adjust = 0) %>%

  adjust("turnout.acs.cvap", "turnout", "Freq") %>%
  mutate(turnoutFinal = plogis(turnout + adjust) * Freq) %>%

  adjust("otherVoteShare", "threeParty", "turnoutFinal") %>%
  mutate(minorFinal = plogis(threeParty + adjust) * turnoutFinal,
         majorFinal = turnoutFinal - minorFinal) %>%

  adjust("demTwoPartyShare", "twoParty", "majorFinal") %>%
  mutate(demFinal = plogis(twoParty + adjust) * majorFinal,
         repFinal = majorFinal - demFinal) %>%
  ungroup %>%
  mutate_each(funs(divide_by(., turnoutFinal)),
              demFinal, repFinal, minorFinal)

### summary stats
summary <- fits %>%
  group_by(state, race_eth) %>%
  summarize(Obama = weighted.mean(demFinal, turnoutFinal),
            Romney = weighted.mean(repFinal, turnoutFinal))

write_csv(summary, "out/summary.csv")
