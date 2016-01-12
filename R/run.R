rm(list = ls())

### packages
library(dplyr)
library(magrittr)
library(lme4)
library(readr)

### functions
source("R/functions.R")

getFit <- function(year) {

  datasets <- c("acs", "cps", "polls")

  data <- datasets %>%
    sprintf("data/%s/%s.csv", year, .) %>%
    lapply(read_csv) %>%
    setNames(datasets)

  ### load / fit models
  model <- getModels(data, year)

  acs <- cbind(data$acs, sapply(model, predict, data$acs,
                                allow.new.levels = T))

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

  saveRDS(fits, sprintf("out/fits-%s.rds", year))

  fits
}

# fit models
fits <- c(2004, 2012) %>%
  lapply(function(yr) mutate(getFit(yr), year = yr)) %>%
  bind_rows()

# get summary stats
summary <- fits %>%
  group_by(state, race_eth, year) %>%
  summarize(demVote = weighted.mean(demFinal, turnoutFinal),
            repVote = weighted.mean(repFinal, turnoutFinal))

write_csv(summary, "out/summary.csv")
