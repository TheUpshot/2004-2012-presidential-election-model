rm(list = ls())

### packages
library(dplyr)
library(ggplot2)
library(magrittr)
library(lme4)
library(readr)

### functions
source("R/functions.R")

getFit <- function(year, force = F) {

  datasets <- c("acs", "cps", "polls")

  data <- datasets %>%
    sprintf("data/%s/%s.csv", year, .) %>%
    lapply(read_csv) %>%
    setNames(datasets)

  ### load / fit models
  model <- getModels(data, year, force)

  acs <- cbind(data$acs, sapply(model, predict, data$acs,
                                allow.new.levels = T))

  ### predict initial support and turnout models
  fits <- acs %>%
    mutate(adjust = 0, year = year) %>%

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

}

# fit models
fits <- c(2004, 2012) %>%
  sapply(getFit) %>%
  bind_rows()

# get summary stats
summary <- fits %>%
  group_by(state, race_eth, year) %>%
  summarize(demVote = weighted.mean(demFinal, turnoutFinal),
            repVote = weighted.mean(repFinal, turnoutFinal),
            pop = sum(Freq),
            turnout = sum(turnoutFinal) / pop)

# plot
ggplot(summary, aes(x = demVote, y = turnout, color = race_eth,
                    size = pop)) +
  geom_point(alpha = .65) +
  scale_size_area(max_size = 15) +
  facet_grid(year ~ .) +
  theme_bw()

write_csv(summary, "out/summary.csv")
