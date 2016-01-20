rm(list = ls())

### packages
library(dplyr)
library(ggplot2)
library(lme4)
library(readr)

### functions
source("R/functions.R")

getFits <- function(year, data, force = F) {

  d <- data[[as.character(year)]]

  # load / fit models
  model <- getModels(d, year, force)
  acs <- sapply(model, predict, d$acs, allow.new.levels = T) %>%
    cbind(d$acs) %>%
    mutate(year = year)

  # predict initial support and turnout models
  acs %>%
    adjust(turnoutFinal = plogis(turnout.acs.cvap + turnout) * Freq) %>%
    adjust(minorFinal = plogis(otherVoteShare + threeParty) * turnoutFinal) %>%
    mutate(majorFinal = turnoutFinal - minorFinal) %>%
    adjust(demFinal = plogis(demTwoPartyShare + twoParty) * majorFinal) %>%
    mutate(repFinal = majorFinal - demFinal) %>%
    mutate_each(funs(. / turnoutFinal), demFinal, repFinal, minorFinal)

}

# load data
years <- setNames(nm = c(2004, 2012))
data <- sapply(years, getData, simplify = F)

# fit models
fits <- sapply(years, getFits, data) %>%
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
