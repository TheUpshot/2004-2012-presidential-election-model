adjust <- function(df, target, initial, weights) {
  suppressWarnings(df %>%
    group_by(state) %>%
    do(adjust = getAdjust(., target, .[[initial]], .[[weights]])) %>%
    mutate(adjust = as.numeric(adjust)) %>%
    right_join(select(df, -adjust)))
}

getAdjust <- function(df, target, initial, weights) {
  paste(target, "~ 1") %>%
    as.formula %>%
    glm(family = binomial, data = df, weights = weights,
        offset = initial, model = F) %>%
    extract("coefficients") %>%
    first %>%
    pmin(1) %>%
    pmax(-1)
}

getModels <- function(data, year, force = F) {
  path <- sprintf("out/model-%s.rds", year)
  if (!file.exists(path) || force) {
    model <- list(
      turnout = modelTurnout(data$cps),
      twoParty = modelTwoParty(filter(data$polls, !otherVote)),
      threeParty = modelThreeParty(data$polls)
    )
    saveRDS(model, path, compress = F)
  }
  readRDS(path)
}

modelTurnout <- function(df) {
  print("-- fitting turnout model --")
  glmer(as.factor(recode) ~ turnout.acs.cvap +
          (1|age4) +
          (1|edu5) +
          (1|race_eth) +
          (1|sex) +
          (1|state) +
          (1|age4:battlegrounds) +
          (1|edu5:battlegrounds) +
          (1|race_eth:age4) +
          (1|race_eth:battlegrounds) +
          (1|race_eth:edu5) +
          (1|race_eth:region) +
          (1|race_eth:sex) +
          (1|region:edu5) +
          (1|race_eth:age4:region) +
          (1|race_eth:edu5:region) +
          (1|race_eth:age4:battlegrounds) +
          (1|race_eth:edu5:battlegrounds),
        df, family = binomial)
}

modelThreeParty <- function(df) {
  print("-- fitting third party support model --")
  glmer(otherVote ~ otherPartyShare + (1 + otherPartyShare|race_eth) +
          (1|age4) +
          (1|edu5) +
          (1|sex) +
          (1|state),
        df, family = binomial)
}

modelTwoParty <- function(df) {
  print("-- fitting two party support model --")
  glmer(demVote ~ demTwoPartyShare + (1 + demTwoPartyShare|race_eth) +
          (1|age4) +
          (1|edu5) +
          (1|region) +
          (1|sex) +
          (1|state) +
          (1|race_eth:age4) +
          (1|race_eth:edu5) +
          (1|race_eth:region) +
          (1|race_eth:sex) +
          (1|race_eth:state) +
          (1|race_eth:new.subregion) +
          (1|region:age4) +
          (1|region:edu5) +
          (1|state:age4) +
          (1|state:edu5) +
          (1|race_eth:age4:region) +
          (1|race_eth:edu5:region),
        df, family = binomial)
}

