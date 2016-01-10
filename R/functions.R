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

getModels <- function(data, force = F) {
  path <- "out/model.rds"
  if (!file.exists(path) || force) {
    model <- list(
      turnout = modelTurnout(data$cps),
      twoParty = modelTwoParty(filter(data$polls, !vto)),
      threeParty = modelThreeParty(data$polls)
    )
    saveRDS(model, "out/model.rds", compress = F)
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
  glmer(paste("vto ~ o.pct + (1 + o.pct|race_eth) +", randomEffectString),
        df, family = binomial)
}

modelTwoParty <- function(df) {
  print("-- fitting two party support model --")
  glmer(paste("vtd ~ d2pv.12 + (1 + d2pv.12|race_eth) +", randomEffectString),
        df, family = binomial)
}

randomEffectString <- alist(age4,
                            edu5,
                            region,
                            sex,
                            state,
                            race_eth:age4,
                            race_eth:edu5,
                            race_eth:region,
                            race_eth:sex,
                            race_eth:state,
                            race_eth:new.subregion,
                            region:age4,
                            region:edu5,
                            state:age4,
                            state:edu5,
                            race_eth:age4:region,
                            race_eth:edu5:region) %>%
  paste0("(1|", ., ")", collapse = " + ")
