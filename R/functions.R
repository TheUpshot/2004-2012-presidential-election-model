adjust <- function(df, ...) {

  # parse function arguments
  lazyDots <- lazyeval::lazy_dots(...)
  arg <- deparse(first(lazyDots)$expr) %>%
    gsub("^plogis|[()*+]", "", .) %>%
    strsplit(" +") %>%
    first() %>%
    setNames(c("target", "offset", "weight"))

  # calculate adjustment
  df %>%
    group_by(state) %>%
    rename_(.dots = as.list(arg)) %>%
    summarize(adj = optimize(objective, c(-1, 1), target,
                          offset, weight)$minimum) %>%
    rename_(.dots = setNames(list(~ adj), first(arg))) %>%
    right_join(select(df, -matches(first(arg)))) %>%
    mutate_(.dots = lazyDots)

}

getData <- function(year) {
  datasets <- c("acs", "cps", "polls")
  datasets %>%
    sprintf("data/%s/%s.csv", year, .) %>%
    lapply(read_csv) %>%
    setNames(datasets)
}

getModels <- function(data, year, force = F) {
  path <- sprintf("out/model-%s.rds", year)
  if (!file.exists(path) || force) {
    model <- list(
      turnout = modelTurnout(data$cps),
      twoParty = modelTwoParty(filter(data$polls, !otherVote)),
      threeParty = modelThreeParty(data$polls)
    )
    saveRDS(model, path)
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
  glmer(otherVote ~ otherVoteShare + (1 + otherVoteShare|race_eth) +
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

objective <- function(x, target, offset, weight) {
  abs(sum(weight * target) - sum(weight * plogis(offset + x)))
}

