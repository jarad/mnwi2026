library("tidyverse")
theme_set(theme_bw())

source("code/construct_matrix.R")

# Plot
plot_ratings <- function(d) {
  ggplot(
    d |>
      pivot_longer(
        offense:strength,
        names_to = "type",
        values_to = "rating"
      ),
    aes(
      x = rating,
      y = team
    )
  ) +
    geom_point(aes(color = type)) +
    # geom_bar(stat="identity") +
    facet_wrap(~type) +
    labs(
      x = "Rating",
      y = "Team",
      title = "FRC IACF 2026 - Qualification"
    )
}

# Iowa Regional
tmp <- read_csv("data/2026iacf_match_data.csv") |>
  filter(comp_level == "qm")

tail(tmp$Match, 1) # last match in these data


# Team numbers
match_teams <- tmp |>
  select(match_number, alliances.blue.team_keys, alliances.red.team_keys) |>
  pivot_longer(-match_number, names_to = "alliance", values_to = "team") |>
  mutate(
    alliance = gsub("alliances.", "", alliance),
    alliance = gsub(".team_keys", "", alliance)
  ) |>
  unique()

teams <- match_teams |>
  pull(team) |>
  unique() |>
  sort() |>
  factor()

match_teams <- match_teams |>
  mutate(
    team = factor(team, levels = teams),
    team_factor = as.numeric(team)
  )

n_teams <- length(teams)
n_matches <- max(match_teams$match_number)

# Get match scores
scores <- tmp |>
  mutate(
    wonAuto = case_when(
      score_breakdown.red.hubScore.autoPoints ==
        score_breakdown.blue.hubScore.autoPoints &
        score_breakdown.blue.hubScore.shift2Count > 0 ~ "blue",
      score_breakdown.red.hubScore.autoPoints ==
        score_breakdown.blue.hubScore.autoPoints &
        score_breakdown.red.hubScore.shift2Count > 0 ~ "red",
      score_breakdown.red.hubScore.autoPoints >
        score_breakdown.blue.hubScore.autoPoints ~ "red",
      score_breakdown.red.hubScore.autoPoints <
        score_breakdown.blue.hubScore.autoPoints ~ "blue"
    )
  ) |>
  select(
    match_number,
    wonAuto,
    score_breakdown.red.hubScore.autoPoints,
    score_breakdown.blue.hubScore.autoPoints,
    score_breakdown.red.totalTeleopPoints,
    score_breakdown.blue.totalTeleopPoints
  ) |>
  unique() |>
  pivot_longer(
    starts_with("score"),
    names_to = "phase_alliance",
    values_to = "points"
  ) |>
  mutate(
    alliance = case_when(
      grepl("red", phase_alliance) ~ "red",
      grepl("blue", phase_alliance) ~ "blue"
    ),
    phase = case_when(
      grepl("auto", phase_alliance) ~ "auto",
      grepl("Teleop", phase_alliance) ~ "teleop"
    )
  ) |>
  select(match_number, wonAuto, phase, alliance, points) |>
  arrange(match_number, phase, alliance)


# Construct model matrices
# row is matches and columns are teams
# cell is 1 if that team was on that alliance in that match and 0 otherwise
# e.g. X_red[23, 33] = 1 means team 33 was on red alliance in match 23
# Teams[33] gives you the FRC number for that team
X_red <- X_blue <- matrix(0, nrow = n_matches, ncol = n_teams)
for (i in 1:nrow(match_teams)) {
  if (match_teams$alliance[i] == "red") {
    X_red[match_teams$match_number[i], match_teams$team_factor[i]] <- 1
  } else {
    X_blue[match_teams$match_number[i], match_teams$team_factor[i]] <- 1
  }
}

# Some data checks
stopifnot(rowSums(X_red) == 3)
stopifnot(rowSums(X_blue) == 3)


################################################################################
#
# Auto
#
# Analysis that provide an improved OPR and DPR for all robots that
# adjusts for all the other robots on the field. First analysis ignores who
# won auto and just uses final match score.
#
################################################################################

Y <- c(
  scores |> filter(phase == "auto", alliance == "red") |> pull(points),
  scores |> filter(phase == "auto", alliance == "blue") |> pull(points)
)

# assume no defense in auto
X <- rbind(
  cbind(X_red),
  cbind(X_blue)
)

# Fit linear regression model without an intercept
m <- lm(Y ~ 0 + X)
# summary(m)

#
IACF2026_auto <- data.frame(
  team = teams,
  type = "auto",
  rating = coef(m)
) |>
  # mutate(
  #   # Make ratings interpretable (this could probably be improved)
  #   rating = ifelse(is.na(rating), 0, rating),
  #   rating = rating - mean(rating)
  # ) |>
  # pivot_wider(
  #   names_from = "type",
  #   values_from = "rating") |>
  mutate(
    team = factor(team, team[order(rating)])
  ) |>
  arrange(desc(rating))

ggplot(IACF2026_auto, aes(x = rating, y = team)) +
  geom_point() +
  labs(
    title = "AUTO Fuel OPR",
    x = "OPR"
  )


################################################################################
#
# Teleop
#
# Analysis that provide an improved OPR and DPR for all robots that
# adjusts for all the other robots on the field. This analysis adjusts provides
# for the effect of who wins auto and adjusts OPR/DPR for who won auto.
#
################################################################################

Y <- c(
  scores |> filter(phase == "teleop", alliance == "red") |> pull(points),
  scores |> filter(phase == "teleop", alliance == "blue") |> pull(points)
)

newtmp <- scores |>
  select(match_number, wonAuto) |>
  unique()

X_wonauto <- c(
  newtmp$wonAuto == "red", # should be  tmp$WonAuto != "Blue"
  newtmp$wonAuto == "blue" # should be  tmp$WonAuto != "Red"
)

X <- rbind(
  cbind(X_red, -X_blue),
  cbind(X_blue, -X_red)
)
X2 <- cbind(X_wonauto, X)

# Fit model
m2 <- lm(Y ~ 0 + X2)
# summary(m2)

# Effect of WonAuto
# This combines the effect of difference in auto scoring which leads to an
# alliance winning auto and the advantage gained in teleop due to having won
# auto.
coef(m2)[1]
confint(m2)[1, ] # uncertainty on this effect

#
IACF2026_offense_defense_wonauto <- data.frame(
  team = rep(teams, times = 2),
  type = rep(c("offense", "defense"), each = length(teams)),
  rating = coef(m2)[-1] # remove wonauto effect
) |>
  mutate(
    rating = ifelse(is.na(rating), 0, rating),
    rating = rating - mean(rating)
  ) |>
  pivot_wider(
    names_from = "type",
    values_from = "rating"
  ) |>
  mutate(
    strength = offense + defense,
    team = factor(team, team[order(strength)])
  ) |>
  arrange(desc(team))


plot_ratings(IACF2026_offense_defense_wonauto) +
  labs(subtitle = "after accounting for wonauto effect")

################################################################################
#
# What needs to be done:
# - separately analyze auton and teleop periods
#   - parse match data to
#     - calculate auton points
#     - determine who won auton
#       (if it was a tie, determine who had teleop second as this is advantageous)
#     - calculate teleop points
#
# - in auton analysis,
#     - consider only having offense as defense is small
#       (although non-zero, e.g. robots that get to neutral first and steal fuel
#       are denying fuel from other robots)
#     - do NOT include wonauto effect
#     - build a model (logistic regression model) for probability of winning auton
#
# - in teleop analysis,
#     - keep offense and defense
#     - include wonauto effect to evaluate strength of robots aside from winning auto
#
# - auton+teleop analysis,
#     - for simplicity, we may want to have a single summary of the strength of
#       a team that accounts for
#       - auton strength,
#       - probability of winning auton * auton effect, and
#       - teleop strength
