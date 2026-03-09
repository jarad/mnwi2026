library("tidyverse")
theme_set(theme_bw())

source("code/construct_matrix.R")

# Plot
plot_ratings <- function(d) {
  ggplot(d |>
           pivot_longer(
             offense:strength,
             names_to = "type",
             values_to = "rating"),
         aes(
           x = rating,
           y = team
         )) +
    geom_point(aes(color = type)) +
    # geom_bar(stat="identity") +
    facet_wrap(~type) +
    labs(
      x = "Rating",
      y = "Team",
      title = "FRC MNWI 2026 - Qualification"
    ) 
}

# Minnesota Bluff Country Regional
tmp <- read_csv("data/mnwi2026.csv") 
# WonAuto was manually entered
tail(tmp$Match,1) # last match in these data


# Team numbers
teams <- tmp |>
  select(`Red 1`:`Blue 3`) |>
  pivot_longer(everything()) |>
  pull(value) |>
  unique() |>
  sort()

n_teams <- length(teams)

# Create factor for all teams
mnwi2026 <- tmp |>
  mutate(
    `Red 1`  = factor(`Red 1`,  levels = teams),
    `Red 2`  = factor(`Red 2`,  levels = teams),
    `Red 3`  = factor(`Red 3`,  levels = teams),
    `Blue 1` = factor(`Blue 1`, levels = teams),
    `Blue 2` = factor(`Blue 2`, levels = teams),
    `Blue 3` = factor(`Blue 3`, levels = teams)
  )

# mnwi2026 |> datatable(filter = "top", rownames = FALSE)


# Construct model matrices
# row is matches and columns are teams
# cell is 1 if that team was on that alliance in that match and 0 otherwise
# e.g. X_red[23, 33] = 1 means team 33 was on red alliance in match 23
# Teams[33] gives you the FRC number for that team
X_red <- construct_matrix(mnwi2026, "Red 1", n_teams) +
  construct_matrix(mnwi2026, "Red 2", n_teams) +
  construct_matrix(mnwi2026, "Red 3", n_teams) 

X_blue <- construct_matrix(mnwi2026, "Blue 1", n_teams) +
  construct_matrix(mnwi2026, "Blue 2", n_teams) +
  construct_matrix(mnwi2026, "Blue 3", n_teams) 

# Some data checks
stopifnot(rowSums(X_red)  == 3)
stopifnot(rowSums(X_blue) == 3)
stopifnot(all(tmp$WonAuto %in% c("Red","Blue")))




################################################################################
#
# Analysis that provide an improved OPR and DPR for all robots that
# adjusts for all the other robots on the field. First analysis ignores who
# won auto and just uses final match score. 
#
################################################################################

Y <- c(mnwi2026$`Red Final`, 
       mnwi2026$`Blue Final`)

# negative sign is so that better defense ability is a more positive number 
X <- rbind(
  cbind(X_red,  -X_blue), 
  cbind(X_blue, -X_red)
)

# Fit model
m <- lm(Y ~ 0 + X)
# summary(m)

# 
mnwi2026_offense_defense <- data.frame(
  team   = rep(teams, times = 2),
  type   = rep(c("offense","defense"), each = length(teams)),
  rating = coef(m)
) |>
  mutate(
    # Make ratings interpretable (this could probably be improved)
    rating = ifelse(is.na(rating), 0, rating),
    rating = rating - mean(rating) 
  ) |>
  pivot_wider(
    names_from = "type", 
    values_from = "rating") |>
  mutate(
    strength = offense + defense,
    team    = factor(team, team[order(strength)])
  ) |>
  arrange(desc(team))

plot_ratings(mnwi2026_offense_defense)


################################################################################
#
# Analysis that provide an improved OPR and DPR for all robots that
# adjusts for all the other robots on the field. This analysis adjusts provides
# for the effect of who wins auto and adjusts OPR/DPR for who won auto. 
#
################################################################################



# Create vector indicating which alliance WonAuto
# need this twice: once for Red score and once for Blue score
X_wonauto <- c(
  tmp$WonAuto == "Red", # should be  tmp$WonAuto != "Blue"
  tmp$WonAuto == "Blue" # should be  tmp$WonAuto != "Red"
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
confint(m2)[1,] # uncertainty on this effect

# 
mnwi2026_offense_defense_wonauto <- data.frame(
  team   = rep(teams, times = 2),
  type   = rep(c("offense","defense"), each = length(teams)),
  rating = coef(m2)[-1] # remove wonauto effect
) |>
  mutate(
    rating = ifelse(is.na(rating), 0, rating),
    rating = rating - mean(rating)
  ) |>
  pivot_wider(
    names_from = "type", 
    values_from = "rating") |>
  mutate(
    strength = offense + defense,
    team    = factor(team, team[order(strength)])
  ) |>
  arrange(desc(team))


plot_ratings(mnwi2026_offense_defense_wonauto) +
  labs(subtitle="after accounting for wonauto effect")


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




