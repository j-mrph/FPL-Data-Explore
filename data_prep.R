library(httr)      # CRAN v1.4.2
library(jsonlite)  # CRAN v1.7.2
library(dplyr)     # CRAN v1.0.7


# retrieving data from FPL API
FPL_URL <- "https://fantasy.premierleague.com/api/bootstrap-static/"
PLAYER_URL <- "https://fantasy.premierleague.com/api/element-summary/"
team_colours_path <- "team_colours.csv"


# raw stats
fpl_stats <- get_fpl_stats(FPL_URL)

# team colours dataframe
teams_w_colours <- create_team_df(fpl_stats, team_colours_path)

# get player df that will be used in app
player_df <- create_player_df(fpl_stats, teams_w_colours)


