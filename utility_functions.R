
get_fpl_stats <- function(url){
  
  resp <- httr::GET(url,
                    user_agent("Testing"),
                    httr::content_type("json")
                    )
  
  content <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  fpl_stats <- jsonlite::fromJSON(content,
                                  simplifyDataFrame = TRUE,
                                  flatten = TRUE)
  
  return(fpl_stats)
  
}


create_team_df <- function(fpl_stats, colours_path) {
  
  teams <- fpl_stats$teams
  
  #reading team colours csv
  team_colours <- readr::read_csv(colours_path,
                                  col_types = list(
                                  name = col_character(),
                                  primary = col_character(),
                                  secondary = col_character(),
                                  tertiary = col_character(),
                                  id = col_double()
                                  ))
  
  teams_w_colours <- left_join(teams, select(team_colours, -name), by = "id")
  
  # conditional colouring in case of missing elements
  teams_w_colours$secondary <- ifelse(teams_w_colours$secondary == "", teams_w_colours$primary, teams_w_colours$secondary)
  
  teams_w_colours$tertiary <- ifelse(teams_w_colours$tertiary == "", teams_w_colours$secondary, teams_w_colours$tertiary)
  
  return(teams_w_colours)
  
}

create_player_df <- function(fpl_stats, team_colours) {
  
  elements <- fpl_stats$elements
  element_types <- fpl_stats$element_types
  
  elements_fields <- c(
    "first_name",
    "second_name",
    "web_name",
    "id",
    "team",
    "selected_by_percent",
    "now_cost",
    "form",
    "minutes",
    "total_points",
    "transfers_in",
    "chance_of_playing_next_round",
    "chance_of_playing_this_round",
    "element_type",
    "photo"
  )
  
  player_df <- select(elements, all_of(elements_fields)) %>%
    left_join(select(element_types, id, singular_name),
              by = c("element_type" = "id")) %>%
    left_join(select(teams_w_colours , id, name, primary, secondary, tertiary),
              by = c("team" = "id")) %>%
    select(-c("team", "element_type")) %>%
    rename(team_name = name, position = singular_name) %>%
    mutate(
      cost_abbrv = now_cost / 10,
      value = total_points / cost_abbrv,
      m_cost = now_cost * 100000,
      photo_jpg = paste0(
        "https://resources.premierleague.com/premierleague/photos/players/110x140/p",
        photo
      ),
      photo_png = paste0(substr(photo_jpg, 1, nchar(photo_jpg) - 4), ".png")
    )
  
  
  return(player_df)
  
  
}


get_one_player_details <- function(player_url, id){
  
  req <- paste0(player_url, id, "/")
  
  resp <- httr::GET(req,
                    user_agent("Testing"),
                    httr::content_type("json"))
  
  
  content <- httr::content(resp, as = "text", encoding = "UTF-8")
  
  
  
  player_stats <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE, flatten = TRUE)
  
  player_stats$history$player_id <- id
  player_stats$fixtures$player_id <- id
  player_stats$history_past$player_id <- id
  
  return(player_stats)
  
  
}