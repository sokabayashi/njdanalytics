library( njdanalytics )

nhl_db <- setup_nhl_db()

team_tbl   <- tbl( nhl_db, "team" ) %>% collect() %>% filter( !name_short %in% c( "ATL", "PHX") )
player_tbl <- tbl( nhl_db, "player" ) %>% collect()

line_combos <- data_frame()
for( i in 1:nrow(team_tbl ) ) {
  team_info <- team_tbl[i,]

  team_short  <- team_info$name_short
  message( "Scraping ", team_short )

  team_hyphen <- gsub( " ", "-", team_info$name ) %>% str_to_lower() %>% gsub( ".", "", ., fixed=T )

  team_url <- sprintf( "http://leftwinglock.com/line-combinations/%s/?team=%s&strength=EV&gametype=GD", team_hyphen, team_hyphen )

  team_html <- team_url %>% read_html()
  team_sets <- team_html %>% html_nodes( ".table-container" )
  forwards  <- team_sets[1] %>% html_node( "table" ) %>% html_table() %>% getElement(1)

line_combos <- rbind(
  line_combos,
  cbind( team_short, forwards ) )
}

player_tbl <- player_tbl %>% mutate(
  full_name = first_last_name %>% str_to_upper()
)

line_combos$`Left Wing`[ line_combos$`Left Wing`=="NIKOLAI KULEMIN" ] <- "NIKOLAY KULEMIN"
line_combos$`Left Wing`[ line_combos$`Left Wing`=="MICHEAL FERLAND" ] <- "MICHAEL FERLAND"

team_lines <- line_combos %>%
  left_join( player_tbl %>% select( full_name, lw_id=nhl_id, lw_shot=shoots, lw_dob=birth_date, lw_age=age_season_end ), by=c( "Left Wing"="full_name" ) ) %>%
  left_join( player_tbl %>% select( full_name, rw_id=nhl_id, rw_shot=shoots, rw_dob=birth_date, rw_age=age_season_end ), by=c( "Right Wing"="full_name" ) )

off_handed <- data_frame()
off_handed <- bind_rows( off_handed,
  team_lines %>% filter(
    lw_shot=="R"
  ) %>% select( team_short, player=`Left Wing`, shot=lw_shot, nhl_id=lw_id, dob=lw_dob, age_20160630=lw_age ) %>%
    mutate(
      position="LW"
    )
)

off_handed <- bind_rows( off_handed,
  team_lines %>% filter(
    rw_shot=="L"
  ) %>% select( team_short, player=`Right Wing`, shot=rw_shot, nhl_id=rw_id, dob=rw_dob, age_20160630=rw_age ) %>%
    mutate(
      position="RW"
    )
)

write_csv( off_handed, path="sandbox/offhanded wingers.csv")
