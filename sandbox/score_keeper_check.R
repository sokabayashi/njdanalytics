library( njdanalytics )

nhl_db <- setup_nhl_db()

gp <- tbl( nhl_db, "game_player" )
njd_gp_all <- gp %>% filter( nhl_id==1, season=="20152016", session_id=="2", filter_period=="all", filter_strength=="all", filter_score_diff=="all") %>% collect()

njd_home_game_id4 <- njd_gp_all %>% filter( team_ha=="H" ) %>% select( game_id4 ) %>% unlist(use.names = F)

game_suffix <- 82
# njd_games, shots_tbl, player_chances_df, njd_players_by_game, pair_chances_df
load( file=paste0( nhl_dir$shot, "/njd_through_", game_suffix, ".RData" ))

official_counts <- njd_gp_all %>% filter( game_id4 %in% njd_home_game_id4 ) %>%
                    summarise( c_total = sum(cf+ca), f_total = sum(ff+fa), s_total=sum(sf+sa))

shots_tbl_home <- shots_tbl %>% filter( game_id4 %in% njd_home_game_id4 ) %>%
  summarise( c_total = n(), f_total = sum(shot!="BLOCK"), s_total=sum(shot %in% c( "SOG", "GOAL")))
