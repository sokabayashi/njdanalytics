library( njdanalytics )

source( paste0( nhl_dir$db, "/player_tbl_create.R" ) )

nhl_db <- setup_nhl_db()

this_season <- "20152016"
season_end  <- substr( this_season, 5, 8 ) %>% as.numeric()

player_tbl   <- tbl( nhl_db, "player" ) %>% collect()
stage_game   <- tbl( nhl_db, "stage_game" ) %>% filter( season == this_season, session_id == "2" ) %>% collect()
stage_roster <- tbl( nhl_db, "stage_roster" ) %>% filter( season == this_season, session_id == "2" ) %>% collect()
team_tbl     <- tbl( nhl_db, "team" ) %>% collect()
draft_tbl    <- tbl( nhl_db, "draft" ) %>% collect()

roster_players <- stage_roster %>% select( nhl_id, last_name, first_name, team_short, position, game_id4, game_date )

remnant_players_games <- roster_players %>% anti_join( player_tbl, by="nhl_id" )

remnant_players <- remnant_players_games %>% group_by( nhl_id, last_name, first_name, team_short, position ) %>%
                    summarize( gm=n(), last_game=max(game_date) ) %>% as.data.frame()

remnant_players <- remnant_players %>% left_join( team_tbl %>% select( name_short, name_common ), by=c( "team_short"="name_short" ) )
remnant_players <- remnant_players %>% arrange( team_short, desc(gm) ) %>% mutate(
  player_url = sprintf( "http://%s.nhl.com/club/player.htm?id=%s&view=stats", gsub( " ", "", name_common ), nhl_id )
) %>% ungroup()




# update player tbl -------------------------------------------------------


con    <- dbConnect( dbDriver( "PostgreSQL"), dbname="nhl" )

for( i in 1:nrow(remnant_players) ) {
  this_player <- remnant_players[i,]

  this_nhl_id     <- this_player$nhl_id
  this_url        <- this_player$player_url
  this_team_short <- this_player$team_short
  this_position   <- this_player$position

  player_df <- get_player_info_from_team_page( this_nhl_id, this_team_short, this_position, season_end, team_tbl, draft_tbl, player_url=this_url )
  message( "Add ", player_df$first_last_name, " to player table." )

  print( player_df %>% glimpse)
  dbWriteTable2( con, "player", as.data.frame(player_df), overwrite=F, append=T )

}

