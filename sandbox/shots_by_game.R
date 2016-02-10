library( njdanalytics )
# source( paste0( nhl_dir$base, "/packages/njdanalytics/sandbox/charts.R" ) )

nhl_db <- setup_nhl_db()
# player_tbl           <- tbl( nhl_db, "player"               ) %>% collect()
# stage_game           <- tbl( nhl_db, "stage_game"           )
# stage_roster         <- tbl( nhl_db, "stage_roster"         )
# stage_playbyplay     <- tbl( nhl_db, "stage_playbyplay"     )
# stage_shift_interval <- tbl( nhl_db, "stage_shift_interval" )
team_score           <- tbl( nhl_db, "team_score"           )
# game_player          <- tbl( nhl_db, "game_player"          )

# njd_games has game_number and game_id4 to join different data sources
this_season     <- "20152016"
this_session_id <- "2"
njd_games            <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short=="NJD" ) %>%
  collect() %>% arrange( game_date )
njd_games            <- add_team_score_label( njd_games, our_team="NJD" ) %>%
  select( game_number, season, session_id, game_id4, ha, opp_team_short, game_label )

load( file=paste0( nhl_dir$shotdata, "/njd_through_44.RData" ) )
njd_summary_spreadsheet <- paste0( nhl_dir$shot, "/NJD_through_gm44.xlsx" )
njd_summary <- read_excel( njd_summary_spreadsheet, skip=1)

last_names  <- njd_summary$Player

njd_players_by_game_ev <- njd_players_by_game %>% filter( strength=="ev5on5", metric %in% c( "green", "blue" ), nhl_id !=1, position_fd != "G" ) %>% 
                select( -season, -session_id, -strength, -ha_number, -team_short, -game_id4 )

window_size <- 5
players_ma <- njd_players_by_game_ev %>% group_by( nhl_id, last_name, position_fd, number, metric ) %>%
  arrange(
    desc(game_number)
  ) %>% filter( n() >= window_size ) %>%
  mutate(
    scf_ma    = rollmean( x=scf,   window_size, align="left", fill=NA ),
    sca_ma    = rollmean( x=sca, window_size, align="left", fill=NA ),
    sc_net_ma = rollmean( x=sc_net, window_size, align="left", fill=NA ),
    scf_i_ma  = rollmean( x=scf_i, window_size, align="left", fill=NA ),
    toi_gm_ma = rollmean( x=toi, window_size, align="left", fill=NA )
  ) %>% ungroup() %>% arrange( position_fd, nhl_id, metric, game_number)

this_last_name <- "CAMMALLERI"
this_player <- players_ma %>% filter( last_name==this_last_name  ) %>% select( 1, 2, 10, 11, 18:23 ) 
this_player <- this_player %>% gather( foragainst, value, -c(1,2,3,5,6) ) %>% filter( foragainst %in% c("scf_ma", "sca_ma", "sc_net_ma"))
this_player <- this_player %>% transform(
  foragainst = factor( foragainst, labels=c( "For", "Against", "Net"))
)
this_title <- paste( this_player[1,]$number, this_player[1,]$last_name )
g.player <- ggplot( this_player %>% filter(metric %in% c("green", "blue")), aes(x=game_number,y=value) )
g.player + geom_hline(yintercept=0, color="grey") +
  # stat_smooth(color="lightgrey", se=F) + 
  geom_line(aes(color=metric),size=1) + 
  geom_point(aes(color=metric),size=1.6) +
  scale_color_manual( "Chance", labels=c( "Blue", "Green" ), values=c( "blue"="slateblue", "green"="yellowgreen"), guide=F) +
  scale_x_continuous( "Game Number",breaks=seq(0,100,5) ) + 
  scale_y_continuous( "Chances", breaks=seq(-50,50,1) ) + 
  facet_grid( metric ~ foragainst ) + theme_bw() + 
  ggtitle( this_title )
  
