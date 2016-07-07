library( njdanalytics )

nhl_db <- setup_nhl_db()

team_score   <- tbl( nhl_db, "team_score"   )
player_tbl   <- tbl( nhl_db, "player" ) %>% collect()
stage_roster <- tbl( nhl_db, "stage_roster" )
gp_tbl       <- tbl( nhl_db, "game_player"  )
h2h_tbl      <- tbl( nhl_db, "game_h2h"     )

# # player season team ------------------------------------------------------
#
# player_season_team <- stage_roster %>% select( season, session_id, nhl_id, team_short, game_date ) %>%
#   group_by( season, session_id, nhl_id, team_short ) %>% summarize(
#     first_game_date = min( game_date )
#   ) %>% collect()
#
# player_team <- player_season_team %>% ungroup() %>% arrange( nhl_id, first_game_date ) %>%
#   group_by( nhl_id, season, session_id ) %>%
#   summarise(
#     teams     = paste0( team_short, collapse="/" ),
#     num_teams = n()
#   ) %>% ungroup()
#
# save( player_season_team, player_team, file=paste0(nhl_dir$db, "/player_team_2016.RData"))

load( file=paste0(nhl_dir$db, "/player_team_2016.RData") )
# jagr 8448208
# vanek 8470598

# aggregate player stats --------------------------------------------------
# call this for every season we have, 2007-2008 to 2014-2015.
# period = "all", score_diff = "all"



season_cols <- read.csv( file=paste0( nhl_dir$db, "/player_season_cols.csv"))

this_session <- "2"
# player_season <- data_frame()
player_season <- vector("list", 50e3 )

# Seasons to UDPATE ONLY
season_end_recent <- 2016
season_ends <- c( 2008:season_end_recent, "last2y", "last3y", "last2y_1yago" )
player_season_filename <- paste0( nhl_dir$db, "/player_season_", season_end_recent, ".RData")

seasons_str <- c()
for( i in 1:length(season_ends) ) {
  season_end <- season_ends[i]
  message( "Process season ", season_end )

  if( season_end == "last3y" ) {
    season_last3y_start <- season_end_recent - 3
    this_season <- paste0( season_last3y_start, season_end_recent )
    this_start_date <- paste0( season_last3y_start, "-10-01" )
    this_end_date   <- paste0( season_end_recent, "-07-01" )
  } else if( season_end == "last2y" ) {
    season_last2y_start <- season_end_recent - 2
    this_season <- paste0( season_last2y_start, season_end_recent )
    this_start_date <- paste0( season_last2y_start, "-10-01" )
    this_end_date   <- paste0( season_end_recent, "-07-01" )
  } else if( season_end == "last2y_1yago" ) {
    season_last2y_1yago_start <- season_end_recent - 3
    season_last2y_1yago_end   <- season_end_recent - 1
    this_season <- paste0( season_last2y_1yago_start, season_last2y_1yago_end )
    this_start_date <- paste0( season_last2y_1yago_start, "-10-01" )
    this_end_date   <- paste0( season_last2y_1yago_end, "-07-01" )
  } else {
    season_end <- as.numeric( season_end )
    this_season <- paste0( season_end-1, season_end )
    this_start_date <- NULL
    this_end_date   <- NULL
  }
  seasons_str <- c( seasons_str, this_season )

  message( "Aggregate all strength" )
  this_player_season_all <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "all", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_all <- season_cols %>% filter( !is.na(all_strength) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_all <- this_player_season_all[, season_cols_all ]

  message( "Aggregate ev5on5" )
  this_player_season_ev5on5 <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "ev5on5", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = TRUE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_ev5on5 <- season_cols %>% filter( !is.na(ev5on5) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_ev5on5 <- this_player_season_ev5on5[ , season_cols_ev5on5 ]
  names( this_player_season_ev5on5 )[-1] <- paste0( names( this_player_season_ev5on5 )[-1], "_ev5on5" )

  message( "Aggregate OT" )
  this_player_season_ot <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    period     = "4", # REGULAR SEASON OT ONLY
    strength   = "all", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_ot <- season_cols %>% filter( !is.na(ot) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_ot <- this_player_season_ot[ , season_cols_ot ]
  names( this_player_season_ot )[-1] <- paste0( names( this_player_season_ot )[-1], "_ot" )

  message( "Aggregate pp" )
  this_player_season_pp <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "pp", # ev5on5, ot, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_pp <- season_cols %>% filter( !is.na(pp) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_pp <- this_player_season_pp[ , season_cols_pp ]
  names( this_player_season_pp )[-1] <- paste0( names( this_player_season_pp )[-1], "_pp" )

  message( "Aggregate sh" )
  this_player_season_sh <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "sh", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_sh <- season_cols %>% filter( !is.na(sh) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_sh <- this_player_season_sh[ , season_cols_sh ]
  names( this_player_season_sh )[-1] <- paste0( names( this_player_season_sh )[-1], "_sh" )

  message( "Aggregate comp EN" )
  this_player_season_comp_en <- aggregate_player_stats(
  start_date = this_start_date,
  end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "comp_en", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )

  season_cols_comp_en <- season_cols %>% filter( !is.na(comp_en) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_comp_en <- this_player_season_comp_en[ , season_cols_comp_en ]
  names( this_player_season_comp_en )[-1] <- paste0( names( this_player_season_comp_en )[-1], "_comp_en" )
#
  message( "Aggregate own EN" )
  this_player_season_own_en <- aggregate_player_stats(
  start_date = this_start_date,
  end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "own_en", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_own_en <- season_cols %>% filter( !is.na(own_en) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_own_en <- this_player_season_own_en[ , season_cols_own_en ]
  names( this_player_season_own_en )[-1] <- paste0( names( this_player_season_own_en )[-1], "_own_en" )

  this_player_stats <- this_player_season_all
  this_player_stats <- this_player_stats %>% left_join( this_player_season_ev5on5,  by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_pp,      by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_sh,      by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_ot,      by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_comp_en, by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_own_en,  by="nhl_id" )

  #  write.csv( names( this_player_stats ), file=paste0(db_dir, "/player_season_sql_cols.csv") )

  this_player_stats <- this_player_stats %>% mutate(
    season          = this_season,
    session_id      = this_session
  )

  # player_season <- bind_rows( player_season, this_player_stats )
  player_season[[i]] <- this_player_stats
}


player_season <- do.call("rbind", player_season )

# player_season$nhl_id <- player_season$nhl_id %>% as.numeric()

player_season_name <- player_season %>% left_join(
  player_tbl %>% select( nhl_id, first_name, last_name, position, position_fd,
                         birth_date, shoots,
                         draft_team_short, draft_year, draft_round, draft_overall
                          ), by="nhl_id"
)

player_season_name <- player_season_name %>% mutate(
  # p_60_all is not conventionally used, but was by Palmieri's agent!
  # p_60_all        = p_60,
  # p1_60_all       = p1_60,
  # p_60_ev5on5     = p_60_ev5on5,
  # p_60_pp         = p_60_pp,
  # p1_60_ev5on5    = p1_60_ev5on5,
  gf_60_ev5on5    = round( gf_ev5on5 / toi_ev5on5 * 60, 3 ),
  sf_60_ev5on5    = round( sf_ev5on5 / toi_ev5on5 * 60, 3 ),
  cf_60_ev5on5    = round( cf_ev5on5 / toi_ev5on5 * 60, 3 ),
  ff_60_ev5on5    = round( ff_ev5on5 / toi_ev5on5 * 60, 3 ),
  ga_60_ev5on5    = round( ga_ev5on5 / toi_ev5on5 * 60, 3 ),
  sa_60_ev5on5    = round( sa_ev5on5 / toi_ev5on5 * 60, 3 ),
  ca_60_ev5on5    = round( ca_ev5on5 / toi_ev5on5 * 60, 3 ),
  fa_60_ev5on5    = round( fa_ev5on5 / toi_ev5on5 * 60, 3 )
  # pen_diff        = pen_diff_i,
  # pen_diff_ev5on5 = pen_diff_i_ev5on5 # pen_draw_i_ev5on5 - pen_i_ev5on5
)

# join player_team
player_season_all <- player_season_name %>% left_join(
  player_team %>% select( nhl_id, season, session_id, teams), by=c( "nhl_id", "season", "session_id" )
)

player_season_all <- player_season_all %>% select(
  first_name, last_name, nhl_id, position, position_fd, season, session_id, teams, gm, everything()
) %>% arrange( nhl_id, season )

player_season_all <- player_season_all %>% filter( nhl_id > 100 )

recent_player_season_all <- player_season_all

# # Load saved version
# load( file=player_season_filename )
# # drop off recent season and last 3, and replace!
# player_season_all <- player_season_all %>% filter( !season %in% seasons_str )
# player_season_all <- bind_rows( player_season_all, recent_player_season_all )

save( player_season_all, file=player_season_filename )









































