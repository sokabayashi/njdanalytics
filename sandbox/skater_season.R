library( plyr ); library( reshape2 ); library( dplyr ); library( rvest ); library( tidyr ); library( lubridate )
library( knitr ); library( printr );
library( ggplot2 ); library( scales ); library( ggthemes ); library( grid ); library( gridExtra ); library( gtable )
library( zoo ); library( stringr )

base_dir <- "/media/driveb/projects/nhl"
db_dir   <- paste0( base_dir, "/Vault/R/db" )
fa_dir   <- paste0( base_dir, "/Vault/R/fa" )

source( "/media/driveb/projects/nhl/Vault//R/db/player_stats_fn.R" )

nhl_db <- src_postgres( dbname = "nhl", user = "postgres", password = "postgres" )
team_score  <- tbl( nhl_db, "team_score" )
player_tbl  <- tbl( nhl_db, "player" ) %>% collect()
stage_roster <- tbl( nhl_db, "stage_roster" )
gp_tbl      <- tbl( nhl_db, "game_player" )
h2h_tbl     <- tbl( nhl_db, "game_h2h" )
hr_tbl      <- tbl( nhl_db, "hockey_reference_skater" ) # hr = hockey-reference
season_cols <- tbl( nhl_db, "cols_player_season" )

sessionInfo()

# player bio --------------------------------------------------------------
# every skater in player_tbl

# nhl_id
# first last name
# position
# shot


# player season team ------------------------------------------------------

# player_season_team <- stage_roster %>% select( season, session_id, nhl_id, team_short, game_date ) %>%
#   group_by( season, session_id, nhl_id, team_short ) %>% summarize(
#     first_game_date = min( game_date )
#   ) %>% collect()
#
# player_team <- player_season_team %>% ungroup() %>% arrange( nhl_id, first_game_date ) %>%
#   group_by( nhl_id, season, session_id ) %>%
#   summarize(
#     teams = paste0( team_short, collapse="/" )
#   ) %>% ungroup()
#
# save( player_season_team, player_team, file=paste0(db_dir, "/player_team.RData"))

load( file=paste0(db_dir, "/player_team.RData") )
# jagr 8448208
# vanek 8470598

# aggregate player stats --------------------------------------------------
# call this for every season we have, 2007-2008 to 2014-2015.
# period = "all", score_diff = "all"


season_cols <- read.csv( file=paste0( db_dir, "/player_season_cols.csv"))

this_session <- "2"
# player_season <- data_frame()
player_season <- vector("list", 50e3 )
seasons <- c( 2008:2015, "last3" )
for( i in 1:length(seasons) ) {
  season_end <- seasons[i]
  message( "Process season ", season_end )



  if( season_end == "last3" ) {
    this_season <- "20122015"
    this_start_date <- "2012-10-01"
    this_end_date   <- "2015-07-01"
  } else {
    season_end <- as.numeric( season_end )
    this_season <- paste0( season_end-1, season_end )
    this_start_date <- NULL
    this_end_date   <- NULL
  }


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
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_ev5on5 <- season_cols %>% filter( !is.na(ev5on5) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_ev5on5 <- this_player_season_ev5on5[ , season_cols_ev5on5 ]
  names( this_player_season_ev5on5 )[-1] <- paste0( names( this_player_season_ev5on5 )[-1], "_ev5on5" )

  message( "Aggregate ev4on4" )
  this_player_season_ev4on4 <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "ev4on4", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_ev4on4 <- season_cols %>% filter( !is.na(ev4on4) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_ev4on4 <- this_player_season_ev4on4[ , season_cols_ev4on4 ]
  names( this_player_season_ev4on4 )[-1] <- paste0( names( this_player_season_ev4on4 )[-1], "_ev4on4" )

  message( "Aggregate pp" )
  this_player_season_pp <- aggregate_player_stats(
    start_date = this_start_date,
    end_date   = this_end_date,
    season     = this_season,
    session_id = this_session, # 2=regular, 3=playoffs
    strength   = "pp", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
    # team_only  = FALSE,
    h2h_stats  = FALSE,
    group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
    nhl_db = nhl_db
  )
  season_cols_pp <- season_cols %>% filter( !is.na(pp) ) %>% select( col ) %>% unlist() %>% as.character()
  this_player_season_pp <- this_player_season_pp[ , season_cols_pp ]
  names( this_player_season_pp )[-1] <- paste0( names( this_player_season_pp )[-1], "_pp" )

#   message( "Aggregate comp EN" )
#   this_player_season_comp_en <- aggregate_player_stats(
#   start_date = this_start_date,
#   end_date   = this_end_date,
  #     season     = this_season,
#     session_id = this_session, # 2=regular, 3=playoffs
#     strength   = "comp_en", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
#     # team_only  = FALSE,
#     h2h_stats  = FALSE,
#     group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
#     nhl_db = nhl_db
#   )
#   this_player_season_comp_en <- this_player_season_comp_en[ , !is.na(season_cols$comp_en)]
#   names( this_player_season_comp_en )[-1] <- paste0( names( this_player_season_comp_en )[-1], "_comp_en" )
#
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

#   message( "Aggregate own EN" )
#   this_player_season_own_en <- aggregate_player_stats(
  #   start_date = this_start_date,
  #   end_date   = this_end_date,
  #     season     = this_season,
#     session_id = this_session, # 2=regular, 3=playoffs
#     strength   = "own_en", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
#     # team_only  = FALSE,
#     h2h_stats  = FALSE,
#     group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
#     nhl_db = nhl_db
#   )
#   this_player_season_own_en <- this_player_season_own_en[ , !is.na(season_cols$own_en)]
#   names( this_player_season_own_en )[-1] <- paste0( names( this_player_season_own_en )[-1], "_own_en" )
#
  this_player_stats <- this_player_season_all
  this_player_stats <- this_player_stats %>% left_join( this_player_season_ev5on5, by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_ev4on4, by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_pp,     by="nhl_id" )
  this_player_stats <- this_player_stats %>% left_join( this_player_season_sh,     by="nhl_id" )

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

player_season <- player_season %>% left_join(
  player_tbl %>% select( nhl_id, first_name, last_name, position, position_fd,
                         birth_date, shoots,
                         draft_team_short, draft_year, draft_round, draft_overall
                          ), by="nhl_id"
)

player_season <- player_season %>% mutate(
  p_60_ev5on5     = p_ev5on5 / toi_ev5on5 * 60,
  p_60_pp         = p_pp / toi_pp * 60,
  gf_60_ev5on5    = gf_ev5on5 / toi_ev5on5 * 60,
  sf_60_ev5on5    = sf_ev5on5 / toi_ev5on5 * 60,
  cf_60_ev5on5    = cf_ev5on5 / toi_ev5on5 * 60,
  ff_60_ev5on5    = ff_ev5on5 / toi_ev5on5 * 60,
  ga_60_ev5on5    = ga_ev5on5 / toi_ev5on5 * 60,
  sa_60_ev5on5    = sa_ev5on5 / toi_ev5on5 * 60,
  ca_60_ev5on5    = ca_ev5on5 / toi_ev5on5 * 60,
  fa_60_ev5on5    = fa_ev5on5 / toi_ev5on5 * 60,
  sf_i_gm_ev5on5  = sf_i_ev5on5 / gm,
  pen_diff_ev5on5 = pen_draw_i_ev5on5 - pen_i_ev5on5
)

# join player_team
player_season_all <- player_season %>% left_join(
  player_team %>% select( nhl_id, season, session_id, teams), by=c( "nhl_id", "season", "session_id" )
)

player_season_all <- player_season_all %>% select(
  first_name, last_name, nhl_id, position, position_fd, season, session_id, teams, gm, everything()
) %>% arrange( nhl_id, season )

player_season_all <- player_season_all %>% filter( nhl_id > 100 )

save( player_season_all, file=paste0( db_dir, "/player_season.RData") )


# Last 2/3 years ----------------------------------------------------------







































