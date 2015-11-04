
# Game Data functions ----------------------------------------------------------
#
#'  Get all time intervals from a game with diff number of on ice players
#'
#' @param shift_interval_df A data frame of shift intervals, from stage_shift_interval tbl
#' @param ha A character "H" or "A"
#' @return Data frame of time intervals
#' @export
get_num_player_intervals_ha <- function( shift_interval_df, ha = "H" ) {
  if( ha == "H" ) {
    num_players_rle <- rle( shift_interval_df$num_players_h )
  } else {
    num_players_rle <- rle( shift_interval_df$num_players_a )
  }

  ## rle returns a list
  #]> values: [1] 6 5 6 5 6 5 6 5 4 5 6
  #> lengths: run length, how many consecuive elements take that value
  #[1] 51  6 13  8 44  6 91  4  1  4 14
  # Use these to index into change point row in shift_interval_df
  num_units <- length( num_players_rle$values )
  num_players_rle$start_index <- c( 1, ( cumsum( num_players_rle$lengths )+1 )[ -num_units ] )
  num_players_rle$end_index   <- c(      cumsum( num_players_rle$lengths ))

  num_player_intervals <- data_frame(
    num_players = num_players_rle$values,
    start_cum   = shift_interval_df$start_cum[ num_players_rle$start_index ],
    end_cum     = shift_interval_df$start_cum[ num_players_rle$end_index ] +
      shift_interval_df$duration[ num_players_rle$end_index ],
    end_period  = shift_interval_df$period[    num_players_rle$end_index ]
  )

  return( num_player_intervals )
}

# get_mandown_intervals -------------------------------------------------------
# determine man adv start and end times based on number of skaters on ice.
# Note: this is *not* one-to-one with the actual penalties; rather, it just
# shows the *impact* of penalties on strength.  also captures 3on3 & 4on4 due
# to penalties (but not OT).
# Suppose team gets two consecutive penalties, separarated by 30 sec.
# assume opponent does not score.
# we will get 3 rows: a 30 sec 5on4, a 1:30 5on3, a 30 sec 5on4.

#' Get intervals for when each team is shorthanded during a game.
#'
#' @param shift_interval_df A data frame of shift intervals, from stage_shift_interval tbl
#' @param game_info A data frame of game info from stage_game
#' @return Data frame of all man down time intervals from a game.
#' @export
get_mandown_intervals <- function( shift_interval_df, game_info ) {

  mandown_times_h <- get_num_player_intervals_ha( shift_interval_df, "H" )
  mandown_times_a <- get_num_player_intervals_ha( shift_interval_df, "A" )

  # we require a min num of players of 4 bc sometimes it's just bad data (missing goalie)
  mandown_times_h <- mandown_times_h %>% filter( num_players >= 4 )
  mandown_times_a <- mandown_times_a %>% filter( num_players >= 4 )

  # which of these are penalties?  Depends on regular season vs playoffs!
  if( ( as.numeric(game_info$session_id) == 1 || as.numeric(game_info$session_id) == 2 ) ) {

    mandown_times_regulation_h <- mandown_times_h %>% filter( num_players <= 5, end_period <= 3 )
    mandown_times_regulation_a <- mandown_times_a %>% filter( num_players <= 5, end_period <= 3 )

    if( game_info$season < "20152016" ) {
      # Through 2014-2015, 4on4 OT.
      mandown_times_ot_h <- mandown_times_h %>% filter( num_players <= 4, end_period == 4 )
      mandown_times_ot_a <- mandown_times_a %>% filter( num_players <= 4, end_period == 4 )

      # Short-handed team in OT 4on3 takes another penalty --> 5on3
      mandown_times_ot_h_5on3 <- mandown_times_a %>% filter( num_players >= 6, end_period == 4 )
      mandown_times_ot_a_5on3 <- mandown_times_h %>% filter( num_players >= 6, end_period == 4 )

      mandown_times_ot_h <- bind_rows( mandown_times_ot_h, mandown_times_ot_h_5on3 )
      mandown_times_ot_a <- bind_rows( mandown_times_ot_a, mandown_times_ot_a_5on3 )

    } else {
      # Staring 2015-2016, 3on3 OT.  Home team has penalty if Away team has 5 or more players.
      mandown_times_ot_h <- mandown_times_a %>% filter( num_players >= 5, end_period == 4 )
      mandown_times_ot_a <- mandown_times_h %>% filter( num_players >= 5, end_period == 4 )
    }

    mandown_times_h <- rbind( mandown_times_regulation_h, mandown_times_ot_h )
    mandown_times_a <- rbind( mandown_times_regulation_a, mandown_times_ot_a )

  } else {
    mandown_times_h <- mandown_times_h %>% filter( num_players < 6 )
    mandown_times_a <- mandown_times_a %>% filter( num_players < 6 )
  }

  mandown_times_h <- mandown_times_h %>% mutate(
    team_ha   = "H",
    down_team = game_info$home_team_short
  )
  mandown_times_a <- mandown_times_a %>% mutate(
    team_ha   = "A",
    down_team = game_info$away_team_short
  )

  bind_rows( mandown_times_h, mandown_times_a )
}

#'  Get data frame of goals from playbyplay for a single game.
#'  Handles SO and OT goal cases.
#'  For SO goal, grabs arbitrary goal from winning team.
#'
#' @param pbp_df A data frame of playbyplay
#' @param game_info A data frame except of a single game from stage_game
#' @return Data frame of goals with time as start_cum
#' @export
get_goals_from_pbp <- function( pbp_df, game_info ) {
  goals_df <- pbp_df %>% filter( event_type == "GOAL" )

  winning_team_short <- with( game_info, ifelse( home_score_final > away_score_final, home_team_short, away_team_short ) )

  ## Shootout case: Filter out all SO goals except last one
  if( (game_info$session_id == "1" || game_info$session_id == "2") && max(pbp_df$period) == 5 ) {
    # SO scores keep incrementing and the way the data is written to db, we *don't* know SO winner (oh no)
    ot_score  <- pbp_df %>% filter( event_type == "PEND", period == 4 ) # grab pre-SO score
    so_goals  <- goals_df %>% filter( period == 5 )
    # arbitrarily grab first goal by winning team
    so_winner <- so_goals %>% filter( event_team == winning_team_short ) %>% head(1)

    so_winner$away_score <- ot_score$away_score
    so_winner$home_score <- ot_score$home_score

    goals_df <- goals_df %>% filter( period < 5 )
    goals_df <- bind_rows( goals_df, so_winner )
  }

  goals_df
}


#'  Count for and against stats by player (ha_number) for a
#'  single game.
#'
#' @param pbp A data frame of playbyplay or shots
#' @param output_col_names A vector of names for output table
#' @param reduce_cols Return just results.  True by default
#' @return data frame with columns ha_number, for_col, away_col
#' @export
tally_ha_number_for_against <- function( pbp, output_col_names = c( "for", "against"), reduce_cols = TRUE ) {
  # Error cases first
  if( !nrow( pbp ) ) {
    retval <- data_frame( ha_number=c( "H", "A") )
    retval[ , output_col_names ] <- 0

    return( retval )
  }

  event_ha_col <- unlist( pbp$event_team_ha )
  if( !all( unique( event_ha_col ) %in% c( "H", "A")) ) {
    # Uh oh. we've got some weird events where PBP did not specify a team and nhlscrapr did not grab a team name or H/A.
    bad_events <- pbp %>% filter( !event_team_ha %in% c( "H", "A" ) )

    message( "Removing non-standard event in pbp: ", bad_events$etext )
    pbp <- pbp %>% filter( event_team_ha %in% c( "H", "A" ) )
    event_ha_col <- unlist( pbp$event_team_ha )

    if( !nrow( pbp ) ) {
      retval <- data_frame( ha_number=c( "H", "A") )
      retval[ , output_col_names ] <- 0

      return( retval )
    }
  }

  # let's go
  on_ice_ha_numbers <- paste( "H", "A", pbp$on_ice_ha_numbers, pbp$away_goalie, pbp$home_goalie )
  on_ice_ha_numbers <- sapply( on_ice_ha_numbers, function( my_cell) gsub( " NA|NA ", "", my_cell ) )

  event_df <- data_frame( event_ha=event_ha_col, ha_number=on_ice_ha_numbers )
  event_df <- event_df %>% mutate( ha_number=strsplit( ha_number, " " ) ) %>% unnest()
  ha_table <- event_df %>% group_by( event_ha, ha_number ) %>% dplyr::summarize( count=n() )
  ha_table <- ha_table %>% spread( event_ha, count, fill=0 )

  # if only H or A is represented, we will only get an H or A column but not both.  we want both columns.
  if( !"A" %in% event_ha_col ) {
    ha_table$A <- 0
  } else if( !"H" %in% event_ha_col ) {
    ha_table$H <- 0
  }

  # for and against cols are relative to player's team.  H goal is GF for H player, GA for A player
  ha_table <- ha_table %>% filter( nchar(ha_number) > 0 ) %>%
    mutate( team_ha = substr( ha_number, 1, 1 ) )

  ha_table[ output_col_names[1] ] <- ifelse( ha_table$team_ha == "H", ha_table$H, ha_table$A )
  ha_table[ output_col_names[2] ] <- ifelse( ha_table$team_ha == "A", ha_table$H, ha_table$A )

  if( reduce_cols ) ha_table <- ha_table[ , c( "ha_number", output_col_names ) ]

  ha_table
}
