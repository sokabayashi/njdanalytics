
# Game Data functions ----------------------------------------------------------

#' @param shift_interval_df A data frame of shift intervals, from stage_shift_interval tbl
#' @param ha A character "H" or "A"
#' @return Data frame of all time intervals from a game with diff number of on ice players
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
#' @param shift_interval_df A data frame of shift intervals, from stage_shift_interval tbl
#' @param game_infoA data frame of game info from stage_game
#' @return Data frame of all man down time intervals from a game.
#' @export
get_mandown_intervals <- function( shift_interval_df, game_info ) {

  mandown_times_h <- get_num_player_runs_ha( shift_interval_df, "H" )
  mandown_times_a <- get_num_player_runs_ha( shift_interval_df, "A" )

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
