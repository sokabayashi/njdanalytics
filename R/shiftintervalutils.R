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

#' Get mandown intervals
#'
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

#' Group roster by lines
#'
#' Group forward line & D pairs together within each team's roster
#'
#' @param roster Augmented roster with faceoff count and shot added.
#' @param toi_matrix Matrix of Ev5on5 ice time with ha_number as row and col names.
#' @param strength String, "ev5on5" by default.  Not used currently.
#'
#' @return Data frame of roster sorted by lines and D pairs.
#' @export
group_roster_by_lines <- function( roster, toi_matrix, strength="ev5on5" ) {
  # individual total TOI
  roster$toi_ev5on5    <- diag( toi_matrix )
  roster$position_fd    <- ifelse( roster$position=="D", "D", "F" )
  roster$rank_toi_ev5on5_adj <- NA

  # this is for EV5on5.  But what happens if PK?
  # want to see units together: 1st unit 2F + 2D, 2nd unit 2F + 2D
  # Some players play F for some games and D for others...
  ## Can add a secondary filter by game?  roster variable has game_id4
  ## Byfuglien
  # roster$positionFD[ as.numeric(roster$nhl_id) == 8470834 ] <- "F"

  rank <- 1
  ## for each team. A BEFORE H.  CONSISTENT WITH toi.roster creation.
  for( HA in c( "A", "H" ) ) {
    ## for each positionFD
    for( FD in c("F", "D") ) {

      if( FD == "F" ) {
        additional_players <- 2
      } else {
        additional_players <- 1
      }

      roster.copy <- roster %>% filter( team_ha == HA, position_fd == FD )
      n_players_remaining <- nrow( roster.copy )
      while( n_players_remaining > 0 ) {
        these_players <- data_frame()

        ## Sort remaining players by *individual* TOI, Hi -> Lo
        roster.copy <- roster.copy %>% arrange( desc(toi_ev5on5) )

        ## Assign top player the best rank remaining (best = 1)
        top_ha_number <- roster.copy$ha_number[1]
        roster$rank_toi_ev5on5[ roster$ha_number == top_ha_number ] <- rank
        these_players <- bind_rows(
                                    these_players,
                                    data_frame( ha_number = top_ha_number, rank = rank )
                          )
        rank <- rank + 1

        ## Remove from roster copy
        roster.copy <- roster.copy[-1,]
        n_players_remaining <- n_players_remaining - 1

        ## Calc *shared* TOI with this player and re-sort remaining players by this shared TOI
        roster.copy$toi_pairwise <- toi_matrix[ top_ha_number, roster.copy$ha_number ]
        roster.copy <- roster.copy %>% arrange( desc(toi_pairwise) )

        for( i in 1:additional_players ) {
          ## Assign top player the best rank remaining (best = 1)
          top_ha_number <- roster.copy$ha_number[1]
          roster$rank_toi_ev5on5[ roster$ha_number == top_ha_number ] <- rank
          these_players <- bind_rows(
                              these_players,
                              data_frame( ha_number = top_ha_number, rank = rank )
                            )
          rank <- rank + 1

          ## Remove from roster copy
          roster.copy <- roster.copy[-1,]
          n_players_remaining <- n_players_remaining - 1
        }

        # Tweak order within lines.
        # for F, put C first.  can tell by faceoff count.
        these_players <- these_players %>%
          left_join( roster %>% select( ha_number, rank_toi_ev5on5, faceoff_cnt, shoots, position_gc ), by="ha_number" )
        if( FD=="F" ) {
          # took some faceoffs.  C, then L then R.
          if( these_players$faceoff_cnt %>% sum(na.rm=T) > 0 ) {
            these_players_sorted <- these_players %>% filter( faceoff_cnt==max(faceoff_cnt, na.rm=T) )
            these_players_sorted <- bind_rows(
                        these_players_sorted,
                        these_players %>% anti_join( these_players_sorted, by="ha_number") %>%
                          arrange( shoots, position_gc, -rank_toi_ev5on5 )
              )
          } else {
            # no faceoffs.  just sort by TOI.
            these_players_sorted <- these_players %>% arrange( rank_toi_ev5on5 )
          }

        } else {
          # For D, go L then R.
          these_players_sorted <- these_players %>% arrange( shoots )
        }
        # based on this new ordering, write down the adj rank
        these_players_sorted$rank_toi_ev5on5_adj <- sort( these_players_sorted$rank_toi_ev5on5 )
        # write these new adj ranks to roster
        apply( these_players_sorted, 1, function(x)
          roster$rank_toi_ev5on5_adj[ roster$ha_number==x[["ha_number"]] ] <<- x[["rank_toi_ev5on5_adj"]]
        )

      } # while
    } # for
  } # for

  roster$rank_toi_ev5on5_adj <- as.numeric( roster$rank_toi_ev5on5_adj )
  return( roster %>% arrange( rank_toi_ev5on5_adj ) )
}


#' Augment roster data frame
#'
#' Add faceoff count and shot (L/R) to roster
#'
#' @param roster Roster, as pulled from stage_roster.
#' @param pbp_df Play-by-play data, as pulled from stage_playbyplay tbl.
#' @param player Player data from player tbl.
#'
#' @return Data frame of roster with additional columns
#' @export
augment_roster <- function( roster, pbp_df, player ) {
  # Figure out our centers. Make C the first row on a forward line.
  faceoffs_ev5on5 <- pbp_df %>% filter( event_type=="FAC", ev5on5 )
  faceoff_cnt_df  <- data_frame( ha_number=c( faceoffs_ev5on5$event_player1, faceoffs_ev5on5$event_player2 ))
  centers_df      <- faceoff_cnt_df %>% group_by( ha_number ) %>% summarize( faceoff_cnt=n() )

  # Also nice to have L/R shot for D pairing.
  roster_augmented <- roster %>% filter( position != "G" ) %>% arrange( ha_number )
  roster_augmented <- roster_augmented %>%
                        left_join( centers_df, by="ha_number" ) %>%
                        left_join( player %>% select( nhl_id, shoots ), by="nhl_id" ) %>%
                        mutate( num_last_name=paste0( number, last_name ) )
  roster_augmented
}

#' Get EV5on5 TOI Matrix
#'
#' @param roster Roster for a game including Goalies, as pulled from stage_game tbl
#' @param shared_toi Shift interval, as pulled from `stage_shift_interval`.
#' Usually filtered for EV5on5.
#'
#' @return matrix TOI with ha_number as row and col names
#' @export
get_toi_matrix <- function(
                            roster, # must be full roster, including G
                            shared_toi ) {
  goalies <- roster %>% filter( position=="G" ) %>% select( ha_number ) %>% unlist()
  goalies_or <- paste(goalies, collapse="|") # could have multiple goalies per team.

  # all ha_numbers from both teams, excluding G
  ha_numbers <- roster %>% filter( position !="G" ) %>% select( ha_number ) %>%
    unlist(use.names = F) %>% sort()
  num_skaters <- ha_numbers %>% length()

  # focus just on one team and remove goalies.  Sort ha_numbers in alphabetical order
  # Q: will this sort guarantee that I can just focus on ther upper triangular matrix?
  # I THINK SO.
  toi_df   <- shared_toi %>% select( duration, on_ice_ha_numbers )

  toi_pairs_df <- toi_df %>% mutate(
    ha_numbers_list = on_ice_ha_numbers %>% gsub( goalies_or, "", . ) %>%
      str_extract_all( "(\\w+)" ) %>%
      llply( sort ),
    pair            = ha_numbers_list %>% laply( get_pairs_of_ha_numbers )
  )

  toi_pairs_unnested <- toi_pairs_df %>% select( duration, pair ) %>% unnest()
  toi_pairs_table    <- toi_pairs_unnested %>% group_by( pair ) %>% summarise(
    toi=sum(duration)
  )
  toi_pairs_table    <- toi_pairs_table %>% separate( pair, c( "ha_number_1", "ha_number_2" ))
  toi_pairs_table$ha_number_index_1 <- match( toi_pairs_table$ha_number_1, ha_numbers )
  toi_pairs_table$ha_number_index_2 <- match( toi_pairs_table$ha_number_2, ha_numbers )

  # Single player's TOI for diagonal
  toi_single_df <- toi_pairs_df %>% select( duration, ha_number=ha_numbers_list ) %>% unnest() %>%
    group_by( ha_number ) %>% summarise( toi=sum(duration ) )
  toi_single_df$ha_number_index_1 <- match( toi_single_df$ha_number, ha_numbers )
  toi_single_df$ha_number_index_2 <- toi_single_df$ha_number_index_1

  toi_matrix <- matrix( 0, nrow=num_skaters, ncol=num_skaters )
  rownames( toi_matrix ) <- colnames( toi_matrix ) <- ha_numbers
  # toi_matrix[ toi_pairs_table$ha_number_1, toi_pairs_table$ha_number_2 ] <- toi_pairs_table$toi
  toi_matrix[ toi_pairs_table[,4:5 ] %>% as.matrix() ] <- toi_pairs_table$toi
  toi_matrix[ toi_pairs_table[,5:4 ] %>% as.matrix() ] <- toi_pairs_table$toi
  toi_matrix[ toi_single_df[,3:4] %>% as.matrix()    ] <- toi_single_df$toi

  toi_matrix
}


# let's go ----------------------------------------------------------------

# library( njdanalytics )
# nhl_db <- setup_nhl_db()
#
# # grab a game
# this_season <- "20152016"
# this_game_id4 <- "0790"
#
# roster      <- tbl( nhl_db, "stage_roster"         ) %>% filter( season==this_season, game_id4==this_game_id4 ) %>% collect()
# pbp_df      <- tbl( nhl_db, "stage_playbyplay"     ) %>% filter( season==this_season, game_id4==this_game_id4 ) %>% collect()
# shared_toi  <- tbl( nhl_db, "stage_shift_interval" ) %>% filter( season==this_season, game_id4==this_game_id4 ) %>% collect()
# player_info <- tbl( nhl_db, "player" ) %>% filter( nhl_id %in% roster$nhl_id ) %>% collect()
#
# # append faceoff count and L/R shot
# roster_skaters <- augment_roster( roster, pbp_df, player_info )
# shared_toi_ev  <- shared_toi %>% filter( num_skaters_h==5, num_skaters_a==5, num_goalies_h==1, num_goalies_a==1 )
# toi_matrix_ev  <- get_toi_matrix( roster, shared_toi_ev ) # need to pass original roster
#
# # sort roster by lines, pairings.
# roster_sorted  <- group_roster_by_lines( roster_skaters, toi_matrix, strength="ev5on5" )



# shots_pairs_unnested <- shots_df %>% select( event_team_ha, pair ) %>% unnest()
# shots_pairs_table <- shots_pairs_unnested %>% group_by( pair ) %>% summarise(
#   A = sum(event_team_ha=="A"),
#   H = sum(event_team_ha=="H")
# )
#
# # check
# # shots_df %>% filter( event_team_ha=="A", grepl( "A02", on_ice_ha_numbers ), grepl( "A14", on_ice_ha_numbers ) ) %>%
# #       select( clock, event_team_ha, on_ice_ha_numbers )
#
# # ALL from OUR perspective, even if home
# shots_pairs_table_colnames <- names( shots_pairs_table )[-1]
# our_sf_col <- shots_pairs_table_colnames==game_info$our_ha
# shots_pairs_table_colnames[  our_sf_col ] <- "sf"
# shots_pairs_table_colnames[ !our_sf_col ] <- "sa"
# names( shots_pairs_table )[-1] <- shots_pairs_table_colnames
#
# shots_pairs_table <- shots_pairs_table %>% separate( pair, c("ha_number_1", "ha_number_2" ) )
#
# njd_chances_pairs <- shots_pairs_table %>% left_join(
#   this_roster %>% select( ha_number_1=ha_number, nhl_id_1=nhl_id ), by="ha_number_1"
# ) %>% left_join(
#   this_roster %>% select( ha_number_2=ha_number, nhl_id_2=nhl_id ), by="ha_number_2"
# )





