#' Get linemates by game
#'
#' @param this_player_name String of first_last_name (capitalize first letters only)
#' @param this_season Season string, "20152016" by default.
#' @param this_session_id Session ID, "2" by default.  "all" will be both "2" and "3".
#' @param player_tbl tbl of "player" from db.  already collected.
#' @param game_h2h tbl of "game_h2h".  Uncollected.
#' @param team_score tbl of "team_score".  Uncollected.
#'
#' @return data frame with game_number, num_last_name_2_label, toi_pct
#' @export
#'
get_linemates_by_game <- function(
  this_player_name,
  this_season="20152016",
  this_session_id="2",
  player_tbl,
  game_h2h,
  this_player_team_games ) {

  this_player <- player_tbl %>% filter( first_last_name==this_player_name ) %>% collect()
  this_player_num_last_name <- paste( this_player$number, this_player$last_name )
  this_player_id            <- this_player$nhl_id %>% as.numeric()
  this_player_fd            <- this_player$position_fd
  # this_team_short           <- this_player$team_short

#  ### ASSUMPTION: player is NOT traded.  using his current team_short from player_tbl and grabbing all games for that team
#  # A more advanced version of this function should provide a df with game_number, game_id4
  # this_team_games <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short==this_team_short ) %>%
  #                                   select( game_number, season, session_id, game_id4, ha, opp_team_short ) %>%
  #                                   arrange( game_date ) %>% collect()

  this_player_team_games <- this_player_team_games %>% arrange( game_date ) %>% collect()
  if( this_session_id == "all" ) {
    these_session_ids <- c( "2", "3" )
  } else {
    these_session_ids <- c( this_session_id )
  }

  # dplyr 0.5 no longer allows arrange() within group
  linemates <- game_h2h %>% filter( season==this_season, session_id %in% these_session_ids, game_id4 %in% this_player_team_games$game_id4,
                                    filter_score_diff=="all", filter_strength=="ev5on5",
                                    nhl_id_1==this_player_id, team_comp=="T" ) %>%
                            group_by( game_id4 ) %>% arrange( desc(toi_period_all) ) %>% collect()
  linemates <- linemates %>% left_join( player_tbl %>%
                                        select( nhl_id_2=nhl_id, number_2=number, last_name_2=last_name, position_fd_2=position_fd ),
                                        by="nhl_id_2" ) %>%
                             filter( position_fd_2==this_player_fd )
  linemates <- linemates %>% select( game_date, game_id4, toi=toi_period_all, nhl_id_2, number_2, last_name_2 ) %>%
                             mutate( num_last_name_2=paste( number_2, last_name_2 ) )

  # Top linemates, by game.
  # Filter for
  #   top 4 (5 including self) for F, top 3 for D
  #   > 20% of this player's TOI in a particular game
  linemate_limit <- ifelse( this_player_fd=="F", 5, 4 )
  top_linemates  <- linemates %>% filter( min_rank(desc(toi)) <=linemate_limit ) %>%
                                  mutate( toi_pct=round( toi/max(toi), 3) ) %>% filter( toi_pct > 0.20 ) %>%
                                  select( game_id4, num_last_name_2, toi, toi_pct )
  linemates_num_last_name <- top_linemates$num_last_name_2 %>% unique() # list of all linemates

  # Top linemates over full season.  Want to put TOI% in axislabel
  # require at least 1% of TOI%
  linemates_season <- linemates %>% ungroup() %>% group_by( nhl_id_2, num_last_name_2 ) %>%
                                    summarise( toi = sum(toi) ) %>% ungroup() %>%
                                    mutate(
                                      toi_pct_total         = round( 100*(toi/max(toi)) ),
                                      num_last_name_2_label = paste0( num_last_name_2, "\n",
                                                              round(toi), " min (", toi_pct_total, "%)" )
                                    ) %>%
                                    filter( toi_pct_total >= 1 ) %>% # must have at least 1% of TOI overlap
                                    arrange( desc(toi) )
  linemates_season <- linemates_season %>% filter( num_last_name_2 %in% linemates_num_last_name )
  this_player_num_last_name_label <- linemates_season$num_last_name_2_label[1]

  # put it all together by game. Start with team_games so we can complete() missing games
  this_player_games <- this_player_team_games %>% left_join( top_linemates, by="game_id4" ) %>%
                                           left_join( linemates_season %>%
                                                      select( num_last_name_2, num_last_name_2_label ), by="num_last_name_2" )
  this_player_games$num_last_name_2_label <- factor( this_player_games$num_last_name_2_label,
                                                     level=rev(linemates_season$num_last_name_2_label) )

  this_player_games_fill <- this_player_games %>% complete( player_game_number, num_last_name_2_label, fill=list(toi_pct=0) )

  # partition: self only (EV) vs linemates
  this_player_toi_games  <- this_player_games_fill %>% filter( num_last_name_2_label == this_player_num_last_name_label )
  this_player_games_fill <- this_player_games_fill %>% filter( num_last_name_2_label != this_player_num_last_name_label )

  list( toi_ev5on5=this_player_toi_games, linemates_ev5on5=this_player_games_fill )
}


#' Get player stats by game
#'
#' @param this_first_last_name String of player first last name
#' @param this_season String like "20152016"
#' @param this_session_id "2" or "3".   "all" will be both "2" and "3".
#' @param player_tbl Table df
#' @param game_player game_player table, NOT collected
#' @param team_score team_score table, NOT collected
#' @param overwrite_ev5on5_toi Flag for whether to overwrite ev5on5 TOI with all - sh - pp TOI. Default is TRUE.
#'
#' @return data frame
#' @export
#'
get_player_stats_by_game <- function(
  this_first_last_name,
  this_season="20152016",
  this_session_id="2",
  player_tbl,
  game_player,
  this_player_team_games,
  overwrite_ev5on5_toi=TRUE ) {

  this_player <- player_tbl %>% filter( first_last_name==this_first_last_name ) %>% collect()
  this_player_num_last_name <- paste( this_player$number, this_player$last_name )
  this_player_id            <- this_player$nhl_id %>% as.numeric()
  this_player_fd            <- this_player$position_fd

  if( this_session_id == "all" ) {
    these_session_ids <- c( "2", "3" )
  } else {
    these_session_ids <- c( this_session_id )
  }
  gp <- game_player %>%
        filter( nhl_id==this_player_id, season==this_season, session_id %in% these_session_ids,
                filter_period=="all", filter_score_diff=="all", filter_strength %in% c( "all", "ev5on5", "pp", "sh" )
        ) %>% collect()


  # this_team_short           <- this_player$team_short

  ### ASSUMPTION: player is NOT traded.  using his current team_short from player_tbl and grabbing all games for that team
  # A more advanced version of this function should provide a df with game_number, game_id4
  # this_team_games <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short==this_team_short ) %>%
  #   select( game_number, season, session_id, game_id4, ha, opp_team_short ) %>%
  #   arrange( game_date ) %>% collect()

  this_player_team_games <- this_player_team_games %>% arrange( game_date ) %>% collect()

  gp_select <- gp %>% select( game_date, game_id4, filter_strength,
                        toi, toi_pct,
                        g, a, p, cf, ca, c_net, cf_pct, cf_pct_rel, ff, fa, f_net, ff_pct, ff_pct_rel,
                        zs_o, zs_n, zs_d, zs_o_pct, zs_o_pct_rel, gf, ga, g_net, sf, sa, s_net, sf_pct, sf_pct_rel,
                        fo_w, fo_l, pen_i, pen_draw_i, sf_i,
                        toi_pct_team, toi_pct_team_f, toi_pct_team_d,
                        toi_pct_comp, toi_pct_comp_f, toi_pct_comp_d ) %>% arrange( game_date )

  if( overwrite_ev5on5_toi ) {
    toi_all   <- gp_select %>% filter( filter_strength=="all"    ) %>% select( game_date, game_id4, toi )
    toi_ev5on5<- gp_select %>% filter( filter_strength=="ev5on5" ) %>% select( game_date, game_id4, toi_ev5on5=toi )
    toi_pp_sh <- gp_select %>% filter( filter_strength %in% c( "pp", "sh" ) ) %>%
      group_by( game_date, game_id4 ) %>% summarise( toi_special=sum(toi) )

    toi_overwrite   <- toi_all %>% left_join( toi_pp_sh, by=c( "game_date", "game_id4" ) ) %>%
      left_join( toi_ev5on5, by=c( "game_date", "game_id4" ) ) %>%
      mutate( toi_remainder=toi-toi_special )

    if( nrow(toi_all) != nrow(toi_overwrite) ) {
      stop( "nrow between TOI vectors do not match" )
    }

    gp_select$toi[ gp_select$filter_strength=="ev5on5" ] <- toi_overwrite$toi_remainder

  }

  this_player_games <- this_player_team_games %>% left_join( gp_select, by=c( "game_id4", "game_date"  ) )

  this_player_games
}


#' Group roster by lines from h2h TOI data
#'
#' Group forward line & D pairs together within each team's roster
#'
#' @param roster Augmented roster with faceoff count and shot added.
#' @param toi_h2h_ev Data frame from game_h2h filtered for strength and score state
#'
#' @return Data frame of roster sorted by lines and D pairs.
#' @export
group_roster_by_lines_from_h2h <- function( roster, toi_h2h_ev ) {

  # join individual ev TOI.  missing for G
  toi_individual <- toi_h2h_ev %>% filter( ha_number_1==ha_number_2 )
  roster <- roster %>% left_join( toi_individual %>% select( ha_number=ha_number_1, toi_ev5on5=toi_period_all), by="ha_number" )
  roster$position_fd     <- ifelse( roster$position=="D", "D", ifelse( roster$position=="G", "G", "F") )
  roster$rank_toi_ev5on5 <- roster$rank_toi_ev5on5_adj <- NA

  toi_h2h_ev <- toi_h2h_ev %>% select( starts_with( "ha_number"), toi=toi_period_all ) %>%
    left_join( roster %>% select( ha_number_1=ha_number, team_ha_1=team_ha, position_fd_1=position_fd ), by="ha_number_1" ) %>%
    left_join( roster %>% select( ha_number_2=ha_number, team_ha_2=team_ha, position_fd_2=position_fd ), by="ha_number_2" )

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

      roster_copy  <- roster  %>% filter( team_ha == HA, position_fd == FD )
      toi_h2h_copy <- toi_h2h_ev %>% filter( team_ha_1 == HA, team_ha_2 == HA, position_fd_1 == FD, position_fd_2 == FD )
      n_players_remaining <- nrow( roster_copy )
      while( n_players_remaining > 0 ) {
        these_players <- data_frame()

        ## Sort remaining players by *individual* TOI, Hi -> Lo
        roster_copy <- roster_copy %>% arrange( desc(toi_ev5on5) )

        ## Assign top player the best rank remaining (best = 1)
        top_ha_number <- roster_copy$ha_number[1]
        roster$rank_toi_ev5on5[ roster$ha_number == top_ha_number ] <- rank
        these_players <- bind_rows(
          these_players,
          data_frame( ha_number = top_ha_number, rank = rank )
        )
        rank <- rank + 1

        ## Remove this guy from roster copy, which we'll use to find top linemate
        roster_copy <- roster_copy[-1,]
        n_players_remaining <- n_players_remaining - 1

        ## Calc *shared* TOI with this player and re-sort remaining players by this shared TOI
        shared_toi <-toi_h2h_copy %>% filter( ha_number_1 == top_ha_number,
          ha_number_2 %in% roster_copy$ha_number ) %>% arrange( desc(toi))

        these_players <- bind_rows(
          these_players,
          data_frame( ha_number = shared_toi$ha_number_2[1:additional_players],
            rank      = seq(from=rank,length=additional_players) )
        )
        rank <- rank + additional_players

        ## Remove from roster copy
        roster_copy <- roster_copy %>% anti_join( these_players, by="ha_number" )
        n_players_remaining <- nrow( roster_copy )

        apply( these_players, 1, function(x)
          roster$rank_toi_ev5on5[ roster$ha_number==x["ha_number"] ] <<- x[ "rank"]
        )


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
                arrange( shoots, position_gc, rank_toi_ev5on5 )
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
        these_players_sorted$rank_toi_ev5on5_adj <- sort( as.numeric(these_players_sorted$rank_toi_ev5on5 ) )
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






#' Group rosters by lines from h2h TOI data
#'
#' Group forward line & D pairs together within each team's roster.
#' Across games, this can get messy so we need to establish some rules:
#'  1. every player on a team appears exactly once
#'  2. Forward lines of 3 F, 3 pairs of D
#'  3. excess F and D are appended on in descending order of TOI all strength
#'
#' rosters_C has Centers identified already with is_center=TRUE.  There will be as many
#' forward lines as there are Centers.
#'
#' @param roster Augmented roster C via augment_rosters_C(). Faceoff count and shot added
#' @param toi_h2h_ev Data frame from game_h2h filtered for strength and score state
#'
#' @return Data frame of roster sorted by lines and D pairs.
#' @export
group_multigame_rosters_by_lines <- function( rosters_C, toi_h2h_ev ) {
  toi_h2h <-aggregate_toi_h2h( toi_h2h_ev, rosters_C )
  toi_h2h_T <- toi_h2h %>% filter( team_comp=="T", nhl_id_1 != nhl_id_2 )  # need consider teammates only

  rosters_C$rank <- NA
  teams <- rosters_C$team_short %>% unique()

  rank <- 1 # running incremental rank through all teams.  when done, rank = nrow(rosters_C)
  for( this_team_short in teams ) {
    # Forwards first ----------------------------------------------------------
    additional_players <- 2
    C_only  <- rosters_C %>% filter( team_short == this_team_short, position_fd == "F",  is_center ) %>% arrange( desc(toi) ) # sort by TOI/gm
    non_C   <- rosters_C %>% filter( team_short == this_team_short, position_fd == "F", !is_center )
    n_C     <- nrow( C_only )
    n_non_C <- nrow( non_C  )

    toi_h2h_copy <- toi_h2h_T %>% filter(
      team_short_1  == this_team_short, team_short_2 == this_team_short,
      position_fd_1 == "F", position_fd_2 == "F", !nhl_id_2 %in% C_only$nhl_id  # we're looking for linemates of C
    )
    while( n_C > 0 ) {
      this_line <- data_frame()
      top_C_id <- C_only$nhl_id[1]

      # write top_C rank to rosters_C
      rosters_C$rank[ rosters_C$nhl_id == top_C_id ] <- rank
      this_line <- data_frame( nhl_id = top_C_id, rank = rank )
      rank <- rank + 1

      ## Remove this guy from roster copy
      C_only <- C_only %>% filter( nhl_id != top_C_id )
      n_C    <- nrow( C_only )

      ## Calc *shared* TOI with this player and re-sort remaining players by this shared TOI
      shared_toi <- toi_h2h_copy %>% filter( nhl_id_1 == top_C_id, !nhl_id_2 %in% C_only$nhl_id ) %>%
        arrange( desc(toi) )

      this_line <- bind_rows(
        this_line,
        data_frame(
          nhl_id = shared_toi$nhl_id_2[1:additional_players],
          rank   = seq( from=rank, length=additional_players ) )
      )
      rank <- rank + additional_players

      ## Remove linemates from non_C and toi_h2h_copy
      non_C   <- non_C %>% anti_join( this_line, by="nhl_id" )
      n_non_C <- nrow( non_C )
      toi_h2h_copy <- toi_h2h_copy %>% filter( !nhl_id_2 %in% this_line$nhl_id )

      # Tweak order within lines.  C first, then L then R.
      this_line <- this_line %>% left_join( rosters_C %>% select( nhl_id, faceoff_cnt, shoots, position, toi ), by="nhl_id" )

      this_line_sorted <- bind_rows(
        this_line %>% slice(1),
        this_line %>% slice(-1) %>% arrange( shoots, position, desc(toi ) )
      )

      # overwrite rank with  this new sort
      this_line_sorted$rank <- sort( as.numeric(this_line_sorted$rank ) )

      # write these new adj ranks to roster
      apply( this_line_sorted, 1, function(x)
        rosters_C$rank[ rosters_C$nhl_id==x["nhl_id"] ] <<- x["rank"]
      )
    } # while n_C > 0

    # no more C remaining.  appending remaining F
    if( n_non_C ) {
      this_line_sorted <- non_C %>% arrange( desc(toi) )
      this_line_sorted$rank <- seq( from=rank, length=n_non_C )
      rank <- rank + n_non_C

      apply( this_line_sorted, 1, function(x)
        rosters_C$rank[ rosters_C$nhl_id==x["nhl_id"] ] <<- x["rank"]
      )
    }

    # Defensemen --------------------------------------------------------------

    additional_players <- 1
    D_only   <- rosters_C %>% filter( team_short == this_team_short, position_fd == "D" ) %>% arrange(desc(toi))
    n_D      <- nrow( D_only  )
    toi_h2h_copy <- toi_h2h_T %>% filter(
      team_short_1 == this_team_short, team_short_2 == this_team_short,
      position_fd_1 == "D", position_fd_2 == "D"
    )
    n_D_pairs <- 3
    while( n_D_pairs > 0 ) {
      this_line <- data_frame()
      top_D_id <- D_only$nhl_id[1]

      # write top_C rank to rosters_C
      rosters_C$rank[ rosters_C$nhl_id == top_D_id ] <- rank
      this_line <- data_frame( nhl_id = top_D_id, rank = rank )
      rank <- rank + 1

      ## Remove this guy from roster copy
      D_only <- D_only %>% filter( nhl_id != top_D_id )
      n_D    <- nrow( D_only )

      ## Calc *shared* TOI with this player and re-sort remaining players by this shared TOI
      shared_toi <- toi_h2h_copy %>% filter( nhl_id_1 == top_D_id, nhl_id_2 != top_D_id ) %>% arrange( desc(toi) )

      this_line <- bind_rows(
        this_line,
        data_frame( nhl_id = shared_toi$nhl_id_2[1:additional_players],
          rank   = seq( from=rank, length=additional_players ) )
      )
      rank <- rank + additional_players

      ## Remove linemates from non_C and toi_h2h_copy
      D_only   <- D_only %>% anti_join( this_line, by="nhl_id" ) %>% arrange( desc(toi) )
      n_D      <- nrow( D_only )
      toi_h2h_copy <- toi_h2h_copy %>% filter( !nhl_id_2 %in% this_line$nhl_id )

      # Tweak order within lines.  C first, then L then R.
      this_line <- this_line %>% left_join( rosters_C %>% select( nhl_id, faceoff_cnt, shoots, position, toi ), by="nhl_id" )
      this_line_sorted <- this_line %>% arrange( shoots, desc(toi ) )

      # overwrite rank with  this new sort
      this_line_sorted$rank <- sort( as.numeric(this_line_sorted$rank ) )

      # write these new adj ranks to roster
      apply( this_line_sorted, 1, function(x)
        rosters_C$rank[ rosters_C$nhl_id==x["nhl_id"] ] <<- x["rank"]
      )
      n_D_pairs <- n_D_pairs-1
    } # while n_D_pairs > 0

    # no more D remaining.  appending remaining D
    if( n_D ) {
      this_line_sorted      <- D_only %>% arrange( desc(toi) )
      this_line_sorted$rank <- seq( from=rank, length=n_D )
      rank <- rank + n_D

      apply( this_line_sorted, 1, function(x)
        rosters_C$rank[ rosters_C$nhl_id==x["nhl_id"] ] <<- x["rank"]
      )
    }

    # Goalies
    this_line_sorted      <-  rosters_C %>% filter( team_short == this_team_short, position_fd == "G" ) %>% arrange(desc(gm))
    n_G <- nrow( this_line_sorted )
    this_line_sorted$rank <- seq( from=rank, length=n_G )
    rank <- rank + n_G

    apply( this_line_sorted, 1, function(x)
      rosters_C$rank[ rosters_C$nhl_id==x["nhl_id"] ] <<- x["rank"]
    )

  } # for

  rosters_C$rank <- as.numeric( rosters_C$rank )
  return( rosters_C %>% arrange( rank ) )
}



#' Aggregate toi_h2h
#'
#' @param toi_h2h_ev Ev5on5 toi_h2h data across multiple games
#' @param rosters_C Roster augmented with C info
#'
#' @return toi_h2h A summarized toi_h2h
#' @export
#'
aggregate_toi_h2h <- function( toi_h2h_ev, rosters_C, overweight_recent_game=F ) {

  if( overweight_recent_game ) {
    # "most recent game" means something different to different players

  }

  toi_h2h <- toi_h2h_ev %>% group_by( nhl_id_1, nhl_id_2, team_comp ) %>% # note team_comp needed in case player traded, e.g., Jagr
    summarise( toi=sum(toi_period_all) ) %>% ungroup()

  rosters_1 <- rosters_2 <- rosters_C
  names( rosters_1 ) <- paste0( names(rosters_1), "_1" )
  names( rosters_2 ) <- paste0( names(rosters_2), "_2" )

  # CAREFUL. A traded player will appear in two rows in rosters_C
  toi_h2h <- toi_h2h %>%
    left_join( rosters_1, by="nhl_id_1" ) %>%
    left_join( rosters_2, by="nhl_id_2" )
  # arrange( team_short_1, position_fd_1, desc(toi_1), team_short_2, position_fd_2, desc(toi_2) )
  toi_h2h
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
  faceoff_cnt_df  <- data_frame( ha_number=c( faceoffs_ev5on5$event_player1, faceoffs_ev5on5$event_player2 ) )
  centers_df      <- faceoff_cnt_df %>% group_by( ha_number ) %>% summarize( faceoff_cnt=n() )

  # Also nice to have L/R shot for D pairing.
  roster_augmented <- roster %>% arrange( ha_number )
  roster_augmented <- roster_augmented %>%
    left_join( centers_df, by="ha_number" ) %>%
    left_join( player %>% select( nhl_id, shoots ), by="nhl_id" ) %>%
    mutate( num_last_name=paste( number, last_name ) )

  # check if num_last_name is unique.  if not, insert first initial
  # 15 SMITH is on both OTT and NSH

  roster_augmented
}


#' Augment roster with faceoff count, center designation, num_last_name
#'
#' @param roster Data frame as from stage_game for arbitrary number of games
#' @param pbp_df Data frame of play by play
#' @param player_tbl Plyaer table
#' @param center_faceoff_rank_cutoff Cutoff for number of C per team
#'
#' @return Data frame of rosters with additional columns: faceoff count, is_center, num_last_name, L/R
#' @export
#'
augment_rosters_C <- function( roster, pbp_df, player_tbl, center_fo_cutoff=4 ) {
  # Figure out our centers. Make C the first row on a forward line.
  faceoffs_ev5on5 <- pbp_df %>% filter( event_type=="FAC", ev5on5 ) %>%
                                select( event_player1, event_p1_id, event_player2, event_p2_id, event_team_ha )

  # need to summation at H, A level and not nhl_id because faceoff win is determined by ha
  fac_a <- faceoffs_ev5on5 %>% group_by( event_player1, event_p1_id ) %>%
    summarise( fo_w = sum( event_team_ha == substr(event_player1, 1, 1) ),
               fo_l = sum( event_team_ha != substr(event_player1, 1, 1) ) ) %>%
    rename( ha_number=event_player1, nhl_id=event_p1_id )

  fac_h <- faceoffs_ev5on5  %>% group_by( event_player2, event_p2_id ) %>%
    summarize( fo_w = sum( event_team_ha == substr(event_player2, 1, 1) ),
               fo_l = sum( event_team_ha != substr(event_player2, 1, 1) ) ) %>%
    rename( ha_number = event_player2, nhl_id=event_p2_id )

  fac_player <- bind_rows( fac_a, fac_h ) %>% select( -ha_number )
  fac_player <- fac_player %>% group_by( nhl_id ) %>% summarize(
                  fo_w = sum( fo_w ),
                  fo_l = sum( fo_l )
  ) %>% ungroup()

  faceoff_cnt_df <- fac_player %>% mutate(
                                          faceoff_cnt = fo_w + fo_l,
                                          fo_pct      = round( fo_w / faceoff_cnt, 3 )
                                   )

  # faceoff_cnt_df  <-    data_frame( nhl_id=c( faceoffs_ev5on5$event_p1_id, faceoffs_ev5on5$event_p2_id ) )

  # faceoff_cnt_df  <- data_frame( nhl_id=c( faceoffs_ev5on5$event_p1_id, faceoffs_ev5on5$event_p2_id ) )
  # centers_df      <- faceoff_cnt_df %>% group_by( nhl_id ) %>% summarize( faceoff_cnt=n() )

  # CAREFUL.  player can get traded and play against former team
  roster_augmented <- roster %>% group_by( team_short, nhl_id ) %>%
    summarize( gm = n(), toi = round( mean(toi_total), 3) ) %>% ungroup()
  roster_augmented <- roster_augmented %>%
    left_join( player_tbl %>% select( nhl_id, number, last_name, shoots, position, position_fd ), by="nhl_id" ) %>%
    left_join( faceoff_cnt_df, by= "nhl_id"  ) %>%
    mutate( num_last_name=paste( number, str_to_upper(last_name) ) )

  roster_augmented[ is.na(roster_augmented) ] <- 0
  # TO DO: check if num_last_name is unique.  if not, insert first initial
  # 15 SMITH is on both OTT and NSH

  # Top 4 faceoff count on each team is C
  roster_augmented <- roster_augmented %>% group_by( team_short ) %>% arrange( desc(faceoff_cnt ) ) %>%
    mutate( is_center=row_number() <= center_fo_cutoff )

  roster_augmented$position_fd <- factor( roster_augmented$position_fd, levels=c("F", "D", "G") )
  retval <- roster_augmented %>% group_by( team_short, position_fd ) %>% arrange( desc(toi), position_fd )

  retval %>% ungroup()
}

