
#' spread_team_comp_results ----------------------------------------------------
#' Adapted from game_player_write_to_db_fn.R / spread_group_results()
#'  simplified here
#'       - don't have to deal with team perspective. already done for us
#'           - nhl_id instead of ha_number
#'  So while identical in practical way, needed a new version here

#' @param player_team_comp Data frame of player level results to spread
#' @param prefix_base String for prefix for
#' @param fd_flag Flag for separating F from D
#'
#' @return Data frame with results spread out
#' @export
#'
spread_team_comp_results <- function( player_team_comp, prefix_base="toi", fd_flag=T ) {
  value_var <- paste0( prefix_base, "_pct_group" )

  ## dcast to spread the results out
  dcast_formula <- "nhl_id_1 ~ team_comp"
  if( fd_flag ) dcast_formula <- paste0( dcast_formula, " + position_fd_2" )

  spread_results <- player_team_comp %>% dcast( dcast_formula, value.var= value_var )

  col_prefix <- paste0( prefix_base, "_pct" )
  col_names  <- names( spread_results )
  new_col_names <- c( "nhl_id", paste0( col_prefix, "_", col_names[ -1 ] ) ) %>% tolower()

  names( spread_results ) <- new_col_names

  spread_results
}


#' calc_shot_stats_basic ---------------------------------------------------
#' Derived shot statistics for indivdidual.
#'
#' Adapted from game_player_write_to_db_fn.R
#' @param game_player Data frame of player stats
#'
#' @return Data frame augment with more columns
#' @export
#'
calc_shot_stats_basic <- function( game_player ) {
  game_player %>% mutate(
    a             = a1 + a2,
    p             = g + a,
    sf_i          = sh_saved_i + g,
    ff_i          = sf_i + miss_i,
    cf_i          = ff_i + ab_i,

    sh_pct_i      = round( g / sf_i * 100, 3),
    f_sh_pct_i    = round( g / ff_i * 100, 3),
    fo_pct        = round( fo_w / (fo_w + fo_l) * 100, 3),
    zs_o_pct      = round( zs_o / (zs_o + zs_d) * 100, 3),
    #######
    # TEAM
    g_net         = gf - ga,
    gf_pct        = round( gf / (gf + ga) * 100, 3),

    sf            = sh_saved_f + gf,
    sa            = sh_saved_a + ga,
    s_net         = sf - sa,
    sf_pct        = round( sf / (sf + sa) * 100, 3),
    #########

    sh_pct        = round( gf / sf * 100, 3),
    sh_pct_a      = round( ga / sa * 100, 3),
    sv_pct        = round( (1 - ga/sa ) * 100, 3),

    ff            = sf + miss_f,
    fa            = sa + miss_a,
    f_net         = ff - fa,
    ff_pct        = round( ff / (ff + fa) * 100, 3),
    ########

    f_sh_pct      = round( gf / ff * 100, 3),
    f_sh_pct_a    = round( ga / fa * 100, 3),
    f_sv_pct      = round( (1 - ga / fa) * 100, 3),

    cf            = ff + ab,
    ca            = fa + bk,
    c_net         = cf - ca,
    cf_pct        = round( cf / (cf + ca) * 100, 3),
    c_sh_pct      = round( gf / cf * 100, 3),
    c_sh_pct_a    = round( ga / ca * 100, 3),
    c_sv_pct      = round( (1 - ga / ca) * 100, 3)
  )
}

# calc_shot_stats_rel ---------------------------------------------------

calc_shot_stats_rel <- function( game_player, corsi_adj_factor ) {
  game_player %>% mutate(
    zs_o_pct_off  = round( zs_o_off / (zs_o_off + zs_d_off) * 100, 3),
    zs_o_pct_rel  = zs_o_pct - zs_o_pct_off,

    gf_pct_off    = round( gf_off / (gf_off + ga_off) * 100, 3),
    gf_pct_rel    = gf_pct - gf_pct_off,

    sf_pct_off    = round( sf_off / (sf_off + sa_off) * 100, 3),
    sf_pct_rel    = sf_pct - sf_pct_off,

    ff_pct_off    = round( ff_off / (ff_off + fa_off) * 100, 3),
    ff_pct_rel    = ff_pct - ff_pct_off,

    cf_pct_off    = round( cf_off / (cf_off + ca_off) * 100, 3),
    cf_pct_rel    = cf_pct - cf_pct_off,

    ff_adj        = ff - corsi_adj_factor*(zs_o - zs_d ),
    fa_adj        = fa + corsi_adj_factor*(zs_o - zs_d ),
    ff_off_adj    = ff_off - corsi_adj_factor*(zs_o_off - zs_d_off ),
    fa_off_adj    = fa_off + corsi_adj_factor*(zs_o_off - zs_d_off ),

    cf_adj        = cf - corsi_adj_factor*(zs_o - zs_d ),
    ca_adj        = ca + corsi_adj_factor*(zs_o - zs_d ),
    cf_off_adj    = cf_off - corsi_adj_factor*(zs_o_off - zs_d_off ),
    ca_off_adj    = ca_off + corsi_adj_factor*(zs_o_off - zs_d_off ),

    cf_pct_adj      = round( cf_adj / (cf_adj + ca_adj) * 100, 3 ),
    cf_pct_off_adj  = round( cf_off_adj / (cf_off_adj + ca_off_adj) * 100, 3 ),
    cf_pct_rel_adj  = cf_pct_adj - cf_pct_off_adj,

    ff_pct_adj      = round( ff_adj / (ff_adj + fa_adj) * 100, 3 ),
    ff_pct_off_adj  = round( ff_off_adj / (ff_off_adj + fa_off_adj) * 100, 3 ),
    ff_pct_rel_adj  = ff_pct_adj - ff_pct_off_adj
  )
}


#' Aggregate player stats
#'
#' @param start_date String of date in YYYY-MM-DD format. Default is NULL which is fine if specifying by season
#' @param end_date String of date in YYYY-MM-DD format. Default is NULL which is fine if specifying by season
#' @param season Season string e.g. "20152016"
#' @param subset_count Integer of games to use for subset.  Default is NULL.
#' @param subset_head_tail String "head" or "tail" for specifying which end of subset.  Default is NULL.
#' @param session_id String "2" regular, "3" playoffs.  Default is NULL.
#' @param period String for filtering, "1", "2", "3", "4", ... "1or2" also okay.
#' @param strength ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
#' @param score_diff trail_1, trail_2, trail_3, tied, lead_1, lead_2, lead_3.  Not in game_player: leading, trailing. trail_2plus, lead_2plus.
#' @param team_only Default is FALSE
#' @param h2h_stats Default is FALSE
#' @param group_team Default is FALSE
#' @param home_away Default is "all"
#' @param ignore_team_games Vector of team_shorts to exclude all games from, e.g., 2015 BUF. Default is NULL
#' @param nhl_db DB objection to avoid repeated connections during multiple calls
#' @param corsi_adj_factor Double.  Default is 0.25 for faceoff diff adj on CR.
#'
#' @return Data frame of player stats
#' @export
#'
aggregate_player_stats <- function(
  start_date = NULL, # YYYY-MM-DD
  end_date   = NULL,
  season     = "20152016",
  subset_count     = NULL, # 41 = half season
  subset_head_tail = NULL, # "head" or "tail"
  session_id = NULL, # 2=regular, 3=playoffs
  period     = "all", # "1", "2", "3", "4",... Not in game_player: "1or2"
  strength   = "all", # ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en
  score_diff = "all", # trail_1, trail_2, trail_3, tied, lead_1, lead_2, lead_3.  Not in game_player: leading, trailing. trail_2plus, lead_2plus.
  team_only  = FALSE,
  h2h_stats  = FALSE,
  group_team = FALSE, # if TRUE, player w two teams will appear twice, e.g., Jagr on NJD and FLA with separate stats.
  home_away  = "all",
  ignore_team_games = NULL, # vector of team_shorts to exclude all games from, e.g., 2015 BUF
  nhl_db = NULL,
  corsi_adj_factor = 0.25
) {
  # load tables from db
  if( is.null( nhl_db ) ) {
    nhl_db <- src_postgres( dbname = "nhl", user = "postgres", password = "postgres" )
  }
  team_score <- tbl( nhl_db, "team_score" )

  if( team_only ) {
    gp_df  <- tbl( nhl_db, "game_player" ) %>% filter( nhl_id < 100 )
    h2h_df <- tbl( nhl_db, sql("SELECT * FROM game_h2h limit 1")) %>%
              collect() %>% slice(0)
    team_tbl <- tbl( nhl_db, "team" ) %>% collect() %>%
      filter(
        !name_short %in% ( c("PHX", "ATL" ) )
      )

  } else { # all
    gp_df  <- tbl( nhl_db, "game_player" )
  }

  if( h2h_stats ) {
    h2h_df <- tbl( nhl_db, "game_h2h"    )
  } else {
    h2h_df <- tbl( nhl_db, sql("SELECT * FROM game_h2h limit 1") ) %>%
      collect() %>% slice(0)
  }

  ## STRENGTH FILTER
  if( strength == "no_en" ) {
    gp_df  <- gp_df  %>% filter( !filter_strength %in% c( "comp_en", "own_en" ) )
    h2h_df <- h2h_df %>% filter( !filter_strength %in% c( "comp_en", "own_en" ) )
  } else {
    gp_df  <- gp_df  %>% filter( filter_strength == strength )
    h2h_df <- h2h_df %>% filter( filter_strength == strength )
  }

  ## SCORE DIFF FILTER
  nonstandard_score_diffs <- c( "leading", "trailing", "trail_2plus", "lead_2plus")
  leading_score_diffs     <- c( "lead_1",  "lead_2" , "lead_3"  )
  trailing_score_diffs    <- c( "trail_1", "trail_2", "trail_3" )
  if( score_diff %in% nonstandard_score_diffs  ) {
    if( score_diff == "leading" ) {
      gp_df  <- gp_df  %>% filter( filter_score_diff %in% leading_score_diffs )
      h2h_df <- h2h_df %>% filter( filter_score_diff %in% leading_score_diffs )

    } else if( score_diff== "trailing" ) {
      gp_df  <- gp_df  %>% filter( filter_score_diff %in% trailing_score_diffs )
      h2h_df <- h2h_df %>% filter( filter_score_diff %in% trailing_score_diffs )

    } else if( score_diff== "lead_2plus" ) {
      gp_df  <- gp_df  %>% filter( filter_score_diff %in% c("lead_2", "lead_3") )
      h2h_df <- h2h_df %>% filter( filter_score_diff %in% c("lead_2", "lead_3") )

    } else if( score_diff== "trail_2plus" ) {
      gp_df  <- gp_df  %>% filter( filter_score_diff %in% c("trail_2", "trail_3") )
      h2h_df <- h2h_df %>% filter( filter_score_diff %in% c("trail_2", "trail_3") )
    }
  } else {
    # standard score_diff - I hope!
    gp_df  <- gp_df  %>% filter( filter_score_diff == score_diff )
    h2h_df <- h2h_df %>% filter( filter_score_diff == score_diff )
  }

  # season instead of date range.  By default, use 20142015.
  this_season     <- season # make a copy here due to dplyr filtering with same var name..
  if( length(session_id) && session_id == "all" ) session_id <- NULL
  this_session_id <- as.character( session_id )

  # if date range specified, use that instead of anything else specified
  if( !is.null(start_date) || !is.null(start_date) ) {
    if( as.Date( start_date, format="%Y-%m-%d" ) %>% is.na() || as.Date( end_date, format="%Y-%m-%d" ) %>% is.na() ) {
      stop( start_date, " or ", end_date, " is not a valid date." )
    }
    date_range_flag <- TRUE
  } else {
    date_range_flag <- FALSE
  }

  ## DATE FILTER
  if( date_range_flag ) {
    gp_df <- gp_df %>% filter(
                              game_date >= start_date,
                              game_date <= end_date
                            )
    h2h_df <- h2h_df %>% filter(
                              game_date >= start_date,
                              game_date <= end_date
    )
    team_score <- team_score %>% filter(
      game_date >= start_date,
      game_date <= end_date
    )

    # might actually want session filter even when using date range filter
    if( length(this_session_id) ) {
      gp_df <- gp_df %>% filter(
        session_id == this_session_id
      )
      h2h_df <- h2h_df %>% filter(
        session_id == this_session_id
      )
      team_score <- team_score %>% filter(
        session_id == this_session_id
      )
    }

  } else {


    # No session specified
    if( is.null( this_session_id ) ) {
      gp_df <- gp_df %>% filter(
        season == this_season
      )
      h2h_df <- h2h_df %>% filter(
        season == this_season
      )
      team_score <- team_score %>% filter(
        season == this_season
      )

    } else {
      # Filter by session_id as well
      gp_df <- gp_df %>% filter(
                      season == this_season,
                      session_id == this_session_id
      )
      h2h_df <- h2h_df %>% filter(
                      season == this_season,
                      session_id == this_session_id
      )
      team_score <- team_score %>% filter(
        season == this_season,
        session_id == this_session_id
      )
    }
  }

  ## PERIOD FILTER
  if( period == "1or2" ) {
    gp_df  <- gp_df  %>% filter( filter_period %in% c("1" , "2") )
    h2h_df <- h2h_df %>% filter( (!is.na(toi_period_1) |  !is.na(toi_period_2) ) )
  } else {
    gp_df  <- gp_df  %>% filter( filter_period == period )

    # need to create period column name to extract from h2h table.
    period_col = paste0( "toi_period_", period )
    h2h_df <- eval( substitute(
      h2h_df %>% filter( !is.na(period_col) ),
      list( period_col = as.name(period_col) )
    ) )
  }


  ## HOME AWAY FILTER
  if( home_away != "all" ) {
    if( home_away == "home" || home_away == "H" ) {
      gp_df <- gp_df %>% filter( team_ha == "H" )
    } else if( home_away == "away" || home_away == "A" ) {
      gp_df <- gp_df %>% filter( team_ha == "A" )
    } else {
      stop( "home_away value not valid.")
    }
  }

  ## EXCLUDE ALL GAMES THAT ARE AGAINST THESE TEAMS
  if( !is.null( ignore_team_games ) ) {
    # Grab season, session_id, game_id4 against these opponents
    ignore_games <- data.frame()
    for( ignore_opp in ignore_team_games ) {
      ignore_set <- gp_df %>% filter( nhl_id < 100, team_short == ignore_opp ) %>%
        select( season, session_id, game_id4 ) %>% collect()
      if( nrow( ignore_set) ) {
        ignore_games <- rbind( ignore_games, ignore_set )
      }
    }

    gp_df <- gp_df %>% anti_join( ignore_games, by=c( "season", "session_id", "game_id4" ), copy=TRUE )
  }

  # Unique game id for counting
  gp_df <- gp_df %>% mutate(
        unique_gm_id = concat( season, session_id, game_id4 )
      )

  # Shot metrics
  if( group_team ) {
    ret_df <- gp_df %>% group_by( team_short, nhl_id, position_fd )
#     ret_df <- ret_df %>% mutate(
#       unique_gm_id = paste( season, session_id, game_id4, nhl_id, team_short ) # we might actually need team_id here...
#     )
  } else {
    ret_df <- gp_df %>% group_by( nhl_id, position_fd )
#     ret_df <- ret_df %>% mutate(
#       unique_gm_id = paste( season, session_id, game_id4, nhl_id )
#     )
  }
  # 1st or last n games only
  if( !is.null(subset_head_tail) ) {
    if( is.null(subset_count) )  stop( "Subset game count not specified.")
    if( date_range_flag )  stop( "game subsets for date ranges not supported.")

    # Take the 1st n games or last n games
    # team_score <- team_score %>% collect()
    game_id4_subset <- c()
    if( subset_head_tail == "head" ) {
      # ret_df <- ret_df %>% arrange( game_date )
      # last game_date to include
      team_date_cutoff <- team_score %>% filter( game_number == subset_count ) %>%
                            select( team_short, cutoff_date=game_date )

      # ret_df <- ret_df %>% collect() %>% left_join( team_date_cutoff, by ="team_short" )
      ret_df <- ret_df %>% left_join( team_date_cutoff, by ="team_short" )
      ret_df <- ret_df %>% filter( game_date <= cutoff_date )

   } else if( subset_head_tail == "tail" ) {
      # ret_df <- ret_df %>% arrange( desc(game_date) )
      max_game_number <- team_score %>% select( game_number ) %>% collect() %>% max()
      min_game_number <- max_game_number - subset_count + 1
      team_date_cutoff <- team_score %>% filter( game_number == min_game_number ) %>%
        select( team_short, cutoff_date=game_date )

      # ret_df <- ret_df %>% collect() %>% left_join( team_date_cutoff, by ="team_short" )
      ret_df <- ret_df %>% left_join( team_date_cutoff, by ="team_short" )
      ret_df <- ret_df %>% filter( game_date >= cutoff_date )
    }

    # I did not want to do the collect() here, but there seems to be an issue with
    # applying a filter() after the group_by()...
    # 6/29/15: I actually now think this is bc of the left_join, not the filter.
    # ret_df <- ret_df %>% collect() %>% filter( row_number() <= subset_count )
  }

  ret_df <- ret_df %>%
            summarise(
              ## individual stats
#               team_short = max( team_short ),
              # gm         = n(), # count of games played
              gm         = count( distinct( unique_gm_id ) ), # count of games played
              toi        = sum( toi        ),
              toi_off    = sum( toi_off    ),
              g          = sum( g          ),
              a1         = sum( a1         ),
              a2         = sum( a2         ),
              bk_i       = sum( bk_i       ),
              ab_i       = sum( ab_i       ),
              miss_i     = sum( miss_i     ),
              sh_saved_i = sum( sh_saved_i ),
              hit_i      = sum( hit_i      ),
              hit_by_i   = sum( hit_by_i   ),
              pen_i      = sum( pen_i      ),
              pen_draw_i = sum( pen_draw_i ),
              pim        = sum( pim        ),
              fo_w       = sum( fo_w       ),
              fo_l       = sum( fo_l       ),
              zs_o       = sum( zs_o       ),
              zs_n       = sum( zs_n       ),
              zs_d       = sum( zs_d       ),

              ## add team level stats
              gf         = sum( gf         ),
              ga         = sum( ga         ),
              sh_saved_f = sum( sh_saved_f ),
              sh_saved_a = sum( sh_saved_a ),
              miss_f     = sum( miss_f     ),
              miss_a     = sum( miss_a     ),
              bk         = sum( bk         ),
              ab         = sum( ab         ),
              hit        = sum( hit        ),
              hit_by     = sum( hit_by     ),
              pen        = sum( pen        ),
              pen_draw   = sum( pen_draw   ),

              ## off ice stats
              zs_o_off   = sum( zs_o_off   ),
              zs_d_off   = sum( zs_d_off   ),
              gf_off     = sum( gf_off     ),
              ga_off     = sum( ga_off     ),
              sf_off     = sum( sf_off     ),
              sa_off     = sum( sa_off     ),
              ff_off     = sum( ff_off     ),
              fa_off     = sum( fa_off     ),
              cf_off     = sum( cf_off     ),
              ca_off     = sum( ca_off     )

              ) %>% ungroup() %>% collect()

  # Replace NA with 0 for non-G and non-team.
  ret_df$position_fd[ ret_df$nhl_id < 100 ] <- "T"
  ret_df[ !ret_df$position_fd %in% c("G", "T") & is.na(ret_df) ] <- 0

  ret_df <- calc_shot_stats_basic( ret_df )
  ret_df <- calc_shot_stats_rel(   ret_df, corsi_adj_factor = corsi_adj_factor )
  ret_df <- ret_df %>% mutate(
                                toi_pct = round( toi /(toi + toi_off) * 100, 3),
                                toi_gm  = round( toi /gm, 3),
                                p_gm    = round( p /gm, 3)
                                )  %>% ungroup()

  # h2h data ----------------------------------------------------------------

  if( !nrow( h2h_df ) ) {
    ret_df <- ret_df %>% mutate(
      toi_pct_comp_d = NA,
      toi_pct_comp_f = NA,
      toi_pct_team_d = NA,
      toi_pct_team_f = NA,
      cf_pct_comp_d  = NA,
      cf_pct_comp_f  = NA,
      cf_pct_team_d  = NA,
      cf_pct_team_f  = NA,
      ff_pct_comp_d  = NA,
      ff_pct_comp_f  = NA,
      ff_pct_team_d  = NA,
      ff_pct_team_f  = NA,
      toi_pct_comp   = NA,
      toi_pct_team   = NA,
      cf_pct_comp    = NA,
      cf_pct_team    = NA,
      ff_pct_comp    = NA,
      ff_pct_team    = NA
    )
  } else {

    if( period == "1or2" ) {
      toi_df <- h2h_df %>% group_by( nhl_id_1, nhl_id_2, team_comp ) %>%
          summarize(
            toi_share_p1 = sum( toi_period_1 ), # na.rm not needed in SQL!
            toi_share_p2 = sum( toi_period_2 )
          ) %>% collect() %>% mutate(
            toi_share = sum( toi_share_p1, toi_share_p2, na.rm=T )
          )
    } else {
      # need to create period column name to extract from h2h table.
      period_col = paste0( "toi_period_", period )

      # Sum pairwise TOI
      toi_df <- eval( substitute(
        h2h_df %>% group_by( nhl_id_1, nhl_id_2, team_comp ) %>%
          summarize(
            toi_share = sum( period_col ) # na.rm not needed in SQL!
          ),
        list( period_col = as.name(period_col) )
      ) ) %>% collect()
    }

    toi_df <- toi_df %>% left_join( ret_df %>% select( nhl_id_1=nhl_id, toi_1=toi ), by="nhl_id_1" ) %>%
                         left_join( ret_df %>% select( nhl_id_2=nhl_id, position_fd_2=position_fd,
                                                       toi_2=toi,       toi_pct_2=toi_pct,
                                                       cf_pct_2=cf_pct, ff_pct_2=ff_pct
                                                       ), by="nhl_id_2" )

    toi_df <- toi_df %>% ungroup() %>% mutate(
                            toi_share_pct_1 = round( toi_share / toi_1 * 100, 3 )
                        ) %>% arrange( nhl_id_1 )

    ## Summarize pairwise TOI data
    toi_h2h_df <- toi_df %>% filter( nhl_id_1 != nhl_id_2 ) %>%
                          mutate( team_comp = ifelse( team_comp == "T", "team", "comp" )
                                        )
    player_team_comp_fd <- toi_h2h_df %>% group_by( nhl_id_1, team_comp, position_fd_2 ) %>%
      summarize(
        #toi_share_sum       = sum( toi_share ),
        toi_share_pct_1_sum = sum( toi_share_pct_1 ),
        toi_pct_group     = round( sum( toi_share_pct_1*toi_pct_2, na.rm=TRUE )/toi_share_pct_1_sum, 3),
        cf_pct_group      = round( sum( toi_share_pct_1*cf_pct_2,  na.rm=TRUE )/toi_share_pct_1_sum, 3),
        ff_pct_group      = round( sum( toi_share_pct_1*ff_pct_2,  na.rm=TRUE )/toi_share_pct_1_sum, 3)
      ) %>% ungroup()

    player_team_comp <- toi_h2h_df %>% group_by( nhl_id_1, team_comp ) %>%
      summarize(
        #toi_share_sum     = sum( toi_share ),
        toi_share_pct_1_sum = sum( toi_share_pct_1 ),
        toi_pct_group     = round( sum( toi_share_pct_1*toi_pct_2, na.rm=TRUE )/toi_share_pct_1_sum, 3),
        cf_pct_group      = round( sum( toi_share_pct_1*cf_pct_2,  na.rm=TRUE )/toi_share_pct_1_sum, 3),
        ff_pct_group      = round( sum( toi_share_pct_1*ff_pct_2,  na.rm=TRUE )/toi_share_pct_1_sum, 3)
      ) %>% ungroup()

    # team/comp and F/D
    toi_team_comp_fd <- spread_team_comp_results( player_team_comp_fd, "toi", fd_flag=T )
    cf_team_comp_fd  <- spread_team_comp_results( player_team_comp_fd, "cf",  fd_flag=T )
    ff_team_comp_fd  <- spread_team_comp_results( player_team_comp_fd, "ff",  fd_flag=T )

    # team/comp only, aggregating F and D
    toi_team_comp <- spread_team_comp_results( player_team_comp, "toi", fd_flag=F )
    cf_team_comp  <- spread_team_comp_results( player_team_comp, "cf",  fd_flag=F )
    ff_team_comp  <- spread_team_comp_results( player_team_comp, "ff",  fd_flag=F )


    # join all the new toi stats we created --------------------------------------

    ret_df <- ret_df %>%
      left_join( toi_team_comp_fd, by="nhl_id" ) %>%
      left_join( cf_team_comp_fd,  by="nhl_id" ) %>%
      left_join( ff_team_comp_fd,  by="nhl_id" ) %>%
      left_join( toi_team_comp,    by="nhl_id" ) %>%
      left_join( cf_team_comp,     by="nhl_id" ) %>%
      left_join( ff_team_comp,     by="nhl_id" )
  } # if nrow h2h df

if( !group_team ) ret_df <- ret_df %>% mutate( team_short = "NHL" )

  ret_df <- ret_df %>% select(
    team_short,
#     team_id,
    nhl_id,
#    last_name,
    position_fd,
    gm,
    toi,
    toi_gm,
    toi_pct,
    g,
    a,
    p,
    p_gm,
    # team shot stats
    gf,
    ga,
    g_net,
    gf_pct,
    gf_pct_off,
    gf_pct_rel,
    sf,
    sa,
    s_net,
    sf_pct,
    sf_pct_off,
    sf_pct_rel,
    ff,
    fa,
    f_net,
    ff_pct,
    ff_pct_off,
    ff_pct_rel,
    cf,
    ca,
    c_net,
    cf_pct,
    cf_pct_off,
    cf_pct_rel,
    # adj team shot stats
    ff_adj,
    fa_adj,
    ff_pct_adj,
    ff_pct_rel_adj,
    cf_adj,
    ca_adj,
    cf_pct_adj,
    cf_pct_rel_adj,
    # shooting pct
    sh_pct_i,
    sh_pct,
    f_sh_pct,
    c_sh_pct,
    # shooting pct
    sh_pct_a,
    f_sh_pct_a,
    c_sh_pct_a,
    # save pct
    sv_pct,
    f_sv_pct,
    c_sv_pct,
    # individual stats
    sf_i,
    ff_i,
    cf_i,
    a1,
    a2,
    hit_i,
    hit_by_i,
    pen_i,
    pen_draw_i,
    pim,
    zs_o,
    zs_n,
    zs_d,
    zs_o_pct,
    zs_o_pct_rel,
    fo_w,
    fo_l,
    fo_pct,
    # team pen
    pen,
    pen_draw,
    # QoT, QoC
    toi_pct_comp_d,
    toi_pct_comp_f,
    toi_pct_team_d,
    toi_pct_team_f,
    cf_pct_comp_d,
    cf_pct_comp_f,
    cf_pct_team_d,
    cf_pct_team_f,
    ff_pct_comp_d,
    ff_pct_comp_f,
    ff_pct_team_d,
    ff_pct_team_f,
    toi_pct_comp,
    toi_pct_team,
    cf_pct_comp,
    cf_pct_team,
    ff_pct_comp,
    ff_pct_team,
    # individual stats, less important
    bk_i,
    ab_i,
    miss_i,
    sh_saved_i,
    f_sh_pct_i,
    # team stats, less important
    sh_saved_f,
    sh_saved_a,
    miss_f,
    miss_a,
    bk,
    ab,
    hit,
    hit_by,
    # off ice stats
    toi_off,
    zs_o_off,
    zs_d_off,
    gf_off,
    ga_off,
    sf_off,
    sa_off,
    ff_off,
    fa_off,
    ff_off_adj,
    fa_off_adj,
    cf_off,
    ca_off,
    cf_off_adj,
    ca_off_adj,
    zs_o_pct_off,
    gf_pct_off,
    sf_pct_off,
    ff_pct_off,
    cf_pct_off,
    cf_pct_off_adj
    )


   # let's replace all Nan with NA for consistency with SQL
  ret_df <- as_data_frame( rapply( ret_df, f=function(x) ifelse(is.nan(x),NA,x), how="replace" ) )

  if( !group_team ) ret_df <- ret_df %>% select( -team_short )
  if( !h2h_stats  ) ret_df <- ret_df %>% select( -(toi_pct_comp_d:ff_pct_team) )

  if( team_only ) {
    ret_df <- ret_df %>% select( -(toi_pct:p_gm),
                                 -(gf_pct_off:gf_pct_rel),
                                 -(sf_pct_off:sf_pct_rel),
                                 -(ff_pct_off:ff_pct_rel),
                                 -(cf_pct_off:sh_pct_i),
                                 -(sf_i:pen_draw_i),
                                 -zs_o_pct_rel,
                                 -(bk_i:f_sh_pct_i) )
    ret_df <- ret_df %>% left_join( team_tbl %>% select(
                          name_short,
                          team_id
                          ),
                                    by = c( "nhl_id"="team_id" )
                                    )
  } else {
    ret_df <- ret_df %>% arrange( position_fd, desc(toi) )
  }


  ret_df
}





