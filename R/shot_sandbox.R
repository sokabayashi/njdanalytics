library( njdanalytics )

# get_num_player_runs_ha -------------------------------------------------------
# Find all time intervals with different number of players on ice

get_num_player_intervals_ha <- function( shift_interval_df, ha = "H" ) {
  if( ha == "H" ) {
    num_players_rle <- rle( shift_interval_df$num_players_h )
  } else {
    num_players_rle <- rle( shift_interval_df$num_players_a )
  }

  ## rle returns a list
  #]> values:
  #[1] 6 5 6 5 6 5 6 5 4 5 6
  #> lengths: run length, how many consecuive elements take that value
  #[1] 51  6 13  8 44  6 91  4  1  4 14
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
# Example: team gets two consecutive penalties, separarated by 30 sec.
# assume opponent does not score.
# we will get 3 rows: a 30 sec 5on4, a 1:30 5on3, a 30 sec 5on4.

get_mandown_intervals <- function( shift, game_info ) {

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


create_shot_quality_line_chart <- function( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, this.game.dir=NULL ) {

  # used a lot
  our_team   <- game_info$our_team
  their_team <- game_info$their_team

  gtitle <- sprintf( "%s %s@%s: Quality Shot Attempts",
    format( as.Date(game_info$game_date), "%a %m.%d.%y"),
    game_info$away_team_short, game_info$home_team_short )
  message( gtitle )

  if( nrow( goals_df ) > 0 ) {
    goals_df$y.goal_text <- 0
    goals_df$goal_text <- with( goals_df,
      sprintf( "%s  %s - %s  %s",
        game_info$away_team_short,
        away_score + as.numeric(event_team==game_info$away_team_short),
        home_score + as.numeric(event_team==game_info$home_team_short),
        game_info$home_team_short
      ))
  }

  green_shots <- shots_df %>% filter( color=="G", type != "Block" ) %>%
                          select( start_cum, event_team, number, strength )
  green_shots <- green_shots %>% mutate(
    ev5on5 = strength=="Ev5on5"
  )

  green_shots_ev5on5 <- green_shots %>% filter( ev5on5 )
  green_shots_ev5on5 <- green_shots_ev5on5 %>% mutate(
                            cum_our_team   = cumsum( event_team==our_team),
                            cum_their_team = cumsum( event_team==their_team)
  )

  our_green_shots <- green_shots %>% filter( event_team==our_team )
  our_green_shots <- our_green_shots %>% mutate(
    count = cumsum( !is.na(event_team) )
  )
  their_green_shots <- green_shots %>% filter( event_team==their_team )
  their_green_shots <- their_green_shots %>% mutate(
    count = cumsum( !is.na(event_team) )
  )

  green_shots_all <- bind_rows( our_green_shots, their_green_shots ) %>% select( -strength )

  # last val for each team
  our_shot_total    <- sum( green_shots_all$event_team==our_team )
  their_shot_total  <- sum( green_shots_all$event_team==their_team )

  # gather is the tidyr vesion of melt().  new_key_name, new_value_name, - all colnames that stay
  # green_shots_m <- green_shots_all  %>% gather( team, count, -(1:4) )

  # append on dummy start and end values
  green_shots_m <- bind_rows(
                  green_shots_all,
                  data_frame( start_cum=0, event_team=our_team,   ev5on5=TRUE, count=0),
                  data_frame( start_cum=0, event_team=their_team, ev5on5=TRUE, count=0),
                  data_frame( start_cum=game_info$toi_total,
                              event_team=our_team,   ev5on5=TRUE, count=our_shot_total ),
                  data_frame( start_cum=game_info$toi_total,
                              event_team=their_team, ev5on5=TRUE, count=their_shot_total)
    )

  save.to.file <- FALSE
  if( save.to.file ) {
    chart_text.size <- 7
    geom_text.size  <- 1.8
    geom_point.size   <- 1.5
  } else {
    chart_text.size <- 12
    geom_text.size  <- 2.8
    geom_point.size <- 2.7
  }

  y.axis.min <- min( green_shots_m$count )
  y.axis.max <- max( green_shots_m$count )

  line_colors <- setNames( c( "red", "black"), c( our_team, their_team ))
  rect_colors <- setNames( c( "red", "cornflowerblue"),
    c( our_team, their_team ))


  p.shots <- ggplot( green_shots_m ) + geom_hline( aes(yintercept=0) )

  p.shots <- p.shots + geom_step( aes(x=start_cum, y=count, color=event_team) )

  # Extract y axis max for G labeling
  y.axis.min <- ggplot_build(p.shots)$panel$ranges[[1]]$y.range[1]
  y.axis.max <- ggplot_build(p.shots)$panel$ranges[[1]]$y.range[2]

  if( nrow( mandown_intervals_df ) ) {
    mandown_intervals_df$ymin <- -Inf #  y.axis.min
    mandown_intervals_df$ymax <-  Inf #  y.axis.max
    p.shots <- p.shots +
      geom_rect( data=mandown_intervals_df, aes( xmin=start_cum, xmax=end_cum,
                                          ymin=ymin, ymax=ymax, fill=down_team ), color="grey30", size=.1, alpha=0.30 ) +
      scale_fill_manual( name="Penalty", breaks=c(their_team, our_team), values=rect_colors )
  }

  p.shots <- p.shots +
    geom_point( data=green_shots_m %>% filter( !ev5on5 ),
      aes(x=start_cum, y=count,shape=ev5on5), color="darkorange", size=geom_point.size ) +
    scale_colour_manual(name = "Team",
      values=line_colors ) +
    scale_shape_manual( name="", values=c(17,18), labels=c("Non-5on5 shot attempt", "even") )

  if( nrow( goals_df) > 0 ) {
    p.shots <- p.shots + geom_vline( data=goals_df, aes(xintercept=start_cum, color=event_team), size=0.35 )

    goals_df$y.goal_text <- y.axis.max
    p.shots <- p.shots + geom_text( data=goals_df, aes(label=goal_text, x=start_cum, y=y.goal_text, color=event_team),
      angle=90, show_guide=F, hjust= 0.8, size=geom_text.size, vjust=-0.4 )
  }

  # cosmetic chart stuff here
  p.shots <- p.shots + scale_x_continuous( breaks = seq( 0, 200, by=5 ) ) +
    scale_y_continuous( breaks = seq( -300, 300, by=2 ) ) +
    labs( title=gtitle ) + xlab( "Game Time (Minutes)" ) + ylab( "Net Shot Attempts") +
    theme_bw() + theme( text = element_text(size=chart_text.size),
      legend.position="bottom", legend.box = "horizontal" )

  p.shots

  # output to file
  if( save.to.file ) {
    file.name <- paste0( this.game.dir, "/", away.team,"-", home.team, "-shot-attempts-line-chart.png" )
    file.name
    ggsave( filename = file.name, width=7.5, height=5. )
  }
}



this_season     <- "20152016"
this_session_id <- "2"
this_game_id4   <- "0037"

game_file <- paste0( nhl_dir$shotdata, "/20151013 Gm3 NSH@NJD.xlsx" )
gamedata <- read_excel( game_file, sheet = 1 )


# windows origin date is "1899-12-30"
origin_time <- ymd_hms( "1899-12-30 20:00:00" )
origin_time_regular_ot <- ymd_hms( "1899-12-30 05:00:00" )

names( gamedata ) <- c( "period", "clock", "strength", "event_team", "number", "color", "event", "type", "comment" )

shots_df <- gamedata %>% mutate(
  period_sec_cum   = ifelse( this_session_id == "3" | period <= 3, as.numeric( origin_time - clock ),
                                                              as.numeric( origin_time_regular_ot - clock ) ),
  period_min_cum   = round( period_sec_cum/60, 3 ),
  period_clock_cum = time_decimal_to_mmss( period_min_cum ),
  start_cum        = (period-1)*20 + period_min_cum
)

# goals
# mandown_times
# game_info

nhl_db <- setup_nhl_db()
stage_game           <- tbl( nhl_db, "stage_game"           )
stage_playbyplay     <- tbl( nhl_db, "stage_playbyplay"     )
stage_shift_interval <- tbl( nhl_db, "stage_shift_interval" )

game_info         <- stage_game           %>% filter( season==this_season, session_id==this_session_id, game_id4==this_game_id4 ) %>% collect()
pbp_df            <- stage_playbyplay     %>% filter( season==this_season, session_id==this_session_id, game_id4==this_game_id4 ) %>% collect() %>% arrange( start_cum )
shift_interval_df <- stage_shift_interval %>% filter( season==this_season, session_id==this_session_id, game_id4==this_game_id4 ) %>%
                                              collect() %>% arrange( start_cum )

# Goals and mandown -----------------------------------------------------
mandown_intervals_df <- get_mandown_intervals( shift_interval_df, game_info )

goals_df <- pbp_df %>% filter( event_type == "GOAL" )
winning_team_short <- with( game_info, ifelse( home_score_final > away_score_final, home_team_short, away_team_short ) )

## Shootout: Filter out all SO goals except last one
if( (game_info$session_id == "1" || game_info$session_id == "2") && max(pbp_df$period) == 5 ) {

  # SO scores keep incrementing and the way the data is written to db, we don't know SO winner (oh no)
  ot_score  <- pbp_df %>% filter( event_type == "PEND", period == 4 ) # grab pre-SO score
  so_goals <- goals_df %>% filter( period == 5 )
  # arbitrarily grab first goal by winning team
  so_winner <- so_goals %>% filter( event_team == winning_team_short ) %>% head(1)

  so_winner$away_score <- ot_score$away_score
  so_winner$home_score <- ot_score$home_score

  goals_df <- goals_df %>% filter( period < 5 )
  goals_df <- bind_rows( goals_df, so_winner )
}






# NJD perspective
our_team <- "NJD"
row_ha   <- "H"; col_ha   <- "A"
if( our_team %in% c( game_info$home_team_short, game_info$away_team_short ) ) {
  if( our_team == game_info$home_team_short ) {
    # njd_away_game <- FALSE
    their_team <- game_info$away_team_short
  } else {
    their_team <- game_info$home_team_short
    # njd_away_game <- TRUE
    row_ha   <- "A"
    col_ha   <- "H"
  }
} else {
  # non-NJD game, set default perspective to Home team
  our_team   <- game_info$home_team_short
  their_team <- game_info$away_team_short
}
game_info <- game_info %>% mutate(
  our_team   = our_team,
  their_team = their_team
)

create_shot_quality_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, this.game.dir=NULL )






