library( njdanalytics )

# get_mandown_times_ha -------------------------------------------------------
# dirty work for determining man adv start and end times based on number of skaters on ice
# will also capture 3on3 and 4on4 due to penalties (but not OT)

get_mandown_times_ha <- function( shared_toi, ha = "H" ) {
  if( ha == "H" ) {
    num_players_rle <- rle( shared_toi$num_players_h )
  } else {
    num_players_rle <- rle( shared_toi$num_players_a )
  }

  ## returns a list
  #]> num_players_rle$values
  #[1] 6 5 6 5 6 5 6 5 4 5 6
  #> num_players_rle$lengths
  #[1] 51  6 13  8 44  6 91  4  1  4 14
  num_units <- length( num_players_rle$values )
  num_players_rle$start_index <- c( 1, ( cumsum( num_players_rle$lengths )+1 )[ -num_units ] )
  num_players_rle$end_index   <- c(      cumsum( num_players_rle$lengths ))

  mandown_times <- data_frame( num_players = num_players_rle$values,
    start_cum   = shared_toi$start_cum[ num_players_rle$start_index ],
    end_cum     = shared_toi$start_cum[ num_players_rle$end_index ] +
                    shared_toi$duration[ num_players_rle$end_index ],
    end_period  = shared_toi$period[    num_players_rle$end_index ]
  )

  return( mandown_times )
}

# get_mandown_times -------------------------------------------------------
# determine man adv start and end times based on number of skaters on ice.
# Note: this is *not* one-to-one with the actual penalties; rather, it just
# shows the *impact* of penalties on strength.  also captures 3on3 & 4on4 due
# to penalties (but not OT).
# Example: team gets two consecutive penalties, separarated by 30 sec.
# assume opponent does not score.
# we will get 3 rows: a 30 sec 5on4, a 1:30 5on3, a 30 sec 5on4.

get_mandown_times <- function( shared_toi, game_info ) {

  mandown_times_h <- get_mandown_times_ha( shared_toi, "H" )
  mandown_times_a <- get_mandown_times_ha( shared_toi, "A" )

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


create_shot_quality_time_chart <- function( shot_attempts, goals, mandown_times, game_info, shared_toi, this.game.dir=NULL ) {

  gtitle <- sprintf( "%s %s@%s: Quality Shot Attempts (%s perspective)",
    format( as.Date(game_info$game_date), "%a %m.%d.%y"),
    game_info$away_team_short, game_info$home_team_short, game_info$our_team )
  message( gtitle )

  if( nrow( goals ) > 0 ) {
    goals$y.goal_text <- 0
    goals$goal_text <- with( goals, sprintf( "%s  %s - %s  %s", away.team, # Away
      away.score + as.numeric(ev.team==awayteam),
      home.score + as.numeric(ev.team==hometeam),
      home.team # Home
    )
    )
  }

  ## EV5on5 Corsi against
  #   subset( shot_attempts, select="corsi_ev5on5") %>% filter( corsi_ev5on5==-1) %>% sum()
  ## EV5on5 Corsi for
  #   subset( shot_attempts, select="corsi_ev5on5") %>% filter( corsi_ev5on5==1) %>% sum()

  shot_attempts_subset <- shot_attempts %>% select( period, seconds, ev.team, ev5on5,
    cum_corsi, cum_corsi_ev5on5 )
  shot_attempts_subset <- rbind(
    data_frame( period=0, seconds=0, ev.team=our.team, ev5on5=TRUE, cum_corsi=0, cum_corsi_ev5on5=0 ),
    shot_attempts_subset
  )
  shots_melted <- melt( shot_attempts_subset, id=1:4, variable.name="strength", value.name="corsi" )

  save.to.file <- TRUE
  if( save.to.file ) {
    chart_text.size <- 7
    geom_text.size  <- 1.8
    geom_point.size   <- 1.5
  } else {
    chart_text.size <- 12
    geom_text.size  <- 2.8
    geom_point.size <- 2.7
  }

  y.axis.min <- min(shots_melted$corsi)
  y.axis.max <- max(shots_melted$corsi)

  line_colors <- setNames( c( "darkorange", "navy", "red", "black"),
    c( "cum_corsi", "cum_corsi_ev5on5", our.team, their.team ))
  rect_colors <- setNames( c( "red", "cornflowerblue"),
    c( our.team, their.team ))


  p.shots <- ggplot( shots_melted ) + geom_hline( aes(yintercept=0) )

  p.shots <- p.shots + geom_step( aes(x=seconds/60, y=corsi, color=strength), size=0.0001 )

  # Extract y axis max for G labeling
  y.axis.min <- ggplot_build(p.shots)$panel$ranges[[1]]$y.range[1]
  y.axis.max <- ggplot_build(p.shots)$panel$ranges[[1]]$y.range[2]

  if( nrow( mandown_times ) ) {
    mandown_times$ymin <- -Inf #  y.axis.min
    mandown_times$ymax <-  Inf #  y.axis.max
    p.shots <- p.shots + geom_rect( data=mandown_times, aes( xmin=start_cum, xmax=end_cum,
      ymin=ymin, ymax=ymax, fill=ev.team ), color="grey30", size=.1, alpha=0.30 ) +
      scale_fill_manual( name="Penalty", breaks=c(their.team, our.team), values=rect_colors )
  }

  p.shots <- p.shots + geom_step( aes(x=seconds/60, y=corsi, color=strength), size=0.5 ) +
    geom_point( data=subset(shots_melted, ev5on5==FALSE & strength=="cum_corsi"),
      aes(x=seconds/60, y=corsi,shape=ev5on5), color="red", size=geom_point.size ) +
    scale_colour_manual(name = "Team Strength",
      values=line_colors,
      breaks=c( "cum_corsi", "cum_corsi_ev5on5"),
      labels=c( "All Strength", "Even 5on5") ) +
    scale_shape_manual( name="", values=c(17,18), labels=c("Non-5on5 shot attempt", "even") )

  if( nrow( goals) > 0 ) {
    p.shots <- p.shots + geom_vline( data=goals, aes(xintercept=seconds/60, color=ev.team), size=0.35 )

    goals$y.goal_text <- y.axis.max
    p.shots <- p.shots + geom_text( data=goals, aes(label=goal_text, x=seconds/60, y=y.goal_text, color=ev.team),
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
this_game_id4   <- "0066"

game_file <- paste0( nhl_dir$shotdata, "/20151013 Gm3 NSH@NJD.xlsx" )
gamedata <- read_excel( game_file, sheet = 1 )


# windows origin date is "1899-12-30"
origin_time <- ymd_hms( "1899-12-30 20:00:00" )
origin_time_regular_ot <- ymd_hms( "1899-12-30 05:00:00" )

names( gamedata ) <- c( "period", "clock", "strength", "team", "number", "color", "event", "type", "comment" )

gamedata <- gamedata %>% mutate(
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
pbp_df            <- stage_playbyplay     %>% filter( season==this_season, session_id==this_session_id, game_id4==this_game_id4 ) %>% collect()
shift_interval_df <- stage_shift_interval %>% filter( season==this_season, session_id==this_session_id, game_id4==this_game_id4 ) %>%
                                              collect() %>% arrange( start_cum )

# Goals and mandown -----------------------------------------------------
goals_df <- pbp_df %>% filter( event_type == "GOAL" )
mandown_rect_times <- get_mandown_times( shift_interval_df, game_info )






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








