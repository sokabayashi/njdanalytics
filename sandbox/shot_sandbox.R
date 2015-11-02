

create_shot_quality_line_chart <- function( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=FALSE, this.game.dir=NULL ) {

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

  green_shots <- shots_df %>% filter( shotcolor=="GREEN", shot != "BLOCK" ) %>%
                          select( start_cum, event_team, shooter, strength )
  green_shots <- green_shots %>% mutate(
    ev5on5 = strength=="EV 5on5"
  )

  if( ev5on5 ) {
    green_shots <- green_shots %>% filter( ev5on5 )
  }

  # Construct cumsum for each team.  There's probably an easier way.
  our_green_shots <- green_shots %>% filter( event_team==our_team ) %>%
                            mutate(
                              count = cumsum( !is.na(event_team) )
                            )
  their_green_shots <- green_shots %>% filter( event_team==their_team ) %>%
                            mutate(
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
    labs( title=gtitle ) + xlab( "Game Time (Minutes)" ) + ylab( "Shot Attempts") +
    theme_bw() + theme( text = element_text(size=chart_text.size),
      legend.position="bottom", legend.box = "horizontal" )



  # output to file
  if( save.to.file ) {
    file.name <- paste0( this.game.dir, "/", away.team,"-", home.team, "-shot-attempts-line-chart.png" )
    file.name
    ggsave( filename = file.name, width=7.5, height=5. )
  }
  p.shots
}




# Chart a game ------------------------------------------------------------

nhl_db <- setup_nhl_db()

this_season     <- "20152016"
this_session_id <- "2"

# this_game_id4   <- "0037"
# game_file <- paste0( nhl_dir$shotdata, "/20151013 Gm3 NSH@NJD.xlsx" )

master_table_file <- paste0( nhl_dir$shotdata, "/g1-g10.xlsx" )
master_table <- read_excel( master_table_file, sheet = 1 )
master_table <- master_table %>% rename(
  period     = Period,
  clock      = Clock,
  strength   = Strength,
  event_team = Team,
  shooter    = Shooter,
  shotcolor  = Color,
  shot       = Shot,
  attribute  = Type,
  comment    = Comment,
  game_num   = `G#`
  )

# windows origin date is "1899-12-30"
# origin_time <- ymd_hms( "1899-12-30 20:00:00" )
# origin_time_regular_ot <- ymd_hms( "1899-12-30 05:00:00" )

origin_time <- ms( "20:00" )
origin_time_regular_ot <- ms( "5:00" )

shots_tbl <- master_table %>% mutate(
  period           = as.numeric( period ),
  game_num         = as.numeric( game_num ),
  clock_ms         = ms( clock ),
  period_sec_cum   = ifelse( this_session_id == "3" | period <= 3,
                                      period_to_seconds( ms( "20:00" ) - clock_ms ),
                                      period_to_seconds( ms(  "5:00" ) - clock_ms ) ),
  period_min_cum   = round( period_sec_cum/60, 3 ),
  period_clock_cum = time_decimal_to_mmss( period_min_cum ),
  start_cum        = (period-1)*20 + period_min_cum
) %>% select( -clock_ms, -period_sec_cum, -period_min_cum )

# goals
# mandown_times
# game_info

stage_game           <- tbl( nhl_db, "stage_game"           )
stage_playbyplay     <- tbl( nhl_db, "stage_playbyplay"     )
stage_shift_interval <- tbl( nhl_db, "stage_shift_interval" )
team_score           <- tbl( nhl_db, "team_score"           )


njd_games         <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short=="NJD" ) %>% collect() %>% arrange( game_date )

stage_game           <- stage_game           %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect()
stage_playbyplay     <- stage_playbyplay     %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect() %>% arrange( start_cum )
stage_shift_interval <- stage_shift_interval %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect() %>% arrange( start_cum )

# note: we've tagged fewer games than actually played
shots_tbl <- shots_tbl %>% left_join( njd_games, by=c( "game_num"="game_number") )

# aggregate stats ---------------------------------------------------------

# unblocked Green shots
fenwick_df <- shots_df %>% filter(shot!="BLOCK" )
fenwick_df %>% group_by( game_num, event_team ) %>% summarize( green=n() )

# single game -------------------------------------------------------------

this_game_num <- 8
this_game_id4 <- shots_tbl %>% filter( game_num==this_game_num ) %>% head(1) %>% select( game_id4 ) %>% unlist(use.names = F)

# get the base data for this game
this_game_score   <- njd_games %>%            filter( game_number==this_game_num )
shots_df          <- shots_tbl %>%            filter( game_id4==this_game_id4 )
game_info         <- stage_game %>%           filter( game_id4==this_game_id4 )
pbp_df            <- stage_playbyplay %>%     filter( game_id4==this_game_id4 )
shift_interval_df <- stage_shift_interval %>% filter( game_id4==this_game_id4 )

# meta data for chart
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
  # an NJD game
  if( our_team == game_info$home_team_short ) {
    their_team <- game_info$away_team_short
  } else {
    their_team <- game_info$home_team_short
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

create_shot_quality_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=FALSE, this.game.dir=NULL )






