create_shot_line_chart <- function( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df,
  ev5on5=FALSE,
  shotcolor="ALL", # GREEN RED BLUE WHITE
  include_blocked=F,
  this.game.dir=NULL ) {

  # tedious to type out these full var every time
  our_team   <- game_info$our_team
  their_team <- game_info$their_team
  this_shotcolor <- shotcolor

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

  chart_shots_df <- shots_df %>% mutate(
    ev5on5 = strength=="EV 5v5" # Callie's convention for ev5on5
  )

  # shot filters
  if( shotcolor == "GREENBLUE" ) {
    chart_shots_df <- chart_shots_df %>% filter( shotcolor %in% c("GREEN", "BLUE") )
  } else if( shotcolor != "ALL" ) {
    chart_shots_df <- chart_shots_df %>% filter( shotcolor==this_shotcolor )
  }
  shot_text <- paste( str_to_title(this_shotcolor), "Shots" )

  if( !include_blocked ) {
    chart_shots_df <- chart_shots_df %>% filter( shot != "BLOCK" )
    shot_text <- paste( shot_text, "(Unblocked)" )
  }
  if( ev5on5 ) {
    chart_shots_df <- chart_shots_df %>% filter( ev5on5 )
    shot_text <- paste( shot_text, " - Ev5on5 only" )
  } else {
    shot_text <- paste( shot_text, " - All strength" )
  }
  chart_shots_df <- chart_shots_df %>% select( start_cum, event_team, shooter, strength, ev5on5 )

  gtitle <- sprintf( "%s %s@%s: %s",
    format( as.Date(game_info$game_date), "%a %m.%d.%y"),
    game_info$away_team_short, game_info$home_team_short,
    shot_text )
  message( gtitle )


  # Construct cumsum for each team.  There's probably an easier way.
  our_shots <- chart_shots_df %>% filter( event_team==our_team ) %>%
    mutate(
      count = cumsum( !is.na(event_team) )
    )
  their_shots <- chart_shots_df %>% filter( event_team==their_team ) %>%
    mutate(
      count = cumsum( !is.na(event_team) )
    )
  chart_shots_all <- bind_rows( our_shots, their_shots ) %>% select( -strength )

  # last val for each team
  our_shot_total    <- sum( chart_shots_all$event_team==our_team )
  their_shot_total  <- sum( chart_shots_all$event_team==their_team )

  # gather is the tidyr vesion of melt().  new_key_name, new_value_name, - all colnames that stay
  # green_shots_m <- green_shots_all  %>% gather( team, count, -(1:4) )

  # append on dummy start and end values
  chart_shots_m <- bind_rows(
    chart_shots_all,
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

  y.axis.min <- min( chart_shots_m$count )
  y.axis.max <- max( chart_shots_m$count )

  their_color <- "grey40"
  line_colors <- setNames( c( "red", their_color ), c( our_team, their_team ))
  rect_colors <- setNames( c( "red", "cornflowerblue"),
    c( our_team, their_team ))


  p.shots <- ggplot( chart_shots_m ) # + geom_hline( aes(yintercept=0) )

  p.shots <- p.shots + geom_step( aes(x=start_cum, y=count, color=event_team),size=1.75 )

  # Extract y axis max for G labeling
  y.axis.min <- ggplot_build(p.shots)$panel$ranges[[1]]$y.range[1]
  y.axis.max <- ggplot_build(p.shots)$panel$ranges[[1]]$y.range[2]

  if( nrow( mandown_intervals_df ) ) {
    mandown_intervals_df <- mandown_intervals_df %>% mutate(
      ymin = -Inf, #  y.axis.min
      ymax =  Inf #  y.axis.max
    )

    p.shots <- p.shots +
      geom_rect( data=mandown_intervals_df, aes( xmin=start_cum, xmax=end_cum,
        ymin=ymin, ymax=ymax, fill=down_team ), alpha=0.15, color="grey30", size=.15 )
    if( nrow( mandown_intervals_df %>% filter( num_players < 5 ) )>1 ) {
      p.shots <- p.shots + geom_rect( data=mandown_intervals_df %>% filter( num_players < 5 ),
        aes( xmin=start_cum, xmax=end_cum, ymin=ymin, ymax=ymax, fill=down_team ), alpha=0.25, color="grey30", size=.15 )
    }
    p.shots <- p.shots + scale_fill_manual( name="Penalty", breaks=c(their_team, our_team), values=rect_colors )
  }

  y_shot_max <- 1.2*y.axis.max
  p.shots <- p.shots +
    geom_point( data=chart_shots_m %>% filter( !ev5on5 ),
      aes(x=start_cum, y=count,shape=ev5on5), color="darkorange", size=geom_point.size ) +
    scale_colour_manual(name = "Team", values=line_colors ) +
    scale_shape_manual( name="", values=c(17,18), labels=c("Non-5on5 shot attempt", "even") )

  if( nrow( goals_df) > 0 ) {
    p.shots <- p.shots + geom_vline( data=goals_df, aes(xintercept=start_cum, color=event_team), linetype="solid", size=0.6 )

    goals_df$y.goal_text <- 1.1*y.axis.max
    p.shots <- p.shots + geom_text( data=goals_df, aes(label=goal_text, x=start_cum, y=y.goal_text, color=event_team),
      angle=90, show.legend=F, hjust= 0.8, size=geom_text.size, vjust=-0.4 )
  }

  # cosmetic chart stuff here
  p.shots <- p.shots +
    # expand_limits(x = 0, y = 0) +
    scale_x_continuous( breaks = seq( 0, 200, by=5 ), expand=c(0,0) ) +
    scale_y_continuous( breaks = seq( -300, 300, by=2 ), expand=c(0,0),limits=c(0,y_shot_max) ) +
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
