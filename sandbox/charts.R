#' Create player heatmap.
#'
#' @param game_info from stage_game
#' @param my_matrix
#' @param row_ha "H" or "A" of row team
#' @param col_ha "H" or "A" of row team
#' @param our_team_short
#' @param matrix_type "TOI" or "Corsi"
#' @param strength Filename suffix
#' @param filter_period "all" by default
#' @param row_num_D 6 by default.  But sometimes it is 7
#' @param col_num_D 5 by default.  But sometimes it is 7
#' @param minutes_played Number of minutes played for chart title
#' @param fig_dir Directory where to save file
#'
#' @return
#' @export
#'
#' @examples
create_player_heatmap <- function(
  game_info, my_matrix,
  row_ha="H", col_ha="H",
  our_team_short="NJD",
  matrix_type="TOI",
  strength="ev5on5",
  filter_period="all",
  row_num_D=6, col_num_D=6,
  minutes_played=60, fig_dir ) {

  file.suffix     <- "toi"
  title.type      <- "TOI"
  sprintf.display <- "%.1f"
  tile.color.low  <- "white"
  tile.color.high <- "steelblue"
  text.size       <- 1.9 # to populate matrix
  text.x.adj      <- 0.39
  if( row_ha != col_ha ) {
    # tile.color.high <- "coral4"
    # tile.color.high <- muted( "blue" )
  }
  if( matrix_type == "Corsi" ) {
    file.suffix     <- "corsi"
    title.type      <- "Net Shot Attempts"
    sprintf.display <- "%s"
    tile.color.low  <- muted( "red" )
    tile.color.high <- "steelblue"
    text.x.adj      <- 0.27 # larger value pushes text further right
  }

  row_team <- col_team <- title_team <- ""
  ## for single team matrix, add team name to title but no row and col labels
  if( col_ha == row_ha ) {
    title_team <- game_info$home_team
    if( col_ha == "A" ) {
      title_team <- game_info$away_team
    }
  } else {
    if( row_ha == "H" ) {
      row_team <- game_info$home_team
    } else {
      row_team <- game_info$away_team
    }

    if( col_ha == "H" ) {
      col_team <- game_info$home_team
    } else {
      col_team <- game_info$away_team
    }
  }

  # 4 F lines, 3 pairs D - usually
  x.line.separators <- c(seq( 0, 12, 3 ), seq( 14, 18, 2 )) + 0.5 ## F then D
  y.line.separators <- c(seq( 0,  6, 2 ), seq(  9, 18, 3 )) + 0.5 ## D then F
  x.FD.separators <- c( 0, 12, 18 ) + 0.5
  y.FD.separators <- c( 0,  6, 18 ) + 0.5
  # sometimes 7 D though
  if( row_num_D == 7 ) {
    y.line.separators <- c(seq( 1,  7, 2 ), 9, seq(  12, 18, 3 )) + 0.5 ## D then F
    y.FD.separators <- c( 0,  7, 18 ) + 0.5
  }
  if( col_num_D == 7 ) {
    x.line.separators <- c(seq( 0, 9, 3 ), 11, seq( 13, 17, 2 )) + 0.5 ## F then D
    x.FD.separators <- c( 0, 11, 18 ) + 0.5
  }

  color.separator <- "red2"
  base_size <- 5.3

  nrow.toi <- nrow( my_matrix )
  # melt matrix into df for ggplot.  Note row order reversal for plotting purposes
  my.melt <- melt( my_matrix[ nrow.toi:1, ], varnames=c( "p1", "p2" ), value.name="value", colour="white" )

  if( matrix_type=="TOI") {
    # don't even display TOI < 1.0
    my.melt$value <- ifelse( my.melt$value < 1, 0, abs(my.melt$value) )
    my.melt$value.display  <- sprintf( sprintf.display, round(my.melt$value,1) )

    # above a threshold TOI, color should be same.
    if( row_ha == col_ha ) {
      toi_high_cutoff <- 11
    } else {
      toi_high_cutoff <- 7 # h2h2
    }
    my.melt$value <- pmin( my.melt$value, toi_high_cutoff )

    # below 3 min, it's 0 for fill fill purposes
    toi_low_cutoff  <- 3
    my.melt$value <- ifelse( abs(my.melt$value)<= toi_low_cutoff, 0, my.melt$value )
  } else {
    # Corsi chart
    my.melt$value.display  <- sprintf( sprintf.display, round(my.melt$value,1) )
    # within a threshold range [-3,3], it's essentially 0 for fill purposes
    corsi_cutoff  <- 3
    my.melt$value <- ifelse( abs(my.melt$value)<= corsi_cutoff, 0, my.melt$value )
  }

  # don't print 0s
  my.melt$value.display  <- ifelse( my.melt$value.display == "0" | my.melt$value.display == "0.0",
    "", my.melt$value.display )


  my.melt$value.display.x <- as.numeric(my.melt$p2) + text.x.adj # fudge factor to get number centered

  # make disparity in fill colors wider
  value.fill_max <- max( my.melt$value^2 )
  my.melt <- my.melt %>% mutate(
    value_sign = sign(value),
    value.fill = value_sign*(value^2) ,
    # black on dark colors is hard to read.  make high values (>70% of max value) white.
    text_color = ifelse( value^2 < 0.7*value.fill_max, "black", "white" )
  )

  # main heat map
  p.mat <- ggplot( my.melt, aes(x=p2, y=p1) ) + geom_tile( aes(fill=value.fill), color="gray95", size=0.2 ) +
    scale_fill_gradient2( low=tile.color.low, high=tile.color.high, guide=FALSE, space="Lab" ) +
    geom_text( aes(x=value.display.x, label= value.display, colour=text_color), size=text.size, hjust=1 ) +
    scale_colour_manual(values=c( "black", "white"), guide=FALSE)

  # F/D separators
  p.mat <- p.mat + geom_vline( xintercept=x.line.separators, size=0.25,  color=color.separator ) +
    geom_hline( yintercept=y.line.separators, size=0.25, color=color.separator ) +
    geom_vline( xintercept=x.FD.separators,   size=0.25, color="black"         ) +
    geom_hline( yintercept=y.FD.separators,   size=0.25, color="black" )

  # axis djustments
  p.mat <- p.mat + theme_bw(base_size = base_size) + labs(x = col_team, y = row_team) +
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) + coord_fixed(ratio=1) +
    #             labs( title = gtitle ) +   ## title screws up all the spacing...
    theme(
      # plot.background = element_rect(colour = "red"),
      axis.ticks = element_blank(),
      line = element_line( size=0.2),
      # axis.ticks.margin = unit(0, "cm"),  deprrecated
      axis.ticks.length = unit(0.1, "cm"),
      legend.position = "none",
      plot.margin = rep(unit(0,"lines"),4),panel.margin = unit(0,"null"),
      panel.grid.minor = element_line( colour="white" ),
      axis.title.y = element_text(size = rel(1.2) ),
      axis.title.x = element_text(size = rel(1.2) ),
      axis.text.y = element_text( size = base_size ),
      axis.text.x = element_text( size = base_size,  angle = 90, hjust = 0, vjust = 0.5 )
    )

  ## Move x-axis to top
  # extract gtable. start with 8 grobs
  g <- ggplot_gtable(ggplot_build(p.mat))

  # horizontal axis rearrangement - reverse title and axis labels
  ia <- which(g$layout$name == "axis-b") #5
  ax <- g$grobs[[ia]]$children[[2]]
  ax$heights <- rev(ax$heights)
  ax$grobs   <- rev(ax$grobs)
  #ax$grobs[[2]]$y <- ax$grobs[[2]]$y - unit(1, "npc") + unit(.25, "cm")
  ax$grobs[[2]]$y <-  unit(1, "npc") - unit(1, "cm")

  g$heights[[2]] <- g$heights[g$layout[ia,]$t]
  # grob: top, left, bottom, right
  # add axis labels grob.  now 9 grobs
  g <- gtable_add_grob(g, ax, 2, 4, 2, 4)

  ## add new row for upper axis label
  g <- gtable_add_rows(g, unit(10,"lines"), 0) ## Sai's addition for extra spacing between axis label and axis title
  g <- gtable_add_rows(g, g$heights[1], 1)
  g <- gtable_add_grob(g, g$grobs[[6]], 2, 4, 2, 4) ## add axis label to top


  # drop old axis
  g$grobs[[ia]]$children[[2]] <- NULL # new 12/21/15
  #   ia <- which(g$layout$name == "xlab")
  #   g$grobs[[ia]]$label = ''
  #
  # g$grobs[[ia]]$children[2]$axis$grobs[[2]]$label <- ""
  # g$grobs[[6]]$children[[2]] <- NULL # new 12/21/15
  # g$grobs[[6]]$label <- ""
  g$grobs[[6]] <- nullGrob() # new 21/21/15

  #   # reduce some margin
  ia_out <- g$layout[ia,]$t

  # g$grobs[[ia_out]]$label = ''
  g$heights[[ia_out]] <- unit( 0, "cm" )
  #
  g <- gtable_trim( g )
  g <- gtable_add_rows(g, unit(10,"lines"), 0) ## Sai's addition for extra spacing between axis label and axis title
  #

  # g[["widths"]][3] <- list(unit(0.5, "line"))

  # draw it
  # grid.newpage()

  # Output file -------------------------------------------------------------
  gtitle <- sprintf( "%s %s-%s: %s %s EV5on5",
    format( as.Date(game_info$game_date), "%a %m.%d.%y"),
    game_info$away_team_short, game_info$home_team_short,
    title_team, title.type )
  if( filter_period=="all" ) {
    gtitle <- paste0( gtitle, " through ", minutes_played, " min" )
  } else {
    gtitle <- paste0( gtitle, " - P", filter_period )
  }



  if( row_team == col_team ) {
    if( row_ha == "H" ) {
      plot.file <- paste0( fig_dir, "/", game_info$home_team_short, "-", file.suffix, "-", strength )
    } else {
      plot.file <- paste0( fig_dir, "/", game_info$away_team_short, "-", file.suffix, "-", strength )
    }
  } else {
    plot.file <- paste0( fig_dir, "/", game_info$away_team_short,"-", game_info$home_team_short, "-h2h-", file.suffix, "-", strength )
    if( matrix_type == "Corsi" ) {
      gtitle <- sprintf ("%s (%s perspective)", gtitle, our_team_short )
    }
  }
  if( filter_period=="all" ) {
    plot.file <- paste0( plot.file, ".png" )
  } else {
    plot.file <- paste0( plot.file, "-P", filter_period, ".png" )
  }

  message( gtitle )
  # png( filename = plot.file, width=700, height=700 )

  png( filename = plot.file, width=3.9, height=4.15, units="in", res=1200,pointsize=1 )
  par(
    mar      = c(0, 0, 0, 0),
    xaxs     = "i",
    yaxs     = "i",
    cex.axis = 1,
    cex.lab  = 1
  )
  # grid.draw(g)
  grid.arrange(g, main=textGrob(gtitle, gp=gpar(fontsize=base_size+1), vjust=2.5), clip=TRUE )
  dev.off()
}


#' Create Shot Color line chart.
#'
#' @param shots_df Data frame from Green shot quality project
#' @param goals_df Data frame of goals.
#' @param mandown_intervals_df Data frame of when each team is down.
#' @param game_info Game info from stage_game.
#' @param shift_interval_df from stage_shift_interval
#' @param ev5on5 Filters shots by ev5on5 or not. Default is FALSE.
#' @param shotcolor Chance type.  ALL, GREEN, RED, BLUE, WHITE.
#' @param include_blocked If \code{TRUE}, includes Blocked shots.
#' @param fig_dir
#'
#' @return chart
#' @export
#'
create_shotcolor_line_chart <- function( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df,
  ev5on5=FALSE,
  shotcolor="ALL", # GREEN RED BLUE WHITE
  include_blocked=F,
  fig_dir=NULL ) {

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
    file.name <- paste0( fig_dir, "/", away.team,"-", home.team, "-shot-attempts-line-chart.png" )
    file.name
    ggsave( filename = file.name, width=7.5, height=5. )
  }

  p.shots
}
