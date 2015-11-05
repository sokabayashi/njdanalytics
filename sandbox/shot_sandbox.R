

create_shot_line_chart <- function( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df,
    ev5on5=FALSE, shotcolor="ALL", include_blocked=F, this.game.dir=NULL) {

  # tedious to type out full var every time
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
    ev5on5 = strength=="EV 5v5"
  )

  # shot filters
  if( shotcolor != "ALL" ) {
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
    scale_colour_manual(name = "Team",
      values=line_colors ) +
    scale_shape_manual( name="", values=c(17,18), labels=c("Non-5on5 shot attempt", "even") )

  if( nrow( goals_df) > 0 ) {
    p.shots <- p.shots + geom_vline( data=goals_df, aes(xintercept=start_cum, color=event_team), linetype="solid", size=0.6 )

    goals_df$y.goal_text <- 1.1*y.axis.max
    p.shots <- p.shots + geom_text( data=goals_df, aes(label=goal_text, x=start_cum, y=y.goal_text, color=event_team),
      angle=90, show_guide=F, hjust= 0.8, size=geom_text.size, vjust=-0.4 )
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




# Chart a game ------------------------------------------------------------

library( njdanalytics )
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
stage_roster         <- tbl( nhl_db, "stage_roster"         )
stage_playbyplay     <- tbl( nhl_db, "stage_playbyplay"     )
stage_shift_interval <- tbl( nhl_db, "stage_shift_interval" )
team_score           <- tbl( nhl_db, "team_score"           )
game_player          <- tbl( nhl_db, "game_player"          )


njd_games         <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short=="NJD" ) %>% collect() %>% arrange( game_date )

stage_game           <- stage_game           %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect()
stage_roster         <- stage_roster         %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect()
stage_playbyplay     <- stage_playbyplay     %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect() %>% arrange( start_cum )
stage_shift_interval <- stage_shift_interval %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect() %>% arrange( start_cum )
game_player          <- game_player %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4,
                                                          filter_period == "all",
                                                          filter_strength %in% c( "ev5on5", "all" )
                                                          ) %>% collect()

# apparently on_ice_ids is not populated correctly
stage_shift_interval <- stage_shift_interval %>% select( -on_ice_ids )
stage_shift_interval <- stage_shift_interval %>% unite( "on_ice_ids", 32:45, sep=" ", remove=F )
stage_shift_interval$on_ice_ids <- stage_shift_interval$on_ice_ids %>% gsub( "NA ", "", ., fixed=T )

# note: we've tagged fewer games than actually played
shots_tbl <- shots_tbl %>% left_join( njd_games %>% select(
  game_number, season, session_id, game_id4, ha ),
  by=c( "game_num"="game_number") )

# aggregate stats ---------------------------------------------------------

# unblocked Green shots
fenwick_df <- shots_tbl %>% filter(shot!="BLOCK" )
fenwick_df %>% group_by( game_num, event_team ) %>% summarize( green=n() )

# single game -------------------------------------------------------------

this_game_num <- 8
this_game_id4 <- shots_tbl %>% filter( game_num==this_game_num ) %>% head(1) %>% select( game_id4 ) %>% unlist(use.names = F)

# get the base data for this game
this_game_score   <- njd_games %>%            filter( game_number==this_game_num )
shots_df          <- shots_tbl %>%            filter( game_id4==this_game_id4 )
game_info         <- stage_game %>%           filter( game_id4==this_game_id4 )
this_roster       <- stage_roster %>%         filter( game_id4==this_game_id4 )
pbp_df            <- stage_playbyplay %>%     filter( game_id4==this_game_id4 )
shift_interval_df <- stage_shift_interval %>% filter( game_id4==this_game_id4 )
this_game_player  <- game_player %>%          filter( game_id4==this_game_id4 )

# meta data for chart
mandown_intervals_df <- get_mandown_intervals( shift_interval_df, game_info )
goals_df             <- get_goals_from_pbp( pbp_df, game_info )

# NJD perspective
our_team <- "NJD"
our_ah   <- "H"
row_ha   <- "H"; col_ha   <- "A"
if( our_team %in% c( game_info$home_team_short, game_info$away_team_short ) ) {
  # an NJD game
  if( our_team == game_info$home_team_short ) {
    our_ha <- "H"
    their_team <- game_info$away_team_short
  } else {
    their_team <- game_info$home_team_short
    our_ha   <- "A"
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


create_shot_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df,
                                ev5on5=F, shotcolor="GREEN", include_blocked=F )


shots_df <- shots_df %>% mutate(
  shift_interval_index = findInterval( shots_df$start_cum-1/60, shift_interval_df$start_cum, rightmost.closed = T )
)

# shift_interval_df <- shift_interval_df %>% unite( "on_ice_ids", 32:45, sep=" ", remove=F )
# shift_interval_df$on_ice_ids <- shift_interval_df$on_ice_ids %>% gsub( " NA|NA ", "", . )

matched_shift_inteval_df <- shift_interval_df[ shots_df$shift_interval_index, ]
shots_df$on_ice_ha_numbers <- matched_shift_inteval_df$on_ice_ha_numbers
shots_df$on_ice_ids        <- matched_shift_inteval_df$on_ice_ids
shots_df$our_score   <- ifelse( our_ha=="H", matched_shift_inteval_df$home_score, matched_shift_inteval_df$away_score )
shots_df$their_score <- ifelse( our_ha=="H", matched_shift_inteval_df$away_score, matched_shift_inteval_df$home_score )

shots_df <- shots_df %>% mutate(
  event_team_ha = ifelse( event_team==game_info$home_team_short, "H", "A" )
)

corsi_all       <- tally_for_against_by_ha_number( shots_df %>% filter() )
corsi_ev5on5    <- tally_for_against_by_ha_number( shots_df %>% filter( strength=="EV 5v5") )
fenwick_all     <- tally_for_against_by_ha_number( shots_df %>% filter( shot != "BLOCK") )
fenwick_ev5on5  <- tally_for_against_by_ha_number( shots_df %>% filter( shot != "BLOCK", strength=="EV 5v5") )
green_all       <- tally_for_against_by_ha_number( shots_df %>% filter( shotcolor=="GREEN", shot != "BLOCK") )
green_ev5on5    <- tally_for_against_by_ha_number( shots_df %>% filter( shotcolor=="GREEN", shot != "BLOCK", strength=="EV 5v5") )

game_block <- cbind( "season"=this_season, "session_id"=this_session_id, "game_id4"=this_game_id4, "game_num"=this_game_num )
njd_chances <- rbind(
  cbind( game_block, "metric"="corsi",   "strength"="all",    corsi_all,      stringsAsFactors=F ),
  cbind( game_block, "metric"="corsi",   "strength"="ev5on5", corsi_ev5on5,   stringsAsFactors=F ),
  cbind( game_block, "metric"="fenwick", "strength"="all",    fenwick_all,    stringsAsFactors=F ),
  cbind( game_block, "metric"="fenwick", "strength"="ev5on5", fenwick_ev5on5, stringsAsFactors=F ),
  cbind( game_block, "metric"="green",   "strength"="all",    green_all,      stringsAsFactors=F ),
  cbind( game_block, "metric"="green",   "strength"="ev5on5", green_ev5on5,   stringsAsFactors=F )
)
njd_chances    <- njd_chances %>% left_join( this_roster %>% select( ha_number, nhl_id, last_name, team_short ), by="ha_number" )
njd_chances$nhl_id[     njd_chances$ha_number==our_ha ] <- 1
njd_chances$last_name[  njd_chances$ha_number==our_ha ] <- "NJD"
njd_chances$team_short[ njd_chances$ha_number==our_ha ] <- "NJD"
njd_chances <- njd_chances %>% filter( team_short=="NJD" )

# team level
njd_team_chances <- njd_chances %>% filter( last_name=="NJD"  )
njd_team_chances %>% filter( metric=="corsi",   strength=="ev5on5" )
njd_team_chances %>% filter( metric=="fenwick", strength=="ev5on5" )
njd_team_chances %>% filter( metric=="green",   strength=="all"    )
njd_team_chances %>% filter( metric=="green",   strength=="ev5on5" )

# NHL officials stats for Ev5on5
this_game_player_ev5on5 <- this_game_player %>% filter( filter_strength=="ev5on5", filter_score_diff=="all", team_short==our_team ) %>%
                                                select( team_ha, team_short, ha_number, nhl_id, toi,
                                                  cf, ca, c_net, ff, fa, f_net
                                                )
this_game_player_ev5on5 %>% filter( nhl_id==1 )


# Compare to NHL stats -------------------------------------------------
# NHL stats - ours.  So, -18 means they undercounted by 18 events.

# Corsi Ev5on5
nhl_corsi_ev5on5_undercount <- this_game_player_ev5on5 %>% filter( nhl_id==1 ) %>%
  select( cf, ca, c_net ) %>% mutate( total=cf+ca ) -
njd_team_chances %>% filter( metric=="corsi", strength=="ev5on5" ) %>%
            select( f, a, net ) %>% mutate( total=f+a )

# Fenwick Ev5on5
nhl_fewnick_ev5on5_undercount <- this_game_player_ev5on5 %>% filter( nhl_id==1 ) %>%
  select( ff, fa, f_net ) %>% mutate( total=ff+fa ) -
  njd_team_chances %>% filter( metric=="fenwick", strength=="ev5on5" ) %>%
  select( f, a, net ) %>% mutate( total=f+a )

print( nhl_corsi_ev5on5_undercount )
print( nhl_fewnick_ev5on5_undercount )














