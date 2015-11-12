library( njdanalytics )
source( "sandbox/charts.R" )

nhl_db <- setup_nhl_db()
this_season     <- "20152016"
this_session_id <- "2"


# Get green shot data file ------------------------------------------------

# this_game_id4   <- "0037"
# game_file <- paste0( nhl_dir$shotdata, "/20151013 Gm3 NSH@NJD.xlsx" )
master_table_file <- paste0( nhl_dir$shotdata, "/g1-g10.xlsx" )
master_table <- read_excel( master_table_file, sheet = 1 )
master_table <- master_table %>% rename(
  period      = Period,
  clock       = Clock,
  strength    = Strength,
  event_team  = Team,
  shooter     = Shooter,
  shotcolor   = Color,
  shot        = Shot,
  attribute   = Type,
  comment     = Comment,
  game_number = `G#`
  )

# windows origin date is "1899-12-30"
# origin_time <- ymd_hms( "1899-12-30 20:00:00" )
# origin_time_regular_ot <- ymd_hms( "1899-12-30 05:00:00" )

origin_time            <- ms( "20:00" )
origin_time_regular_ot <- ms( "5:00"  )

shots_tbl <- master_table %>% mutate(
  period           = as.numeric( period   ),
  game_number      = as.numeric( game_number ),
  clock_ms         = ms( clock ),
  period_sec_cum   = ifelse( this_session_id == "3" | period <= 3,
                                      period_to_seconds( ms( "20:00" ) - clock_ms ),
                                      period_to_seconds( ms(  "5:00" ) - clock_ms ) ),
  period_min_cum   = round( period_sec_cum/60, 3 ),
  period_clock_cum = time_decimal_to_mmss( period_min_cum ),
  start_cum        = (period-1)*20 + period_min_cum
) %>% select( -clock_ms, -period_sec_cum, -period_min_cum )


# Get NHL data from our db ------------------------------------------------

stage_game           <- tbl( nhl_db, "stage_game"           )
stage_roster         <- tbl( nhl_db, "stage_roster"         )
stage_playbyplay     <- tbl( nhl_db, "stage_playbyplay"     )
stage_shift_interval <- tbl( nhl_db, "stage_shift_interval" )
team_score           <- tbl( nhl_db, "team_score"           )
game_player          <- tbl( nhl_db, "game_player"          )

njd_games            <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short=="NJD" ) %>%
                                       collect() %>% arrange( game_date )
njd_games            <- add_team_score_label( njd_games, our_team="NJD" ) %>%
                                              select( game_number, season, session_id, game_id4, ha, game_label )
stage_game           <- stage_game           %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect()
stage_roster         <- stage_roster         %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect()
stage_playbyplay     <- stage_playbyplay     %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect() %>% arrange( start_cum )
stage_shift_interval <- stage_shift_interval %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4 ) %>% collect() %>% arrange( start_cum )
game_player          <- game_player          %>% filter( season==this_season, session_id==this_session_id,
                                                          game_id4 %in% njd_games$game_id4,
                                                          filter_period == "all", filter_strength %in% c( "ev5on5", "all" )
                                                          ) %>% collect()

# NHL shot data for all games  -----------------------------------------------------
# use this as framework to which to attach other data
njd_game_player <- game_player %>% filter( filter_score_diff=="all", team_short==our_team ) %>%
  select( strength=filter_strength, season, session_id, game_id4, team_ha, team_short, ha_number, nhl_id, toi,
    cf, ca, c_net, cf_pct, ff, fa, f_net, ff_pct
  ) %>% mutate(
    c_total = cf+ca,
    f_total = ff+fa
  )

njd_game_team <- njd_game_player %>% filter( nhl_id==1 ) %>% right_join( njd_games, by=c("season", "session_id", "game_id4") ) %>%
  select( game_number, season, session_id, game_id4, game_label, everything() )

# on_ice_ids field is not populated so do it anew here
stage_shift_interval <- stage_shift_interval %>% select( -on_ice_ids )
stage_shift_interval <- stage_shift_interval %>% unite( "on_ice_ids", 32:45, sep=" ", remove=F )
stage_shift_interval$on_ice_ids <- stage_shift_interval$on_ice_ids %>% gsub( "NA ", "", ., fixed=T )

# note: we've tagged fewer games than actually played
shots_tbl <- shots_tbl %>% left_join( njd_games, by="game_number" )

# unblocked Green shots
fenwick_df <- shots_tbl %>% filter( shot!="BLOCK" )
# glimpse
fenwick_df %>% group_by( game_number, event_team ) %>% summarize( green=n() )


min_game_number <- min( shots_tbl$game_number )
max_game_number <- max( shots_tbl$game_number )

num_games <- max_game_number - min_game_number + 1

# single game -------------------------------------------------------------


this_game_number <- 8
for( this_game_number in min_game_number:max_game_number ) {
  message( "Process game ", this_game_number )

  this_game_id4 <- shots_tbl %>% filter( game_number==this_game_number ) %>% head(1) %>% select( game_id4 ) %>% unlist(use.names = F)

  # get NHL data for this game
  shots_df          <- shots_tbl %>%            filter( game_id4==this_game_id4 )
  game_info         <- stage_game %>%           filter( game_id4==this_game_id4 )
  this_roster       <- stage_roster %>%         filter( game_id4==this_game_id4 )
  pbp_df            <- stage_playbyplay %>%     filter( game_id4==this_game_id4 )
  shift_interval_df <- stage_shift_interval %>% filter( game_id4==this_game_id4 )
  this_game_player  <- game_player %>%          filter( game_id4==this_game_id4 )

  # meta data for chart
  mandown_intervals_df <- get_mandown_intervals( shift_interval_df, game_info )
  goals_df             <- get_goals_from_pbp( pbp_df, game_info )
  game_info            <- supplement_game_info( game_info, our_team="NJD" )

  # create_shot_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df,
  #                                 ev5on5=F, shotcolor="GREEN", include_blocked=F )

  # Join green shot data to NHL shift data ----------------------------------------
  shots_df <- shots_df %>% mutate(
    shift_interval_index = findInterval( shots_df$start_cum-1/60, shift_interval_df$start_cum, rightmost.closed = T )
  )

  matched_shift_inteval_df   <- shift_interval_df[ shots_df$shift_interval_index, ]
  shots_df$on_ice_ha_numbers <- matched_shift_inteval_df$on_ice_ha_numbers
  shots_df$on_ice_ids        <- matched_shift_inteval_df$on_ice_ids
  shots_df$our_score   <- ifelse( our_ha=="H", matched_shift_inteval_df$home_score, matched_shift_inteval_df$away_score )
  shots_df$their_score <- ifelse( our_ha=="H", matched_shift_inteval_df$away_score, matched_shift_inteval_df$home_score )

  shots_df <- shots_df %>% mutate(
    event_team_ha = ifelse( event_team==game_info$home_team_short, "H", "A" )
  )

  # tally up green shots for each player ------------------------------------------

  corsi_all       <- tally_for_against_by_ha_number( shots_df %>% filter() )
  corsi_ev5on5    <- tally_for_against_by_ha_number( shots_df %>% filter( strength=="EV 5v5") )
  fenwick_all     <- tally_for_against_by_ha_number( shots_df %>% filter( shot != "BLOCK") )
  fenwick_ev5on5  <- tally_for_against_by_ha_number( shots_df %>% filter( shot != "BLOCK", strength=="EV 5v5") )
  green_all       <- tally_for_against_by_ha_number( shots_df %>% filter( shotcolor=="GREEN", shot != "BLOCK") )
  green_ev5on5    <- tally_for_against_by_ha_number( shots_df %>% filter( shotcolor=="GREEN", shot != "BLOCK", strength=="EV 5v5") )

  game_block <- cbind( "season"=this_season, "session_id"=this_session_id, "game_id4"=this_game_id4, "game_number"=this_game_num )
  njd_chances <- rbind(
    cbind( game_block, "metric"="corsi",   "strength"="all",    corsi_all,      stringsAsFactors=F ),
    cbind( game_block, "metric"="corsi",   "strength"="ev5on5", corsi_ev5on5,   stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick", "strength"="all",    fenwick_all,    stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick", "strength"="ev5on5", fenwick_ev5on5, stringsAsFactors=F ),
    cbind( game_block, "metric"="green",   "strength"="all",    green_all,      stringsAsFactors=F ),
    cbind( game_block, "metric"="green",   "strength"="ev5on5", green_ev5on5,   stringsAsFactors=F )
  )
  njd_chances <- njd_chances %>% mutate(
    f_pct = round(f/(f+a)*100,1)
  )

  njd_chances    <- njd_chances %>% left_join( this_roster %>% select( ha_number, nhl_id, last_name, team_short ), by="ha_number" )
  njd_chances$nhl_id[     njd_chances$ha_number==our_ha ] <- 1
  njd_chances$last_name[  njd_chances$ha_number==our_ha ] <- "NJD"
  njd_chances$team_short[ njd_chances$ha_number==our_ha ] <- "NJD"
  njd_chances <- njd_chances %>% filter( team_short=="NJD" )
  njd_chances$game_number <- njd_chances$game_number %>% as.numeric()
  # team level
  njd_team_chances <- njd_chances %>% filter( last_name=="NJD"  )
  njd_team_chances %>% filter( metric=="corsi",   strength=="ev5on5" )
  njd_team_chances %>% filter( metric=="fenwick", strength=="ev5on5" )
  njd_team_chances %>% filter( metric=="green",   strength=="all"    )
  njd_team_chances %>% filter( metric=="green",   strength=="ev5on5" )

  # NJD team level - sneak peak
  njd_team_chances %>% filter( metric=="corsi",   strength=="all" )
  njd_team_chances %>% filter( metric=="corsi",   strength=="ev5on5" )



  # pairwise shot data ------------------------------------------------------
  # shots_df
  # roster

  goalies <- this_roster %>% filter( position=="G" ) %>% select( ha_number ) %>% unlist()
  goalies_or <- paste(goalies, collapse="|")
  ha_numbers <- this_roster %>% filter( position!="G" ) %>% select( ha_number ) %>%
                              unlist(use.names = F) %>% sort()

  # focus just on one team and remove goalies.  Sort ha_numbers in alphabetical order
  shots_df <- shots_df %>% mutate(
      ha_numbers_list = on_ice_ha_numbers %>% gsub( goalies_or, "", . ) %>%
                                        str_extract_all( "(\\w+)" ) %>%
                                        llply( sort ),
      pair            = ha_numbers_list %>% laply( get_pairs_of_ha_numbers )
  )

  shots_pairs_unnested <- shots_df %>% select( event_team_ha, pair ) %>% unnest()
  shots_pairs_table <- shots_pairs_unnested %>% group_by( pair ) %>% summarise(
    A = sum(event_team_ha=="A"),
    H = sum(event_team_ha=="H")
  )

# check
# shots_df %>% filter( event_team_ha=="A", grepl( "A02", on_ice_ha_numbers ), grepl( "A14", on_ice_ha_numbers ) ) %>%
#       select( clock, event_team_ha, on_ice_ha_numbers )

  # ALL from OUR perspective, even if home
  shots_pairs_table_colnames <- names( shots_pairs_table )[-1]
  our_sf_col <- shots_pairs_table_colnames==game_info$our_ha
  shots_pairs_table_colnames[  our_sf_col ] <- "sf"
  shots_pairs_table_colnames[ !our_sf_col ] <- "sa"
  names( shots_pairs_table )[-1] <- shots_pairs_table_colnames

  shots_pairs_table <- shots_pairs_table %>% separate( pair, c("ha_number_1", "ha_number_2" ) )

  njd_chances_pairs <- shots_pairs_table %>% left_join(
    this_roster %>% select( ha_number_1=ha_number, nhl_id_1=nhl_id ), by="ha_number_1"
  ) %>% left_join(
    this_roster %>% select( ha_number_2=ha_number, nhl_id_2=nhl_id ), by="ha_number_2"
  )

  # NHL shot data by player for this game  -----------------------------------------------------

  # all strength
  this_game_player_all <- this_game_player %>% filter( filter_strength=="all", filter_score_diff=="all", team_short==our_team ) %>%
                                                  select( season, session_id, game_id4, team_ha, team_short, ha_number, nhl_id, toi,
                                                    cf, ca, c_net, cf_pct, ff, fa, f_net, ff_pct
                                                  ) %>% mutate(
                                                    c_total = cf+ca,
                                                    f_total = ff+fa
                                                  )

  # ev5on5
  this_game_player_ev5on5 <- this_game_player %>% filter( filter_strength=="ev5on5", filter_score_diff=="all", team_short==our_team ) %>%
                                                  select( season, session_id, game_id4, team_ha, team_short, ha_number, nhl_id, toi,
                                                    cf, ca, c_net, cf_pct, ff, fa, f_net, ff_pct
                                                  ) %>% mutate(
                                                    c_total = cf+ca,
                                                    f_total = ff+fa
                                                  )

  # Attach shot data to team_score ------------------------------------------

  # corsi
  njd_game_team$c_total_video[
    njd_game_team$game_number==this_game_number & njd_game_team$strength=="all"
    ] <- njd_team_chances %>% filter(metric=="corsi", strength=="all") %>% select( total ) %>% unlist()
  njd_game_team$c_total_video[
    njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
    ] <- njd_team_chances %>% filter(metric=="corsi", strength=="ev5on5") %>% select( total ) %>% unlist()
  njd_game_team$cf_pct_video[
    njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
    ] <- njd_team_chances %>% filter(metric=="corsi", strength=="ev5on5") %>% select( f_pct ) %>% unlist()

  # fenwick
  njd_game_team$f_total_video[
    njd_game_team$game_number==this_game_number & njd_game_team$strength=="all"
    ] <- njd_team_chances %>% filter(metric=="fenwick", strength=="all") %>% select( total ) %>% unlist()
  njd_game_team$f_total_video[
    njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
    ] <- njd_team_chances %>% filter(metric=="fenwick", strength=="ev5on5") %>% select( total ) %>% unlist()
  njd_game_team$ff_pct_video[
    njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
    ] <- njd_team_chances %>% filter(metric=="fenwick", strength=="ev5on5") %>% select( f_pct ) %>% unlist()



} # for loop through games







# Compare NHL to ours -----------------------------------------------------

# corsi
njd_game_team_corsi_all <- njd_game_team %>% filter( strength=="all", !is.na(c_total_video) ) %>%
                            select( game_label, team_ha, c_total, c_total_video ) %>% mutate(
                              diff = c_total - c_total_video
                            )
njd_game_team_corsi_all_m <- njd_game_team_corsi_all %>% gather( source, count, -c(1:2))

njd_game_team_cf_pct_ev <- njd_game_team %>% filter( strength=="ev5on5", !is.na(cf_pct_video) ) %>%
  select( game_label, team_ha, cf_pct, cf_pct_video ) %>% mutate(
    # diff = cf_pct - cf_pct_video
  )
njd_game_team_cf_pct_ev_m <- njd_game_team_cf_pct_ev %>% gather( source, count, -c(1:2))

g.corsi_all <- ggplot( njd_game_team_corsi_all_m, aes(x=game_label,y=count) )
g.corsi_all + geom_bar(aes(fill=source), stat="identity", position = "dodge", width=0.7) +
  geom_hline(yintercept=0, size=0.5,color="red")+
  scale_fill_manual( "Data source",
    labels=c( "NHL", "NJD Video", "Difference"),
    values=c( "c_total"="black", "c_total_video"="darkred", "diff"="violet" ) ) +
  facet_grid(team_ha~.) +
  scale_y_continuous( "Count", breaks=seq(-100,200,20) ) +
  scale_x_discrete( "" )

g.cf_pct_ev <- ggplot( njd_game_team_cf_pct_ev_m, aes(x=game_label,y=count) )
g.cf_pct_ev + geom_bar(aes(fill=source), stat="identity", position = "dodge", width=0.7) +
  # geom_hline(yintercept=50, size=0.5,color="red")+
  scale_fill_manual( "Data source",
    labels=c( "NHL", "NJD Video"),
    values=c( "cf_pct"="black", "cf_pct_video"="darkred") ) +
  facet_grid(team_ha~.) +
  scale_y_continuous( "Corsi%", breaks=seq(-50,100,10) ) +
  scale_x_discrete( "" )

# fenwick
njd_game_team_fenwick_all <- njd_game_team %>% filter( strength=="all", !is.na(f_total_video) ) %>%
  select( game_label, team_ha, f_total, f_total_video ) %>% mutate(
    diff = f_total - f_total_video
  )
njd_game_team_fenwick_all_m <- njd_game_team_fenwick_all %>% gather( source, count, -c(1:2))

njd_game_team_ff_pct_ev <- njd_game_team %>% filter( strength=="ev5on5", !is.na(ff_pct_video) ) %>%
  select( game_label, team_ha, ff_pct, ff_pct_video ) %>% mutate(
  )
njd_game_team_ff_pct_ev_m <- njd_game_team_ff_pct_ev %>% gather( source, count, -c(1:2))

g.fenwick_all <- ggplot( njd_game_team_fenwick_all_m, aes(x=game_label,y=count) )
g.fenwick_all + geom_bar(aes(fill=source), stat="identity", position = "dodge", width=0.7) +
  geom_hline(yintercept=0, size=0.5,color="red")+
  scale_fill_manual( "Data source",
    labels=c( "NHL", "NJD Video", "Difference"),
    values=c( "f_total"="black", "f_total_video"="darkred", "diff"="violet" ) ) +
  facet_grid(team_ha~.) +
  scale_y_continuous( "Count", breaks=seq(-100,200,20) ) +
  scale_x_discrete( "" )

g.ff_pct_ev <- ggplot( njd_game_team_ff_pct_ev_m, aes(x=game_label,y=count) )
g.ff_pct_ev + geom_bar(aes(fill=source), stat="identity", position = "dodge", width=0.7) +
  geom_hline(yintercept=0, size=0.5,color="red")+
  scale_fill_manual( "Data source",
    labels=c( "NHL", "NJD Video"),
    values=c( "ff_pct"="black", "ff_pct_video"="darkred") ) +
  facet_grid(team_ha~.) +
  scale_y_continuous( "Fenwick%", breaks=seq(-50,100,10) ) +
  scale_x_discrete( "" )



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














