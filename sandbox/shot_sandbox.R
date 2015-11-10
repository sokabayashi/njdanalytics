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

origin_time            <- ms( "20:00" )
origin_time_regular_ot <- ms( "5:00"  )

shots_tbl <- master_table %>% mutate(
  period           = as.numeric( period   ),
  game_num         = as.numeric( game_num ),
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

# on_ice_ids field is not populated so do it anew here
stage_shift_interval <- stage_shift_interval %>% select( -on_ice_ids )
stage_shift_interval <- stage_shift_interval %>% unite( "on_ice_ids", 32:45, sep=" ", remove=F )
stage_shift_interval$on_ice_ids <- stage_shift_interval$on_ice_ids %>% gsub( "NA ", "", ., fixed=T )

# note: we've tagged fewer games than actually played
shots_tbl <- shots_tbl %>% left_join( njd_games %>% select( game_number, season, session_id, game_id4, ha ),
                                                      by=c( "game_num"="game_number") )



# aggregate stats ---------------------------------------------------------

# unblocked Green shots
fenwick_df <- shots_tbl %>% filter(shot!="BLOCK" )
fenwick_df %>% group_by( game_num, event_team ) %>% summarize( green=n() )

# single game -------------------------------------------------------------

this_game_num <- 4
this_game_id4 <- shots_tbl %>% filter( game_num==this_game_num ) %>% head(1) %>% select( game_id4 ) %>% unlist(use.names = F)

# get NHL data for this game
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
game_info            <- supplement_game_info( game_info, our_team="NJD" )

create_shot_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df,
                                ev5on5=F, shotcolor="GREEN", include_blocked=F )


# Join green shot data to NHL data ----------------------------------------

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


# tally up shots for each player ------------------------------------------

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

# NHL stats for Ev5on5
this_game_player_ev5on5 <- this_game_player %>% filter( filter_strength=="ev5on5", filter_score_diff=="all", team_short==our_team ) %>%
                                                select( team_ha, team_short, ha_number, nhl_id, toi,
                                                  cf, ca, c_net, ff, fa, f_net
                                                )
# NJD team level
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














