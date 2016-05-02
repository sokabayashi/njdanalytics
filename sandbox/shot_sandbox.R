library( njdanalytics )
nhl_db <- setup_nhl_db()

# source( paste0( nhl_dir$base, "/packages/njdanalytics/sandbox/charts.R" ) )
this_season     <- "20152016"
this_session_id <- "2"
our_team        <- "NJD"

# Get green shot data file ------------------------------------------------

# Callie's Access file, exported to xlsx
master_table_file  <- paste0( nhl_dir$shot, "/Master Table.xlsx" )
master_table_file2 <- paste0( nhl_dir$shot, "/Master Table2.xlsx" )
mismatch_ev5on5_file <- paste0(nhl_dir$shot, "/mismatch_ev5on5.csv")
if( file.exists(mismatch_ev5on5_file) ) {
  file.remove( mismatch_ev5on5_file )
}

master_table  <- read_excel( master_table_file, sheet = 1 )
master_table2 <- read_excel( master_table_file2, sheet = 1 )
master_table2$`G#` <- master_table2$`G#` %>% as.numeric()
master_table  <- bind_rows( master_table, master_table2 )

master_table <- master_table %>% rename(
  period      = Period,
  clock       = Clock,
  strength    = Strength,
  event_team  = Team,
  shooter     = Shooter,
  passer      = Passer,
  shotcolor   = Color,
  shot        = Shot,
  attribute   = Type,
  comment     = Comment,
  game_number = `G#`
  ) %>% filter( !is.na(shot) ) %>% select( -ID )

origin_time            <- ms( "20:00" )
origin_time_regular_ot <- ms( "5:00"  )

## shots_tbl will house all our shot data
shots_tbl <- master_table %>% mutate(
  period           = as.numeric( period      ),
  game_number      = as.numeric( game_number ),
  clock_ms         = ms( clock ),
  period_sec_cum   = ifelse( this_session_id == "3" | period <= 3,
                              period_to_seconds( ms( "20:00" ) - clock_ms ),
                              period_to_seconds( ms(  "5:00" ) - clock_ms ) ),
  period_min_cum   = round( period_sec_cum/60, 3 ),
  period_clock_cum = time_decimal_to_mmss( period_min_cum ),
  start_cum        = (period-1)*20 + period_min_cum
) %>% select( game_number, start_cum, everything(), -clock_ms, -period_sec_cum, -period_min_cum, -period_clock_cum ) %>%
      arrange( game_number, start_cum )


# NHL data from our db for *ALL* NJD games ------------------------------------------------
# collect() all of these tables

player_tbl           <- tbl( nhl_db, "player"               ) %>% collect()
stage_game           <- tbl( nhl_db, "stage_game"           )
stage_roster         <- tbl( nhl_db, "stage_roster"         )
stage_playbyplay     <- tbl( nhl_db, "stage_playbyplay"     )
stage_shift_interval <- tbl( nhl_db, "stage_shift_interval" )
team_score           <- tbl( nhl_db, "team_score"           )
game_player          <- tbl( nhl_db, "game_player"          )

# njd_games has game_number and game_id4 to join different data sources
njd_games            <- team_score %>% filter( season==this_season, session_id==this_session_id, team_short=="NJD" ) %>%
                                       collect() %>% arrange( game_date )
njd_games            <- add_team_score_label( njd_games, our_team="NJD" ) %>%
                                        select( game_number, season, session_id, game_id4, ha, opp_team_short, game_label )
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

# useful NHL shot data  -----------------------------------------------------

njd_game_player <- game_player %>% filter( filter_score_diff=="all", team_short==our_team ) %>%
  select( strength=filter_strength, season, session_id, game_id4, team_ha, team_short, ha_number, nhl_id, toi,
    cf, ca, c_net, cf_pct, ff, fa, f_net, ff_pct, cf_i, ff_i, sf_i
  ) %>% mutate(
    c_total = cf+ca,
    f_total = ff+fa
  )

njd_game_team <- njd_game_player %>% filter( nhl_id==1 ) %>% right_join( njd_games, by=c("season", "session_id", "game_id4") ) %>%
  select( game_number, season, session_id, game_id4, game_label, everything() )

# on_ice_ids field is not populated correctly in db so do it anew here
stage_shift_interval <- stage_shift_interval %>% select( -on_ice_ids )
stage_shift_interval <- stage_shift_interval %>% unite( "on_ice_ids", 32:45, sep=" ", remove=F )
stage_shift_interval$on_ice_ids <- stage_shift_interval$on_ice_ids %>% gsub( "NA ", "", ., fixed=T )

# NOTE: we've reviewed fewer games than actually played.
# append on game_id4 so we can join with official NHL game data
shots_tbl <- shots_tbl %>% left_join( njd_games, by="game_number" )



# Process single game shots data -------------------------------------------------------------

min_game_number <- min( shots_tbl$game_number )
max_game_number <- max( shots_tbl$game_number )
num_games <- max_game_number - min_game_number + 1
# this_game_number <- 10 # only useful when debugging

shots_all_df   <- vector( "list", num_games ) # Store all shots_tbl with augmented info
player_chances <- vector( "list", num_games ) # Store all player results
pair_chances   <- vector( "list", num_games ) # Store all player-pair results

game_numbers <- shots_tbl$game_number %>% unique() %>% sort()
# how many games?
game_numbers %>% length()
for( this_game_number in game_numbers ) {

  message( "Process game ", this_game_number )
  this_game_id4 <- shots_tbl %>% filter( game_number==this_game_number ) %>% head(1) %>%
                                 select( game_id4 ) %>% unlist(use.names = F)

  # data for this game only
  shots_df          <- shots_tbl %>%            filter( game_id4==this_game_id4 )
  game_info         <- stage_game %>%           filter( game_id4==this_game_id4 )
  this_roster       <- stage_roster %>%         filter( game_id4==this_game_id4 )
  pbp_df            <- stage_playbyplay %>%     filter( game_id4==this_game_id4 )
  shift_interval_df <- stage_shift_interval %>% filter( game_id4==this_game_id4 )
  this_game_player  <- game_player %>%          filter( game_id4==this_game_id4 )

  # meta data for chart
  mandown_intervals_df <- get_mandown_intervals( shift_interval_df, game_info )
  goals_df             <- get_goals_from_pbp(    pbp_df, game_info )
  game_info            <- supplement_game_info(  game_info, our_team="NJD" )

  # CHART
  # create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="ALL", include_blocked=T )
  # create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="GREEN", include_blocked=F )
  # create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="BLUE", include_blocked=F )

  # create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="GREENBLUE", include_blocked=F )

  # Join green shot data to NHL shift interval (on ice player) data ----------------------------------------
  # Do this one game at a time since 0-60 min will occur in every game.
  shots_df <- shots_df %>% mutate(
    shift_interval_index = findInterval( shots_df$start_cum-1/60, shift_interval_df$start_cum, rightmost.closed = T )
  )

  ## HACK FOR BAD SHIFT INTERVALS
  # No interval should have 0 players (skaters + goalies) for EITHER team.
  # PS is the only situation I can think of, and that has no time duration (according to clock)
  # bad_shift_intervals <- shift_interval_df %>% filter( num_players_h==0 | num_players_a==0 )
  if( this_season=="20152016" && this_session_id=="2" ) {
    if( this_game_id4 == "0212" ) {
      shots_df$shift_interval_index[ shots_df$shift_interval_index %in% c( 101,102) ] <- 100
    }
  }

  matched_shift_inteval_df   <- shift_interval_df[ shots_df$shift_interval_index, ]
  shots_df$on_ice_ha_numbers <- matched_shift_inteval_df$on_ice_ha_numbers
  shots_df$home_goalie       <- matched_shift_inteval_df$home_goalie
  shots_df$away_goalie       <- matched_shift_inteval_df$away_goalie
  shots_df$on_ice_ids        <- matched_shift_inteval_df$on_ice_ids
  shots_df$num_skaters_h     <- matched_shift_inteval_df$num_skaters_h
  shots_df$num_skaters_a     <- matched_shift_inteval_df$num_skaters_a
  shots_df$num_goalies_h     <- matched_shift_inteval_df$num_goalies_h
  shots_df$num_goalies_a     <- matched_shift_inteval_df$num_goalies_a
  shots_df$our_score         <- ifelse( game_info$our_ha=="H",
                                        matched_shift_inteval_df$home_score, matched_shift_inteval_df$away_score )
  shots_df$their_score       <- ifelse( game_info$our_ha=="H",
                                        matched_shift_inteval_df$away_score, matched_shift_inteval_df$home_score )

  # finally
  shots_df <- shots_df %>% mutate(
    event_team_ha      = ifelse( event_team==game_info$home_team_short, "H", "A" ),
    num_players_on_ice = on_ice_ha_numbers %>% str_split( " " ) %>% laply(length ),
    ev5on5             = ifelse( (num_skaters_h==5 & num_skaters_a==5 & num_goalies_h==1 & num_goalies_a==1), TRUE, FALSE ),
    shooter_ha_number  = get_ha_number( event_team_ha, shooter ),
    passer_ha_number   = ifelse( passer=="NA", NA, get_ha_number( event_team_ha, shooter ) )
  ) %>% select( -shift_interval_index )

  # Check *our* strength field from manual entry
  mismatch_ev5on5 <- (shots_df$strength=="EV 5v5") != shots_df$ev5on5
  if( nrow(shots_df[mismatch_ev5on5,]) ) {
    print( shots_df[mismatch_ev5on5,] )
  }

  write_csv( shots_df[mismatch_ev5on5, ], path=mismatch_ev5on5_file, append=T )


  # tally up shots for each player ------------------------------------------
  # Corsi - All and EV
  # Fenwick - All and EV
  shots_df_ev5on5  <- shots_df        %>% filter( strength=="EV 5v5" )
  shots_df_pp      <- shots_df        %>% filter( grepl( "PP", strength ) )
  shots_df_sh      <- shots_df        %>% filter( grepl( "SH", strength ) )
  ushots_df        <- shots_df        %>% filter( shot != "BLOCK" )
  ushots_df_ev5on5 <- shots_df_ev5on5 %>% filter( shot != "BLOCK" )
  ushots_df_pp     <- shots_df_pp     %>% filter( shot != "BLOCK" )
  ushots_df_sh     <- shots_df_sh     %>% filter( shot != "BLOCK" )

  corsi_all       <- tally_sc_by_ha_number( shots_df        )
  corsi_ev5on5    <- tally_sc_by_ha_number( shots_df_ev5on5 )
  corsi_pp        <- tally_sc_by_ha_number( shots_df_pp     )
  corsi_sh        <- tally_sc_by_ha_number( shots_df_sh     )

  goal_all        <- tally_sc_by_ha_number( shots_df        %>% filter( shot == "GOAL" ) )
  goal_ev5on5     <- tally_sc_by_ha_number( shots_df_ev5on5 %>% filter( shot == "GOAL" ) )
  goal_pp         <- tally_sc_by_ha_number( shots_df_pp     %>% filter( shot == "GOAL" ) )
  goal_sh         <- tally_sc_by_ha_number( shots_df_sh     %>% filter( shot == "GOAL" ) )


  fenwick_all     <- tally_sc_by_ha_number( ushots_df        )
  fenwick_ev5on5  <- tally_sc_by_ha_number( ushots_df_ev5on5 )
  fenwick_pp      <- tally_sc_by_ha_number( ushots_df_pp     )
  fenwick_sh      <- tally_sc_by_ha_number( ushots_df_sh     )

  # UNBLOCKED Green and Blue shots
  green_all       <- tally_sc_by_ha_number( ushots_df        %>% filter( shotcolor=="GREEN" ) )
  green_ev5on5    <- tally_sc_by_ha_number( ushots_df_ev5on5 %>% filter( shotcolor=="GREEN" ) )
  green_pp        <- tally_sc_by_ha_number( ushots_df_pp     %>% filter( shotcolor=="GREEN" ) )
  green_sh        <- tally_sc_by_ha_number( ushots_df_sh     %>% filter( shotcolor=="GREEN" ) )

  blue_all        <- tally_sc_by_ha_number( ushots_df        %>% filter( shotcolor=="BLUE" ) )
  blue_ev5on5     <- tally_sc_by_ha_number( ushots_df_ev5on5 %>% filter( shotcolor=="BLUE" ) )
  blue_pp         <- tally_sc_by_ha_number( ushots_df_pp     %>% filter( shotcolor=="BLUE" ) )
  blue_sh         <- tally_sc_by_ha_number( ushots_df_sh     %>% filter( shotcolor=="BLUE" ) )

  greenblue_ev5on5    <- tally_sc_by_ha_number( ushots_df_ev5on5 %>% filter( shotcolor=="GREEN" | shotcolor=="BLUE" ) )

  # greennomiss_ev5on5 <- tally_sc_by_ha_number( shots_df %>%
  #     filter( shotcolor=="GREEN", shot != "MISS", shot != "BLOCK", strength=="EV 5v5") )
  #
  # bluenomiss_ev5on5 <- tally_sc_by_ha_number( shots_df %>%
  #     filter( shotcolor=="BLUE", shot != "MISS", shot != "BLOCK", strength=="EV 5v5") )
  #
  # # NO MISS for GREEN or BLUE
  # greenblue_nomiss_ev5on5    <- tally_sc_by_ha_number( shots_df %>%
  #     filter( (shotcolor=="GREEN" | shotcolor=="BLUE"), shot !="MISS", shot != "BLOCK", strength=="EV 5v5") )
  #
  # # GREEN or (BLUE NO MISS)
  # green_bluenomiss_ev5on5    <- tally_sc_by_ha_number( shots_df %>%
  #     filter( shot != "BLOCK", strength=="EV 5v5", (shotcolor=="GREEN" | (shotcolor=="BLUE" & shot!="MISS") ) ) )
  #

  # prefix columns
  game_block <- cbind( "season"=this_season, "session_id"=this_session_id,
                       "game_id4"=this_game_id4, "game_number"=this_game_number )
  njd_chances <- rbind(
    cbind( game_block, "metric"="corsi",            "strength"="all",    corsi_all,               stringsAsFactors=F ),
    cbind( game_block, "metric"="corsi",            "strength"="ev5on5", corsi_ev5on5,            stringsAsFactors=F ),
    cbind( game_block, "metric"="corsi",            "strength"="pp",     corsi_pp,                stringsAsFactors=F ),
    cbind( game_block, "metric"="corsi",            "strength"="sh",     corsi_sh,                stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick",          "strength"="all",    fenwick_all,             stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick",          "strength"="ev5on5", fenwick_ev5on5,          stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick",          "strength"="pp",     fenwick_pp,              stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick",          "strength"="sh",     fenwick_sh,              stringsAsFactors=F ),
    cbind( game_block, "metric"="goal",             "strength"="all",    goal_all,                stringsAsFactors=F ),
    cbind( game_block, "metric"="goal",             "strength"="ev5on5", goal_ev5on5,             stringsAsFactors=F ),
    cbind( game_block, "metric"="goal",             "strength"="pp",     goal_pp,                 stringsAsFactors=F ),
    cbind( game_block, "metric"="goal",             "strength"="sh",     goal_sh,                 stringsAsFactors=F ),
    cbind( game_block, "metric"="green",            "strength"="all",    green_all,               stringsAsFactors=F ),
    cbind( game_block, "metric"="green",            "strength"="ev5on5", green_ev5on5,            stringsAsFactors=F ),
    cbind( game_block, "metric"="green",            "strength"="pp",     green_pp,                stringsAsFactors=F ),
    cbind( game_block, "metric"="green",            "strength"="sh",     green_sh,                stringsAsFactors=F ),
    cbind( game_block, "metric"="blue",             "strength"="all",    blue_all,                stringsAsFactors=F ),
    cbind( game_block, "metric"="blue",             "strength"="ev5on5", blue_ev5on5,             stringsAsFactors=F ),
    cbind( game_block, "metric"="blue",             "strength"="pp",     blue_pp,                 stringsAsFactors=F ),
    cbind( game_block, "metric"="blue",             "strength"="sh",     blue_sh,                 stringsAsFactors=F ),
    cbind( game_block, "metric"="greenblue",        "strength"="ev5on5", greenblue_ev5on5,        stringsAsFactors=F )
    # cbind( game_block, "metric"="greennomiss",      "strength"="ev5on5", greennomiss_ev5on5,      stringsAsFactors=F ),
    # cbind( game_block, "metric"="bluenomiss",       "strength"="ev5on5", bluenomiss_ev5on5,       stringsAsFactors=F )
    # cbind( game_block, "metric"="greenblue_nomiss", "strength"="ev5on5", greenblue_nomiss_ev5on5, stringsAsFactors=F ),
    # cbind( game_block, "metric"="green_bluenomiss", "strength"="ev5on5", green_bluenomiss_ev5on5, stringsAsFactors=F )
  )
  njd_chances <- njd_chances %>% mutate(
    scf_pct = round(scf/(scf+sca)*100,1),
    scf_i   = ifelse( is.na(scf_i), 0, scf_i )
  )

  # Join the players by ha_number
  njd_chances    <- njd_chances %>% left_join( this_roster %>% select( ha_number, nhl_id, last_name, team_short ), by="ha_number" )

  njd_chances$nhl_id[     njd_chances$ha_number==game_info$our_ha ]   <- 1
  njd_chances$last_name[  njd_chances$ha_number==game_info$our_ha ]   <- "NJD"
  njd_chances$team_short[ njd_chances$ha_number==game_info$our_ha ]   <- "NJD"
  njd_chances$last_name[  njd_chances$ha_number==game_info$their_ha ] <- game_info$their_team
  njd_chances$team_short[ njd_chances$ha_number==game_info$their_ha ] <- game_info$their_team

  #  njd_chances <- njd_chances %>% filter( team_short=="NJD" )  # we only care about our players (I think)
  njd_chances$game_number <- njd_chances$game_number %>% as.numeric()

  # team level - peek!
  njd_team_chances <- njd_chances %>% filter( last_name=="NJD"  )
  njd_team_chances %>% filter( metric=="corsi",   strength=="all"    )
  njd_team_chances %>% filter( metric=="corsi",   strength=="ev5on5" )
  njd_team_chances %>% filter( metric=="fenwick", strength=="ev5on5" )
  njd_team_chances %>% filter( metric=="green",   strength=="all"    )
  njd_team_chances %>% filter( metric=="green",   strength=="ev5on5" )


  # Pairwise shot data ------------------------------------------------------
  # shots_df
  # roster
  # Do this with ha_number rather than nhl_id level.  no reason, just easier to look at.  Join nhl_id later.
  # EV only since we're really concerned about matchup h2h.
  corsi_ev_pairs     <- tally_sc_by_ha_number_pairs( shots_df_ev5on5,
                                                     this_roster, game_info$our_ha, game_info$their_ha )
  fenwick_ev_pairs   <- tally_sc_by_ha_number_pairs( ushots_df_ev5on5,
                                                     this_roster, game_info$our_ha, game_info$their_ha )
  goal_ev_pairs      <- tally_sc_by_ha_number_pairs( ushots_df_ev5on5 %>% filter( shot == "GOAL" ),
                                                     this_roster, game_info$our_ha, game_info$their_ha )
  green_ev_pairs     <- tally_sc_by_ha_number_pairs( ushots_df_ev5on5 %>% filter( shotcolor == "GREEN" ),
                                                     this_roster, game_info$our_ha, game_info$their_ha )
  blue_ev_pairs      <- tally_sc_by_ha_number_pairs( ushots_df_ev5on5 %>% filter( shotcolor == "BLUE" ),
                                                     this_roster, game_info$our_ha, game_info$their_ha )
  greenblue_ev_pairs <- tally_sc_by_ha_number_pairs( ushots_df_ev5on5 %>% filter( shotcolor == "GREEN" | shotcolor == "BLUE" ),
                                                     this_roster, game_info$our_ha, game_info$their_ha )
  pair_chances_game <- rbind(
    cbind( game_block, "metric"="corsi",     "strength"="ev5on5", corsi_ev_pairs,       stringsAsFactors=F ),
    cbind( game_block, "metric"="fenwick",   "strength"="ev5on5", fenwick_ev_pairs,     stringsAsFactors=F ),
    cbind( game_block, "metric"="goal",      "strength"="ev5on5", goal_ev_pairs,        stringsAsFactors=F ),
    cbind( game_block, "metric"="green",     "strength"="ev5on5", green_ev_pairs,       stringsAsFactors=F ),
    cbind( game_block, "metric"="blue",      "strength"="ev5on5", blue_ev_pairs,        stringsAsFactors=F ),
    cbind( game_block, "metric"="greenblue", "strength"="ev5on5", greenblue_ev_pairs,   stringsAsFactors=F )
  ) %>% as_data_frame()

  pair_chances_game <- pair_chances_game %>% left_join(
    this_roster %>%
      select( ha_number_1=ha_number, nhl_id_1=nhl_id, last_name_1=last_name, team_short_1=team_short ),
              by="ha_number_1" ) %>% left_join(
    this_roster %>%
      select( ha_number_2=ha_number, nhl_id_2=nhl_id, last_name_2=last_name, team_short_2=team_short ),
              by="ha_number_2" ) %>% mutate(
      sc_net = scf - sca
  )

  pair_chances[[this_game_number]] <- pair_chances_game

  # official NHL shot data by player for this game  -----------------------------------------------------

  # all strength
  this_game_player_all <- this_game_player %>% filter( filter_strength=="all", filter_score_diff=="all", team_short==our_team ) %>%
                                                  select( season, session_id, game_id4, strength=filter_strength,
                                                    team_ha, team_short, ha_number, nhl_id, toi,
                                                    cf, ca, c_net, cf_pct, ff, fa, f_net, ff_pct
                                                  ) %>% mutate(
                                                    c_total = cf+ca,
                                                    f_total = ff+fa
                                                  )

  # ev5on5
  this_game_player_ev5on5 <- this_game_player %>% filter( filter_strength=="ev5on5", filter_score_diff=="all", team_short==our_team ) %>%
                                                  select( season, session_id, game_id4, strength=filter_strength,
                                                    team_ha, team_short, ha_number, nhl_id, toi,
                                                    cf, ca, c_net, cf_pct, ff, fa, f_net, ff_pct
                                                  ) %>% mutate(
                                                    c_total = cf+ca,
                                                    f_total = ff+fa
                                                  )

  # # team level - Attach shot data to team_score ------------------------------------------
  # # corsi
  # njd_game_team$c_total_video[
  #   njd_game_team$game_number==this_game_number & njd_game_team$strength=="all"
  #   ] <- njd_team_chances %>% filter(metric=="corsi", strength=="all") %>% select( sc_total ) %>% unlist()
  # njd_game_team$c_total_video[
  #   njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
  #   ] <- njd_team_chances %>% filter(metric=="corsi", strength=="ev5on5") %>% select( sc_total ) %>% unlist()
  # njd_game_team$cf_pct_video[
  #   njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
  #   ] <- njd_team_chances %>% filter(metric=="corsi", strength=="ev5on5") %>% select( scf_pct ) %>% unlist()
  #
  # # fenwick
  # njd_game_team$f_total_video[
  #   njd_game_team$game_number==this_game_number & njd_game_team$strength=="all"
  #   ] <- njd_team_chances %>% filter(metric=="fenwick", strength=="all") %>% select( sc_total ) %>% unlist()
  # njd_game_team$f_total_video[
  #   njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
  #   ] <- njd_team_chances %>% filter(metric=="fenwick", strength=="ev5on5") %>% select( sc_total ) %>% unlist()
  # njd_game_team$ff_pct_video[
  #   njd_game_team$game_number==this_game_number & njd_game_team$strength=="ev5on5"
  #   ] <- njd_team_chances %>% filter(metric=="fenwick", strength=="ev5on5") %>% select( scf_pct ) %>% unlist()
  #



# join game_player --------------------------------------------------------


  njd_chances_toi    <- njd_chances %>% left_join( this_game_player %>%
                                        filter( filter_score_diff=="all", filter_period=="all" ) %>%
                                        select( strength=filter_strength, nhl_id, toi, cf, ca, c_net, ff, fa, f_net ),
                                by=c("strength", "nhl_id") )

    # ha_number, nhl_id, last_name, team_short
  player_chances[[this_game_number]] <- njd_chances_toi
  shots_all_df[[this_game_number]] <- shots_df

} # for loop through games

player_chances_df <- do.call("rbind", player_chances )
pair_chances_df   <- do.call("rbind", pair_chances )
shots_all_df      <- do.call("rbind", shots_all_df )

njd_players_by_game   <- player_chances_df %>% filter( team_short=="NJD" ) %>%
                          left_join( player_tbl %>% select( nhl_id, position_fd, number ), by="nhl_id" )

shots_tbl$event_team <- ifelse( shots_tbl$event_team=="TB", "TBL", shots_tbl$event_team )
save( njd_games, shots_all_df, player_chances_df, njd_players_by_game, pair_chances_df,
      file=paste0( nhl_dir$shot, "/njd_through_", this_game_number, ".RData" ) )
load( file=paste0( nhl_dir$shot, "/njd_through_", this_game_number, ".RData" ))


















# sneak peak --------------------------------------------------------------

all_corsi_ev     <- njd_player_chances_df %>% filter( team_short=="NJD", metric=="corsi",     strength=="ev5on5" )
all_fenwick_ev   <- njd_player_chances_df %>% filter( team_short=="NJD", metric=="fenwick",   strength=="ev5on5" )
all_green_ev     <- njd_player_chances_df %>% filter( team_short=="NJD", metric=="green",     strength=="ev5on5" )
all_blue_ev      <- njd_player_chances_df %>% filter( team_short=="NJD", metric=="blue",      strength=="ev5on5" )
all_greenblue_ev <- njd_player_chances_df %>% filter( team_short=="NJD", metric=="greenblue", strength=="ev5on5" )
# all_green_nomiss_ev     <- njd_player_chances_df %>% filter( metric=="greennomiss",     strength=="ev5on5" )
# all_gb_nomiss_ev     <- njd_player_chances_df %>% filter( metric=="greenblue_nomiss",     strength=="ev5on5" )

njd_summary <- njd_player_chances_df %>% group_by( metric, strength, nhl_id, last_name ) %>%
                summarize(
                  scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net), scf_i=sum(scf_i),
                  cf =sum(cf),  ca = sum(ca), c_net = sum(c_net)
                )

all_corsi_ev %>%     group_by( nhl_id, last_name ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net), scf_i=sum(scf_i) ) %>% View
all_fenwick_ev %>%   group_by( nhl_id, last_name ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net), scf_i=sum(scf_i) ) %>% View
all_green_ev %>% group_by( nhl_id, last_name ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net), scf_i=sum(scf_i) ) %>% View
all_blue_ev %>% group_by( nhl_id, last_name ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net), scf_i=sum(scf_i) ) %>% View
all_greenblue_ev %>% group_by( nhl_id, last_name ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net), scf_i=sum(scf_i) ) %>% View

all_green_ev %>%  group_by( nhl_id, last_name ) %>% summarize(
  gm = n(),
  scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net),
  toi_total=sum(toi) %>% round(1),
  scf_60   =(scf/toi_total*60) %>% round(1),
  sca_60   =(sca/toi_total*60) %>% round(1),
  sc_net_60=(sc_net/toi_total*60) %>% round(1),
  scf_i=sum(scf_i),
  scf_i_60 = (scf_i/toi_total*60) %>% round(1)
  ) %>% ungroup() %>% arrange( desc(sc_net) ) %>%  filter( toi_total > 80 ) %>% View()

# all_green_nomiss_ev %>%     group_by( nhl_id, last_name ) %>%
#   summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net) ) %>% View
# all_gb_nomiss_ev %>%     group_by( nhl_id, last_name ) %>%
#   summarize( scf=sum(scf), a=sum(sca), sc_net=sum(sc_net) ) %>% View














# Compare NHL to ours -----------------------------------------------------


# corsi
njd_game_team_corsi_all <- njd_game_team %>% filter( strength=="all", !is.na(c_total_video) ) %>%
                            select( game_label, team_ha, c_total, c_total_video ) %>% mutate(
                              diff = c_total - c_total_video
                            )

njd_game_team_corsi_all  %>% group_by( team_ha )  %>% summarize( c_total = sum(c_total), c_total_video=sum(c_total_video),
  diff=c_total-c_total_video, error_rate = diff/c_total_video )


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

njd_game_team_fenwick_all  %>% group_by( team_ha )  %>% summarize( f_total = sum(f_total), f_total_video=sum(f_total_video),
  diff=f_total-f_total_video, error_rate = diff/f_total_video)


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



# NHL stats - ours, for last game.  So, -18 means they undercounted by 18 events.

# Corsi Ev5on5
nhl_corsi_ev5on5_undercount <- this_game_player_ev5on5 %>% filter( nhl_id==1 ) %>%
  select( cf, ca, c_net ) %>% mutate( total=cf+ca ) -
njd_team_chances %>% filter( metric=="corsi", strength=="ev5on5" ) %>%
            select( cf=scf, ca=sca, c_net=sc_net ) %>% mutate( total=cf+ca )

# Fenwick Ev5on5
nhl_fewnick_ev5on5_undercount <- this_game_player_ev5on5 %>% filter( nhl_id==1 ) %>%
  select( ff, fa, f_net ) %>% mutate( total=ff+fa ) -
  njd_team_chances %>% filter( metric=="fenwick", strength=="ev5on5" ) %>%
  select( ff=scf, fa=sca, f_net=sc_net ) %>% mutate( total=ff+fa )

print( nhl_corsi_ev5on5_undercount )
print( nhl_fewnick_ev5on5_undercount )



# vs a specific team ------------------------------------------------------

opponent <- "MIN"
games_vs_opp <- njd_games %>% filter( opp_team_short==opponent )

this_game_number <- games_vs_opp$game_number
player_chances_vs_opp <- player_chances_df %>% filter( game_number %in% games_vs_opp$game_number )

# or perhaps we know the exact game number
# this_game_number <- c( 67)

# or perhaps we want season to date NJD only
# player_chances_vs_opp <- njd_player_chances_df %>% filter( team_short=="NJD" )

this_game_id4 <- shots_tbl %>% filter( game_number %in% this_game_number ) %>%
  select( game_id4 ) %>% unlist(use.names = F) %>% unique()
player_chances_vs_opp <- player_chances_df %>% filter( game_number %in% c( this_game_number) )


# data for one game only
this_game_id4 <- this_game_id4[1]

shots_df          <- shots_tbl %>%            filter( game_id4==this_game_id4 )
game_info         <- stage_game %>%           filter( game_id4==this_game_id4 )
this_roster       <- stage_roster %>%         filter( game_id4==this_game_id4 )
pbp_df            <- stage_playbyplay %>%     filter( game_id4==this_game_id4 )
shift_interval_df <- stage_shift_interval %>% filter( game_id4==this_game_id4 )
this_game_player  <- game_player %>%          filter( game_id4==this_game_id4 )

# append faceoff count and L/R shot
roster_skaters <- augment_roster( this_roster, pbp_df, player_tbl ) %>% filter( position != "G" )
shared_toi_ev  <- shift_interval_df %>% filter( num_skaters_h==5, num_skaters_a==5, num_goalies_h==1, num_goalies_a==1 )
toi_matrix_ev  <- get_toi_matrix( this_roster, shared_toi_ev ) # need to pass *original* roster

# # sort roster by lines, pairings.

# debugonce(group_roster_by_lines)
roster_sorted  <- group_roster_by_lines( roster_skaters, toi_matrix_ev, strength="ev5on5" )

# meta data for chart
mandown_intervals_df <- get_mandown_intervals( shift_interval_df, game_info )
goals_df             <- get_goals_from_pbp( pbp_df, game_info )
game_info            <- supplement_game_info( game_info, our_team="NJD" )

# CHART
 create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="GREEN", include_blocked=F )
 create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="BLUE", include_blocked=F )

 create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="GREENBLUE", include_blocked=F )

 foo_ev <- shots_df %>% filter( shot != "BLOCK", strength=="EV 5v5", shotcolor %in% c( "BLUE", "GREEN") ) %>%
   group_by( shotcolor, period, event_team ) %>%
   summarise( count=n() )
 foo_other <- shots_df %>% filter( shot != "BLOCK", strength != "EV 5v5", shotcolor %in% c( "BLUE", "GREEN") ) %>%
   group_by( shotcolor, period, event_team ) %>%
   summarise( count=n() )

 foo_all <- shots_df %>% filter( shot != "BLOCK", shotcolor %in% c( "BLUE", "GREEN") ) %>%
   group_by( shotcolor, period, event_team, ev5on5=strength=="EV 5v5" ) %>%
   summarise( count=n() )

 foo_ev$shotcolor <- factor( foo_ev$shotcolor, levels= c( "GREEN", "BLUE"))
 foo_other$shotcolor <- factor( foo_other$shotcolor, levels= c( "GREEN", "BLUE"))

 ggplot(foo_ev, aes(x=period,y=count)) +
   geom_bar(data=foo_ev %>% filter( event_team=="NJD"), stat = "identity", aes(fill=shotcolor)) +
   geom_bar(data=foo_ev %>% filter(event_team!="NJD"), aes(y=-count),  fill="firebrick2", stat = "identity") +
   geom_text(data=foo_ev %>% filter( event_team=="NJD"),aes(label=count), vjust=-0.1, size=3)+
   geom_text(data=foo_ev %>% filter( event_team!="NJD"),aes(y=-count,label=count), size=3,vjust=-0.5)+
   geom_hline(yintercept = 0) +
   scale_x_continuous("Period" ) +
   scale_fill_manual( values=c( "BLUE"="slateblue2", "GREEN"="springgreen4") ) +
   scale_y_continuous("Chances", breaks=seq(-20,20,2) ) +
   facet_grid( shotcolor ~., scales="free_y", switch="both" ) + theme_bw( ) + theme( legend.position="none") +
   ggtitle( "EV5on5 Chances")

 ggplot(foo_other, aes(x=period,y=count)) +
   geom_bar(data=foo_other %>% filter( event_team=="NJD"), stat = "identity", aes(fill=shotcolor), width=0.9) +
   geom_bar(data=foo_other %>% filter(event_team!="NJD"), aes(y=-count),  fill="firebrick2", stat = "identity", width=0.9) +
   geom_text(data=foo_other %>% filter( event_team=="NJD"),aes(label=count), vjust=-0.1,size=3)+
   geom_text(data=foo_other %>% filter( event_team!="NJD"),aes(y=-count,label=count), vjust=-0.5,size=3)+
   geom_hline(yintercept = 0) +
   scale_x_continuous("Period" ) +
   scale_fill_manual( values=c( "BLUE"="slateblue2", "GREEN"="springgreen4") ) +
   scale_y_continuous("Chances", breaks=seq(-20,20,2) ) +
   facet_grid( shotcolor ~., scales="free_y", switch="both" ) + theme_bw( ) + theme( legend.position="none") +
   ggtitle( "Non-EV5on5 Chances")





player_chances_vs_opp_ev  <- player_chances_vs_opp %>% filter( strength=="ev5on5" )
player_chances_vs_opp_all <- player_chances_vs_opp %>% filter( strength=="all" )
player_chances_vs_opp_pp  <- player_chances_vs_opp %>% filter( strength=="pp" )

player_stats <- player_chances_vs_opp_all %>% filter( metric=="fenwick" ) %>%  group_by( team_short, nhl_id, last_name ) %>% summarize(
  gm = n(),
  toi_total=sum(toi) %>% round(2),
  toi_gm = (toi_total/gm) %>% round(1)
) %>% left_join( player_tbl %>% select( nhl_id, position_fd, number ), by="nhl_id" ) %>%
  ungroup() %>% arrange( team_short, position_fd, desc(toi_gm) )

### EV
player_fenwick_ev <- player_chances_vs_opp_ev %>% filter( metric=="fenwick" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  toi_total_ev=sum(toi) %>% round(3),
  toi_gm_ev=(toi_total_ev/n() ) %>% round(1),
  ff=sum(scf), fa=sum(sca), f_net=sum(sc_net),
  ff_i_ev=sum(scf_i),
  ff_i_60_ev = (ff_i_ev/toi_total_ev*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total_ev )

player_green_ev <- player_chances_vs_opp_ev %>% filter( metric=="green" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  green_f=sum(scf), green_a=sum(sca), green_net=sum(sc_net),
  toi_total_ev=sum(toi) %>% round(2),
  green_i_ev=sum(scf_i),
  green_i_60_ev = (green_i_ev/toi_total_ev*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total_ev )

player_blue_ev <- player_chances_vs_opp_ev %>% filter( metric=="blue" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  blue_f=sum(scf), blue_a=sum(sca), blue_net=sum(sc_net),
  toi_total_ev=sum(toi) %>% round(2),
  blue_i_ev=sum(scf_i),
  blue_i_60_ev = (blue_i_ev/toi_total_ev*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total_ev )

player_greenblue_ev <- player_chances_vs_opp_ev %>% filter( metric=="greenblue" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  greenblue_f=sum(scf), greenblue_a=sum(sca), greenblue_net=sum(sc_net),
  toi_total_ev=sum(toi) %>% round(2),
  greenblue_i_ev=sum(scf_i),
  greenblue_i_60_ev = (greenblue_i_ev/toi_total_ev*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total_ev )

#### ALL
corsi_all <- player_chances_vs_opp_all %>% filter( metric=="corsi" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  toi_total=sum(toi) %>% round(2),
  toi_gm=toi_total/n(),
  cf=sum(scf), ca=sum(sca), c_net=sum(sc_net),
  cf_i=sum(scf_i),
  cf_i_60 = (cf_i/toi_total*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total )

player_fenwick_all <- player_chances_vs_opp_all %>% filter( metric=="fenwick" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  toi_total=sum(toi) %>% round(2),
  toi_gm=toi_total/n(),
  ff=sum(scf), fa=sum(sca), f_net=sum(sc_net),
  ff_i=sum(scf_i),
  ff_i_60 = (ff_i/toi_total*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total )

player_green_pp <- player_chances_vs_opp_pp %>% filter( metric=="green" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  green_f_pp=sum(scf), green_a_pp=sum(sca), green_net_pp=sum(sc_net),
  toi_total=sum(toi) %>% round(2),
  toi_gm_pp = toi_total/n(),
  green_i_pp=sum(scf_i),
  green_i_60 = (green_i_pp/toi_total*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total )

player_blue_pp <- player_chances_vs_opp_pp %>% filter( metric=="blue" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  blue_f_pp=sum(scf), blue_a_pp=sum(sca), blue_net_pp=sum(sc_net),
  toi_total=sum(toi) %>% round(2),
  blue_i_pp=sum(scf_i),
  blue_i_60 = (blue_i_pp/toi_total*60) %>% round(1)
) %>% ungroup() %>% select(-toi_total )

player_stats <- player_stats %>%
  left_join( player_fenwick_ev, by=c("nhl_id", "last_name") ) %>%
  left_join( player_green_ev, by=c("nhl_id", "last_name") ) %>%
  left_join( player_blue_ev, by=c("nhl_id", "last_name") )  %>%
  left_join( player_greenblue_ev, by=c("nhl_id", "last_name") ) %>%
  left_join( corsi_all %>% select( nhl_id, cf_i ), by=c("nhl_id" ) ) %>%
  left_join( player_green_pp %>% select( nhl_id, green_i_pp ), by=c("nhl_id" ) ) %>%
  left_join( player_blue_pp %>% select( nhl_id, blue_i_pp ), by=c("nhl_id" ) )

player_stats[ is.na(player_stats) ] <- 0

# player_stats <- player_stats %>% mutate(
#   green_i_other = green_i - green_i_ev,
#   blue_i_other  = blue_i - blue_i_ev
# )

# SORT PLAYERS IF ONLY ONE GAME
player_stats <- player_stats %>% left_join( roster_sorted %>% select( nhl_id, rank_toi_ev5on5_adj), by="nhl_id" ) %>%
                          arrange( rank_toi_ev5on5_adj )

player_stats_brief <- player_stats %>% select( team_short, last_name, number, position_fd,
                                               gm, toi_gm, toi_gm_ev,
                                               green_net, blue_net, greenblue_net,
                                               green_i_ev, green_i_pp, blue_i_ev, blue_i_pp, cf_i )

brief_col_names <- c( "Team", "Player", "Number", "Pos", "Games", "TOI/Gm", "TOI/Gm EV", "Green Net", "Blue Net", "Green or Blue Net",
                      "Green i EV", "Green i PP",
                      "Blue i EV", "Blue i PP",
                      "Shot Attempts Individual" )

player_stats_brief <- player_stats_brief %>% filter( position_fd != "G" )
names(player_stats_brief) <- brief_col_names

# write_csv( player_stats_brief, path=paste0(nhl_dir$shot, "/opponent/NJD_through_gm50.csv") )

write_csv( player_stats_brief, path=paste0(nhl_dir$shot, "/opponent/gm43_MIN.csv") )

player_green_pp %>% filter( last_name =="NJD" )

player_chances_vs_opp_ev %>% filter( metric=="corsi", last_name=="NJD" )
player_chances_vs_opp_ev %>% filter( metric=="fenwick", last_name=="NJD" )

player_chances_vs_opp_ev %>% filter( metric=="green", last_name=="NJD" )
player_chances_vs_opp_ev %>% filter( metric=="blue", last_name=="NJD" )

player_chances_vs_opp_all %>% filter( metric=="green", last_name=="NJD" )
player_chances_vs_opp_all %>% filter( metric=="blue", last_name=="NJD" )

























njd_stats_summary <- player_stats %>% select( team_short, last_name, number, position_fd,
  gm, toi_gm, toi_gm_ev,
  green_f, green_a, green_net,
  blue_f, blue_a, blue_net,
  greenblue_f, greenblue_a, greenblue_net,
  green_i_ev, green_i_pp, blue_i_ev, blue_i_pp, cf_i )

brief_col_names <- c( "Team", "Player", "Number", "Pos", "Games", "TOI/Gm", "TOI/Gm EV",
  "Green For", "Green Against", "Green Net",
  "Blue For", "Blue Against", "Blue Net",
  "Green or Blue For", "Green or Blue Against", "Green or Blue Net",
  "Green i EV", "Green i PP",
  "Blue i EV", "Blue i PP",
  "Shot Attempts Individual" )

njd_stats_summary <- njd_stats_summary %>% filter( position_fd != "G" ) %>% arrange( team_short, desc(position_fd), desc(toi_gm) )
names(njd_stats_summary) <- brief_col_names

write_csv( njd_stats_summary %>% filter( Team=="NJD" ), path=paste0(nhl_dir$shot, "/NJD_through_gm58.csv") )

### Compare 28 vs 39
severson_games <- njd_player_chances_df  %>% filter( last_name=="SEVERSON", metric=="corsi", strength=="all") %>% select( game_number ) %>% unlist()
severson_last_10_games <- severson_games %>% sort() %>% tail(10)

helgeson_games <- njd_player_chances_df  %>% filter( last_name=="HELGESON", metric=="corsi", strength=="all") %>% select( game_number ) %>% unlist()
helgeson_last_10_games <- helgeson_games %>% sort() %>% tail(10)

combined_last_10_games <- c( severson_last_10_games, helgeson_last_10_games ) %>% unique()
length( combined_last_10_games ) # 13 games.  so 7 overlap.

player_chances_vs_opp <- njd_player_chances_df %>% filter( game_number %in% combined_last_10_games )
write_csv( njd_stats_summary %>% filter( Team=="NJD" ), path=paste0(nhl_dir$shot, "/njd_28_vs_39.csv") )

combined_game_id4s <- player_chances_vs_opp %>% select(game_id4 ) %>% distinct() %>% unlist()

combind_gp <- game_player %>% filter( game_id4 %in% combined_game_id4s, filter_strength=="ev5on5", filter_score_diff=="all" )
my_gp <- combind_gp %>% group_by( last_name ) %>% summarize( gm=n(), toi_total=sum(toi), toi_gm=toi_total/gm)


