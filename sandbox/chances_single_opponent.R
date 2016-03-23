library( njdanalytics )

nhl_db <- setup_nhl_db()

# MANUALLY UPDATE
game_suffix <- 68

this_season     <- "20152016"
this_session_id <- "2"

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

# njd_games, shots_tbl, player_chances_df, njd_players_by_game, pair_chances_df
load( file=paste0( nhl_dir$shot, "/njd_through_", game_suffix, ".RData" ))

# vs a specific team ------------------------------------------------------

opponent <- "WSH"
games_vs_opp <- njd_games %>% filter( opp_team_short==opponent )

this_game_number <- games_vs_opp$game_number
player_chances_vs_opp <- player_chances_df %>% filter( game_number %in% games_vs_opp$game_number )

# or perhaps we know the exact game number (sometimes we exclude an odd game)
# this_game_number <- c( 67)

# or perhaps we want season to date NJD only
# player_chances_vs_opp <- njd_player_chances_df %>% filter( team_short=="NJD" )
toi_h2h_ev_gm       <- tbl( nhl_db, "game_h2h" ) %>% filter( season==this_season, game_id4 %in% games_vs_opp$game_id4,
  filter_score_diff=="all", filter_strength=="ev5on5") %>% collect()

this_game_id4 <- shots_tbl %>% filter( game_number %in% this_game_number ) %>%
  select( game_id4 ) %>% unlist(use.names = F) %>% unique()
player_chances_vs_opp <- player_chances_df %>% filter( game_number %in% c( this_game_number) )

# single game analysis only
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
roster_sorted  <- group_roster_by_lines( roster_skaters, toi_matrix_ev, strength="ev5on5" )


# meta data for chart
mandown_intervals_df <- get_mandown_intervals( shift_interval_df, game_info )
goals_df             <- get_goals_from_pbp( pbp_df, game_info )
game_info            <- supplement_game_info( game_info, our_team="NJD" )

# CHART
create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="GREEN", include_blocked=F )
create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="BLUE", include_blocked=F )
create_shotcolor_line_chart( shots_df, goals_df, mandown_intervals_df, game_info, shift_interval_df, ev5on5=F, shotcolor="GREENBLUE", include_blocked=F )

# chances by period
sc_by_team_ev <- shots_df %>% filter( shot != "BLOCK", strength=="EV 5v5", shotcolor %in% c( "BLUE", "GREEN") ) %>%
  group_by( shotcolor, period, event_team ) %>%
  summarise( count=n() )
sc_by_team_other <- shots_df %>% filter( shot != "BLOCK", strength != "EV 5v5", shotcolor %in% c( "BLUE", "GREEN") ) %>%
  group_by( shotcolor, period, event_team ) %>%
  summarise( count=n() )

sc_by_team_all <- shots_df %>% filter( shot != "BLOCK", shotcolor %in% c( "BLUE", "GREEN") ) %>%
  group_by( shotcolor, period, event_team, ev5on5=strength=="EV 5v5" ) %>%
  summarise( count=n() )

sc_by_team_ev$shotcolor    <- factor( sc_by_team_ev$shotcolor,    levels= c( "GREEN", "BLUE"))
sc_by_team_other$shotcolor <- factor( sc_by_team_other$shotcolor, levels= c( "GREEN", "BLUE"))

y_max_sc <- max( sc_by_team_ev$count, sc_by_team_other$count )+1 # give room for data label
y_min_sc <- min( -1*sc_by_team_ev$count, -1*sc_by_team_other$count )

label_size <- 5
ggplot(sc_by_team_ev, aes(x=period,y=count)) +
  geom_bar(data=sc_by_team_ev %>% filter( event_team=="NJD"), stat = "identity", aes(fill=shotcolor)) +
  geom_bar(data=sc_by_team_ev %>% filter(event_team!="NJD"), aes(y=-count),  fill="firebrick2", stat = "identity") +
  geom_text(data=sc_by_team_ev %>% filter( event_team=="NJD"),aes(label=count), vjust=-0.1, size=label_size)+
  geom_text(data=sc_by_team_ev %>% filter( event_team!="NJD"),aes(y=-count,label=count), size=label_size,vjust=-0.5)+
  geom_hline(yintercept = 0) +
  scale_x_continuous("Period" ) +
  scale_fill_manual( values=c( "BLUE"="slateblue2", "GREEN"="springgreen4") ) +
  scale_y_continuous("Chances", breaks=seq(-20,20,2), limits=c(y_min_sc, y_max_sc) ) +
  facet_grid( shotcolor ~., scales="free_y", switch="both" ) + theme_bw( ) + theme( legend.position="none") +
  ggtitle( "EV5on5 Chances")

ggplot(sc_by_team_other, aes(x=period,y=count)) +
  geom_bar(data=sc_by_team_other %>% filter( event_team=="NJD"), stat = "identity", aes(fill=shotcolor), width=0.9) +
  geom_bar(data=sc_by_team_other %>% filter(event_team!="NJD"), aes(y=-count),  fill="firebrick2", stat = "identity", width=0.9) +
  geom_text(data=sc_by_team_other %>% filter( event_team=="NJD"),aes(label=count), vjust=-0.1,size=label_size)+
  geom_text(data=sc_by_team_other %>% filter( event_team!="NJD"),aes(y=-count,label=count), vjust=-0.5,size=label_size)+
  geom_hline(yintercept = 0) +
  scale_x_continuous("Period" ) +
  scale_fill_manual( values=c( "BLUE"="slateblue2", "GREEN"="springgreen4") ) +
  scale_y_continuous("Chances", breaks=seq(-20,20,2), limits=c(y_min_sc, y_max_sc) ) +
  facet_grid( shotcolor ~., scales="free_y", switch="both" ) + theme_bw( ) + theme( legend.position="none") +
  ggtitle( "Non-EV5on5 Chances")


sc_by_team_ev    %>% group_by( shotcolor, event_team ) %>% summarize( count=sum(count) )
sc_by_team_other %>% group_by( shotcolor, event_team ) %>% summarize( count=sum(count) )

sc_by_team_all %>% group_by( event_team ) %>% summarize( count=sum(count) )



# csv for export -----------------------------------------------------

if( nrow(games_vs_opp) == 1 ) {
  num_centers <- 4
} else {
  num_centers <- 5
}
vs_opp_rosters_C      <- augment_rosters_C( vs_opp_rosters, vs_opp_pbp, player_tbl, center_fo_cutoff = num_centers  )
vs_opp_rosters_group  <- group_multigame_rosters_by_lines( vs_opp_rosters_C, toi_h2h_ev_gm )


# roster
roster_sorted <- vs_opp_rosters_group

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

player_green_all <- player_chances_vs_opp_all %>% filter( metric=="green" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  green_i_all=sum(scf_i)
) %>% ungroup()

player_blue_all <- player_chances_vs_opp_all %>% filter( metric=="blue" ) %>%  group_by( nhl_id, last_name ) %>% summarize(
  blue_i_all=sum(scf_i)
) %>% ungroup()

player_stats_retval <- player_stats %>%
  left_join( player_fenwick_ev, by=c("nhl_id", "last_name") ) %>%
  left_join( player_green_ev, by=c("nhl_id", "last_name") ) %>%
  left_join( player_blue_ev, by=c("nhl_id", "last_name") )  %>%
  left_join( player_greenblue_ev, by=c("nhl_id", "last_name") ) %>%
  left_join( corsi_all %>% select( nhl_id, cf_i ), by=c("nhl_id" ) ) %>%
  left_join( player_green_pp %>% select( nhl_id, green_i_pp ), by=c("nhl_id" ) ) %>%
  left_join( player_blue_pp %>% select( nhl_id, blue_i_pp ), by=c("nhl_id" ) ) %>%
  left_join( player_green_all %>% select( nhl_id, green_i_all ), by=c("nhl_id" ) ) %>%
  left_join( player_blue_all %>% select( nhl_id, blue_i_all ), by=c("nhl_id" ) )

player_stats_retval[ is.na(player_stats_retval) ] <- 0

# player_stats <- player_stats %>% mutate(
#   green_i_other = green_i - green_i_ev,
#   blue_i_other  = blue_i - blue_i_ev
# )

# SORT PLAYERS IF ONLY ONE GAME
player_stats_retval <- player_stats_retval %>% left_join( roster_sorted %>% select( nhl_id, rank_toi_ev5on5_adj), by="nhl_id" ) %>%
  arrange( rank_toi_ev5on5_adj )

player_stats_retval <- player_stats_retval %>% left_join( vs_opp_rosters_group %>% select( nhl_id, rank), by="nhl_id" ) %>%
  arrange( rank )

# player_stats_brief <- player_stats_retval %>% select( team_short, last_name, number, position_fd,
#   gm, toi_gm, toi_gm_ev,
#   green_net, blue_net, greenblue_net,
#   green_i_ev, green_i_pp, blue_i_ev, blue_i_pp, cf_i )
#
# brief_col_names <- c( "Team", "Player", "Number", "Pos", "Games", "TOI/Gm", "TOI/Gm EV", "Green Net", "Blue Net", "Green or Blue Net",
#   "Green i EV", "Green i PP",
#   "Blue i EV", "Blue i PP",
#   "Shot Attempts Individual" )

player_stats_brief <- player_stats_retval %>% select( team_short, last_name, number, position_fd,
  gm, toi_gm, toi_gm_ev,
  green_net, blue_net, greenblue_net,
  green_i_all, blue_i_all, cf_i )

brief_col_names <- c( "Team", "Player", "Number", "Pos", "Games", "TOI/Gm", "TOI/Gm EV", "Green Net", "Blue Net", "Green or Blue Net",
  "Green i",
  "Blue i",
  "Shot Attempts Individual" )

player_stats_brief <- player_stats_brief %>% filter( position_fd != "G" )
names(player_stats_brief) <- brief_col_names

# write_csv( player_stats_brief, path=paste0(nhl_dir$shot, "/opponent/NJD_through_gm50.csv") )

write_csv( player_stats_brief, path=paste0(nhl_dir$shot, "/opponent/", opponent, ".csv") )

# player_green_pp %>% filter( last_name =="NJD" )
player_chances_vs_opp_ev %>% filter( metric=="corsi", last_name=="NJD" )
player_chances_vs_opp_ev %>% filter( metric=="fenwick", last_name=="NJD" )
player_chances_vs_opp_ev %>% filter( metric=="greenblue", last_name=="NJD" )

player_chances_vs_opp_ev %>% filter( metric=="green", last_name=="NJD" )
player_chances_vs_opp_ev %>% filter( metric=="blue", last_name=="NJD" )

player_chances_vs_opp_all %>% filter( metric=="green", last_name=="NJD" )
player_chances_vs_opp_all %>% filter( metric=="blue", last_name=="NJD" )


sc_green_summary_ev <- player_chances_vs_opp_ev %>% filter( metric=="green", last_name=="NJD" ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net))
sc_blue_summary_ev  <- player_chances_vs_opp_ev %>% filter( metric=="blue", last_name=="NJD" ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net))

sc_green_summary_all <- player_chances_vs_opp_all %>% filter( metric=="green", last_name=="NJD" ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net))
sc_blue_summary_all <- player_chances_vs_opp_all %>% filter( metric=="blue", last_name=="NJD" ) %>% summarize( scf=sum(scf), sca=sum(sca), sc_net=sum(sc_net))


sc_green_summary_ev
sc_blue_summary_ev

sc_green_summary_all - sc_green_summary_ev
sc_blue_summary_all - sc_blue_summary_ev





















