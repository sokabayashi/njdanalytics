library( njdanalytics )

nhl_db <- setup_nhl_db()
this_season     <- "20152016"
this_session_id <- "2"
our_team        <- "NJD"

players <- c(
  "Jiri Tlusty",
  "Tyler Kennedy",
  "Tuomo Ruutu"
  )

player_tbl <- tbl( nhl_db, "player" )
gp_tbl     <- tbl( nhl_db, "game_player" ) %>% filter(
  season %in% c( "20132014", "20142015", "20152016" ),
  session_id %in% c( "2", "3" ),
  filter_period == "all",
  filter_score_diff == "all",
  filter_strength %in% c( "all", "ev5on5", "pp", "sh" )
)

these_players <- player_tbl %>% filter( first_last_name %in% players ) %>% collect()

these_gp <- gp_tbl %>% filter( nhl_id %in% these_players$nhl_id ) %>% collect() %>%
  select( last_name, game_date, filter_strength, team_short, everything() )

these_gp$team_short <- factor( these_gp$team_short )
these_gp$last_name <- factor( these_gp$last_name, levels=c( "TLUSTY", "KENNEDY", "RUUTU") )

window_size <- 40
gp_ma <- these_gp %>% group_by( nhl_id, filter_strength ) %>%
  arrange(
    nhl_id, filter_strength, desc(game_date)
  ) %>%
mutate(
  gm_back = row_number(),
  gm_back_neg = -gm_back,
  pts_sum = rollsum( x=p,   window_size, align="left", fill=NA ),
  toi_sum = rollsum( x=toi, window_size, align="left", fill=NA ),
  cf_sum = rollsum( x=cf, window_size, align="left", fill=NA ),
  ca_sum = rollsum( x=ca, window_size, align="left", fill=NA ),
  cf_pct = cf_sum / (cf_sum+ca_sum),
  toi_gm = rollmean( x=toi, window_size, align="left", fill=NA ),
  pts_gm = rollmean( x=p, window_size, align="left", fill=NA )
)

gp_select <- gp_ma %>% select( last_name, team_short, gm_back, gm_back_neg, game_date, g,a,p,toi,
  toi_gm, pts_gm, pts_sum, toi_sum, cf_sum,ca_sum, cf_pct )

gp_all <- gp_select %>% filter( filter_strength=="all" )

p.ma <- ggplot( gp_all, aes(x=gm_back_neg, y=pts_gm, color=last_name ) )
p.ma + geom_line( size=2 ) +
  scale_y_continuous( ("Pts/Gm" ), breaks=seq(0,1,0.1), limits = c(0,0.8) ) +
  scale_x_continuous( "Games Back", breaks=seq(-200,0,10), limits=c(-100,0) ) +
  scale_color_few() +
  theme(legend.position="none" ) +
  # theme(legend.title=element_blank ) +
  ggtitle( "Points/Game - All Strength (40-game moving average)" )
ggsave( paste0(nhl_dir$fa, "/fig/tlusty_et_al_pts_gm.png" ), width=6, height=3.5 )

p.toi <- ggplot( gp_all, aes(x=gm_back_neg, y=toi_gm, color=last_name ) )
p.toi + geom_line( size=2 ) +
  scale_y_continuous( ("TOI/Gm" ), breaks=seq(0,20,2), limits = c(10,18) ) +
  scale_x_continuous( "Games Back", breaks=seq(-200,0,10), limits=c(-100,0) ) +
  scale_color_few() + theme(legend.title=element_blank()) +
  ggtitle( "TOI/Game - All Strength (40-game moving average)" )
ggsave( paste0(nhl_dir$fa, "/fig/tlusty_et_al_toi_gm.png" ), width=6, height=3.5 )

gp_ev5on5 <- gp_select %>% filter( filter_strength=="ev5on5" )

p.corsi <- ggplot( gp_ev5on5, aes(x=gm_back_neg, y=cf_pct, color=last_name ) )
p.corsi +
  geom_hline(yintercept=0.5, size=0.5, color="black") +
  geom_line( size=2 ) +
  scale_y_continuous( ("Corsi%" ), breaks=seq(0,1,0.02), label=percent ) +
  scale_x_continuous( "Games Back", breaks=seq(-200,0,10), limits=c(-100,0) ) +
  scale_color_few() + theme(legend.title=element_blank()) +
  ggtitle( "EV5on5 Corsi% (40-game rolling window)" )
ggsave( paste0(nhl_dir$fa, "/fig/tlusty_et_al_corsi.png" ), width=6, height=3.5 )

write_csv( gp_all, path=paste0(nhl_dir$fa, "/tlusty_kennedy_ruutu.csv"))
