library( njdanalytics )

nhl_db <- setup_nhl_db()

this_season <- "20152016"
# this_season <- "20132014"
home_team_score <- tbl( nhl_db, "team_score" ) %>% filter( season==this_season, session_id=="2", ha=="H" ) %>%
                    arrange( game_date ) %>% collect()

home_team_score <- home_team_score %>% mutate(
                    game_month = paste( year(game_date), sprintf( "%02.f", month(game_date) ), sep="-" )
)

# not enough games finished in Feb 2016
# home_team_score$game_month[ home_team_score$game_month=="2016-02" ] <- "2016-01"

season_summary <- home_team_score %>% group_by( game_month ) %>%
  summarize(
    games = n(),
    ot    = sum( overtime=="OT" ),
    so    = sum( overtime=="SO" ),
    otso  = ot+so,
    otso_prop = otso / games,
    so_prop   = so/otso
  )

season_summary

sum( season_summary$games )
sum( season_summary$otso )
sum( season_summary$otso ) / sum( season_summary$games )
sum( season_summary$so )
sum( season_summary$so ) / sum( season_summary$otso )

g.so <- ggplot( season_summary,aes(x=game_month, y=so_prop, group=1 ) )
g.so + geom_line(size=1) + geom_point(size=2) +
  scale_x_discrete("") +
  scale_y_continuous("Shootout%", limits=c(0,0.6), label=percent) +
  ggtitle( "2015-2016: Proportion of OT games that go to Shootout" )

njd_games <- tbl( nhl_db, "team_score" ) %>% filter( season==this_season, session_id=="2", team_short=="NJD" ) %>%
          arrange( game_date ) %>% collect()

njd_games %>% group_by( overtime ) %>% summarise( w=sum(win_loss=="W"), l=sum(win_loss!="W"))
