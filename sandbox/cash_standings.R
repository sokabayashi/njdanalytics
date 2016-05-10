library( njdanalytics )

nhl_db <- setup_nhl_db()

filename <- paste0( nhl_dir$base, "/meeting/201603 board/cash 2016.xlsx" )

file.exists(filename)

# NHL + AHL 1-way
cash_2016 <- read_excel( filename )
names( cash_2016 )[3] <- "cash"
team_tbl <- tbl( nhl_db, "team") %>% collect()

standings <- get_season_standings( "20152016", team_tbl )

cash_standings <- cash_2016 %>% left_join( standings, by=c ("team_short" ) )


cash_standings <- cash_standings %>% select( Team, team_short, cash, pts, conference, division, made_playoffs ) %>%
                    arrange( conference, desc(pts), desc(made_playoffs ))
cash_standings <- cash_standings %>% mutate(
              playoff_label = ifelse( made_playoffs, "Playoffs", "Missed" )
)

stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, colour="red", geom=geom, size = 6, ...)
}

p.cash <- ggplot( cash_standings %>% filter(conference=="Eastern"), aes(x=playoff_label,y=cash))

p.cash +
  geom_point( position=position_jitter(width = 0, height=0), size=4, aes(color=made_playoffs)) +
  geom_text( aes(label=team_short ), nudge_x = 0.1, size=3 ) +
  stat_sum_single(mean, shape=17)+
  scale_x_discrete( NULL ) +
  scale_y_continuous( "Cash ($MM)" ) + theme( legend.position="none") +
  ggtitle( "2015-16 Eastern Conference Cash Spend")

write_csv( cash_standings, path=paste0( nhl_dir$base, "/meeting/201603 board/cash_standings.csv"))

cash_standings %>% group_by( conference, made_playoffs ) %>% summarize( cash=mean(cash))
