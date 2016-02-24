library( njdanalytics )

get_conference_standings <- function( this_season_end, east_west ) {
  conf_tag <- "WES"
  if( east_west == "East" ) conf_tag <- "EAS"
  standings_tag <- sprintf( "#%s_standings", conf_tag )

  # standings_file <- sprintf( "%s/standings_%s_%s_%s.RData", rdata_dir, east_west, this_season_end-1, this_season_end )

  # if( !file.exists( standings_file ) ) {
    standings_url <- sprintf( "http://www.hockey-reference.com/leagues/NHL_%s.html", this_season_end )
    league_html   <- standings_url %>% read_html() %>% html_nodes( "#teams" )

    conf_html     <- standings_url %>% read_html() %>% html_nodes( standings_tag )
    conf_standings     <- conf_html[[1]] %>% html_table( header = T ) %>% na.omit()
    conf_standings[,1] <- conf_standings[,1] %>% gsub( "*", "", ., fixed=T)

    max_games <- conf_standings$GP %>% max()

    conf_standings_adj <- conf_standings %>% mutate(
      season_end = this_season_end,
      pts_gm     = PTS/GP,
      pts_adj    = ( (max_games - GP)*pts_gm + PTS ) %>% round(1)
    )

    conf_standings_adj %>% arrange( desc( pts_adj ) )

    # save( conf_standings, file = standings_file )

  # } else {
    # message( "Loading standings from RData file for ", this_season_end )
    # load( standings_file )

  # }

  conf_standings_adj
}


p.standings <- ggplot( conf_standings_adj )
p.standings + geom_dotplot(aes(x=pts_adj))


standings_url <- sprintf( "http://www.hockey-reference.com/leagues/NHL_%s.html", this_season_end )
league_html   <- standings_url %>% read_html() %>% html_nodes( "#teams" )
standings     <- league_html[[1]] %>% html_table( header = T ) %>% na.omit()
standings[,1] <- standings[,1] %>% gsub( "*", "", ., fixed=T)

max_games <- standings$GP %>% max()

standings_adj <- standings %>% mutate(
  season_end = this_season_end,
  pts_gm     = PTS/GP,
  pts_adj    = ( (max_games - GP)*pts_gm + PTS ) %>% round(1)
)

standings_adj %>% arrange( desc( pts_adj ) )

# save( conf_standings, file = standings_file )

# } else {
# message( "Loading standings from RData file for ", this_season_end )
# load( standings_file )

# }

conf_standings_adj

p.standings <- ggplot( standings_adj )
p.standings + geom_dotplot(aes(x=pts_adj))

