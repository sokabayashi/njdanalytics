library( njdanalytics )
con    <- dbConnect( dbDriver( "PostgreSQL"), dbname="nhl" )

njdanalytics_dir <- paste0( nhl_dir$base, "/packages/njdanalytics" )
sandbox_dir      <- paste0( njdanalytics_dir, "/sandbox" )

cols_player_season_file <- paste0( sandbox_dir, "/player_season_cols.csv" )
cols_player_season <- read_csv( cols_player_season_file )
cols_player_season <- data.frame( cols_player_season, row.names=NULL )


cols_player_comparison_file <- paste0( sandbox_dir, "/player_comparison_cols.csv" )
cols_player_comparison <- read_csv( cols_player_comparison_file )
cols_player_comparison <- data.frame( cols_player_comparison, row.names=NULL )


# write to db -------------------------------------------------------------


dbWriteTable( con, "cols_player_season",     cols_player_season,     append=F, row.names=F )
dbWriteTable( con, "cols_player_comparison", cols_player_comparison, append=F, row.names=T ) # hmm rownames is number
