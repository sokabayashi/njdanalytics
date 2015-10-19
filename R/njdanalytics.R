#' njdanalytics: A package for working with data scraped from NHL.com
#'
#' The njdanalytics package provides functions to work in the
#' NJ Devils Hockey Analytics deparment
#'
#'  @name njdanalytics
NULL

.onAttach <- function( libname, pkgname ) {
  # packageStartupMessage( "" )
  base_dir     <- "/media/driveb/projects/nhl"
  vault_dir    <- paste0( base_dir,  "/Vault"    )
  r_dir        <- paste0( vault_dir, "/R"        )
  db_dir       <- paste0( r_dir,     "/db"       )
  fa_dir       <- paste0( r_dir,     "/fa"       )
  draft_dir    <- paste0( r_dir,     "/draft"    )
  report_dir   <- paste0( r_dir,     "/report"   )
  analysis_dir <- paste0( r_dir,     "/analysis" )
  study_dir    <- paste0( r_dir,     "/study"    )

  assign( "base_dir",     base_dir,      envir = .GlobalEnv )
  assign( "vault_dir",    vault_dir,     envir = .GlobalEnv )
  assign( "r_dir",        r_dir,         envir = .GlobalEnv )
  assign( "db_dir",       db_dir,        envir = .GlobalEnv )
  assign( "fa_dir",       fa_dir,        envir = .GlobalEnv )
  assign( "draft_dir",    draft_dir,     envir = .GlobalEnv )
  assign( "report_dir",   report_dir,    envir = .GlobalEnv )
  assign( "analysis_dir", analysis_dir,  envir = .GlobalEnv )
  assign( "study_dir",    study_dir,     envir = .GlobalEnv )
}

#' Setup a src to our NHL postgreSQL database
#'
#' @export
setup_nhl_db <- function() {
  # assign( "nhl_db", src_postgres( dbname = "nhl", user = "postgres", password = "postgres" ), envir = .GlobalEnv )

  nhl_db <- src_postgres( dbname = "nhl", user = "postgres", password = "postgres" )

  team_tbl  <<- tbl( nhl_db, "team" )
  assign( "team_tbl", team_tbl, envir = .GlobalEnv )

  nhl_db
}



