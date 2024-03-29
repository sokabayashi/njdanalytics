#' njdanalytics: A package for working with NHL.com data.
#'
#' The njdanalytics package provides functions for working in the
#' NJ Devils Hockey Analytics deparment
#'
#' @details Details
"_PACKAGE"

.onAttach <- function( libname, pkgname ) {
  packageStartupMessage( "NJD Analytics package" )

  base_dir     <- "/media/driveb/projects/nhl"
  vault_dir    <- paste0( base_dir, "/Vault" )
  r_dir        <- paste0( vault_dir, "/R" )

  nhl_dir <- list(
    base     = base_dir,
    vault    = vault_dir,
    r        = r_dir,
    db       = paste0( r_dir,     "/db"        ),
    fa       = paste0( r_dir,     "/fa"        ),
    draft    = paste0( r_dir,     "/draft"     ),
    reports  = paste0( r_dir,     "/reports"   ),
    analysis = paste0( r_dir,     "/analysis"  ),
    study    = paste0( r_dir,     "/study"     ),
    shot     = paste0( base_dir,  "/data/shot" )
  )

  assign( "nhl_dir", nhl_dir, envir = .GlobalEnv )
}

#' Setup a src to our NHL postgreSQL database
#'
#' @return An src for connecting to the 'nhl' PostgreSQL database.
#' @export
setup_nhl_db <- function() {
  # assign( "nhl_db", src_postgres( dbname = "nhl", user = "postgres", password = "postgres" ), envir = .GlobalEnv )

  nhl_db <- src_postgres( dbname = "nhl", user = "postgres", password = "postgres" )

  # team_tbl  <<- tbl( nhl_db, "team" )
  # assign( "team_tbl", team_tbl, envir = .GlobalEnv )

  nhl_db
}



