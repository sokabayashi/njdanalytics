% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h2hutils.R
\name{augment_roster}
\alias{augment_roster}
\title{Augment roster data frame}
\usage{
augment_roster(roster, pbp_df, player)
}
\arguments{
\item{roster}{Roster, as pulled from stage_roster.}

\item{pbp_df}{Play-by-play data, as pulled from stage_playbyplay tbl.}

\item{player}{Player data from player tbl.}
}
\value{
Data frame of roster with additional columns
}
\description{
Add faceoff count and shot (L/R) to roster
}

