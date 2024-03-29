% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h2hutils.R
\name{group_roster_by_lines_from_h2h}
\alias{group_roster_by_lines_from_h2h}
\title{Group roster by lines from h2h TOI data}
\usage{
group_roster_by_lines_from_h2h(roster, toi_h2h_ev)
}
\arguments{
\item{roster}{Augmented roster with faceoff count and shot added.}

\item{toi_h2h_ev}{Data frame from game_h2h filtered for strength and score state}
}
\value{
Data frame of roster sorted by lines and D pairs.
}
\description{
Group forward line & D pairs together within each team's roster
}

