% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/h2hutils.R
\name{group_multigame_rosters_by_lines}
\alias{group_multigame_rosters_by_lines}
\title{Group rosters by lines from h2h TOI data}
\usage{
group_multigame_rosters_by_lines(rosters_C, toi_h2h_ev)
}
\arguments{
\item{toi_h2h_ev}{Data frame from game_h2h filtered for strength and score state}

\item{roster}{Augmented roster C via augment_rosters_C(). Faceoff count and shot added}
}
\value{
Data frame of roster sorted by lines and D pairs.
}
\description{
Group forward line & D pairs together within each team's roster.
Across games, this can get messy so we need to establish some rules:
 1. every player on a team appears exactly once
 2. Forward lines of 3 F, 3 pairs of D
 3. excess F and D are appended on in descending order of TOI all strength
}
\details{
rosters_C has Centers identified already with is_center=TRUE.  There will be as many
forward lines as there are Centers.
}

