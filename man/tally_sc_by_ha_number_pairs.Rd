% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scutils.R
\name{tally_sc_by_ha_number_pairs}
\alias{tally_sc_by_ha_number_pairs}
\title{Tally scoring chances by pairs of ha_number.}
\usage{
tally_sc_by_ha_number_pairs(shots_df_filter, roster, our_ha, their_ha)
}
\arguments{
\item{shots_df_filter}{Shots df already filtered for color and strength.}

\item{roster}{from stage_roster}

\item{our_ha}{H or A}

\item{their_ha}{H or A}
}
\value{
Data frame with ha_number_1, ha_number_2, scf, sca.
}
\description{
Perspective: when pairs are on same team, scf is from those players' perspective, regardless of our team or their team.
When different teams, the scf is from our_ha.
}

