% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/playerstatsfn.R
\name{aggregate_player_stats}
\alias{aggregate_player_stats}
\title{Aggregate player stats}
\usage{
aggregate_player_stats(start_date = NULL, end_date = NULL,
  season = "20152016", subset_count = NULL, subset_head_tail = NULL,
  session_id = NULL, period = "all", strength = "all",
  score_diff = "all", team_only = FALSE, h2h_stats = FALSE,
  group_team = FALSE, home_away = "all", ignore_team_games = NULL,
  nhl_db = NULL, corsi_adj_factor = 0.15, fenwick_adj_factor = 0.1,
  shot_adj_factor = 0.05)
}
\arguments{
\item{start_date}{String of date in YYYY-MM-DD format. Default is NULL which is fine if specifying by season}

\item{end_date}{String of date in YYYY-MM-DD format. Default is NULL which is fine if specifying by season}

\item{season}{Season string e.g. "20152016"}

\item{subset_count}{Integer of games to use for subset.  Default is NULL.}

\item{subset_head_tail}{String "head" or "tail" for specifying which end of subset.  Default is NULL.}

\item{session_id}{String "2" regular, "3" playoffs.  Default is NULL.}

\item{period}{String for filtering, "1", "2", "3", "4", ... "1or2" also okay.}

\item{strength}{ev5on5, ev4on4, pp, sh, comp_en, own_en, other.  Not in game player: no_en}

\item{score_diff}{trail_1, trail_2, trail_3, tied, lead_1, lead_2, lead_3.  Not in game_player: leading, trailing. trail_2plus, lead_2plus.}

\item{team_only}{Default is FALSE. Don't care about individual players, only team.  Faster.}

\item{h2h_stats}{Default is FALSE.  Don't care about QoC QoT.  Much faster.}

\item{group_team}{Default is FALSE. If TRUE, a traded player will appear twice, e.g., 2015 Jagr on NJD and FLA with separate stats.}

\item{home_away}{Default is "all".}

\item{ignore_team_games}{Vector of team_shorts to exclude all games from, e.g., 2015 BUF. Default is NULL}

\item{nhl_db}{DB objection to avoid repeated connections during multiple calls}

\item{corsi_adj_factor}{Double.  Default is 0.25 for faceoff diff adj on CR.}
}
\value{
Data frame of player stats
}
\description{
Aggregate player stats
}

