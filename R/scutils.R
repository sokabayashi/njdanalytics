
# Scoring Chance functions ----------------------------------------------------------
# tools to deal with manually collected scoring chance data.


#' left join scf_i onto ha_number scoring chance df.  Append a row for team level, H and A.
#'
#' @param on_ice_sc_by_ha_number On ice scoring chance summary
#' @param shots_df_filter Filtered master shots df
#'
#' @return An expanded scoring chance df by ha_number, with scf_i added.
#' @export
#'
left_join_scf_i <- function( on_ice_sc_by_ha_number, shots_df_filter ) {
  scf_summary_i      <- shots_df_filter %>% group_by( shooter_ha_number ) %>% summarise( scf_i=n() )
  scf_summary_team   <- shots_df_filter %>% group_by( event_team_ha )     %>% summarise( scf_i=n() ) %>%
                                       rename( shooter_ha_number=event_team_ha )

  scf_combined <- bind_rows( scf_summary_i, scf_summary_team )

  on_ice_sc_by_ha_number %>% left_join( scf_combined, by = c( "ha_number"="shooter_ha_number" ) )
}


#' Tally scoring chances by ha_number
#'
#' @param shots_df_filter Shots df filtered for strength or shot color.
#'
#' @return data frame of ha_number, scf, sca, sc_net, sc_total, scf_i.
#' @export
#'
tally_sc_by_ha_number <- function( shots_df_filter ) {
  on_ice_sc_by_ha_number <- tally_for_against_by_ha_number( shots_df_filter )

  left_join_scf_i( on_ice_sc_by_ha_number, shots_df_filter )
}


#' Tally scoring chances by pairs of ha_number.
#'
#' Perspective is important: when pairs are on same team, scf is from those players' perspective, regardless of our team or their team.
#' When different teams, the scf is from our_ha.
#'
#' @param shots_df_filter
#' @param roster from stage_roster
#' @param our_ha H or A
#' @param their_ha H or A
#'
#' @return Data frame with ha_number_1, ha_number_2, scf, sca.
#' @export
#'
tally_sc_by_ha_number_pairs <- function( shots_df_filter, roster, our_ha, their_ha ) {

  goalies    <- roster %>% filter( position == "G" ) %>% select( ha_number ) %>% unlist()
  ha_numbers <- roster %>% filter( position != "G" ) %>% select( ha_number ) %>%
                           unlist(use.names = F) %>% sort() # sort is important

  # Sort ha_numbers in alphabetical order
  shots_df_filter <- shots_df_filter %>% mutate(
    ha_numbers_list = on_ice_ha_numbers %>% gsub( paste( goalies, collapse="|" ), "", . ) %>%
                                            str_extract_all( "(\\w+)" ) %>%
                                            llply( sort ),
    pair            = ha_numbers_list %>% laply( get_pairs_of_ha_numbers ) # one direction only
  )

  shots_pairs_unnested <- shots_df_filter %>% select( event_team_ha, pair ) %>% unnest()
  shots_pairs_table <- shots_pairs_unnested %>% group_by( pair ) %>% summarise(
    A = sum( event_team_ha=="A" ),
    H = sum( event_team_ha=="H" )
  )
  shots_pairs_table <- shots_pairs_table %>% ungroup() %>% separate( pair, c("ha_number_1", "ha_number_2" ) )

  # quick check
  # shots_df_filter %>% filter( event_team_ha=="A", grepl( "A02", on_ice_ha_numbers ), grepl( "A14", on_ice_ha_numbers ) ) %>%
  #       select( clock, event_team_ha, on_ice_ha_numbers )

  ## Switch SC perspective from H and A to For and Against.
  ## probably a better way to do this using standard evaluation to relate first character of team to column A or H
  ## but haven't figured that out yet... here is a tidyr way basically melt (gather) and reshape (spread).
  shots_pairs_m <- shots_pairs_table %>% gather( shot_ha, shot_count, -c(1,2) ) %>% arrange( ha_number_1, ha_number_2 )
  shots_pairs_m <- shots_pairs_m %>% mutate(
                          team_ha_1 = substr(ha_number_1, 1, 1 ),
                          team_ha_2 = substr(ha_number_2, 1, 1 ),
                          teamcomp  = ifelse( team_ha_1 == team_ha_2, "T", "C" ),
                          sc_perspective = ifelse( teamcomp=="T",
                                                   ifelse( shot_ha == team_ha_1, "scf", "sca" ),
                                                   ifelse( shot_ha == our_ha,    "scf", "sca" )
                          )
  )
  shots_pairs_group <- shots_pairs_m %>% select( ha_number_1, ha_number_2, sc_perspective, shot_count ) %>%
                                         spread( sc_perspective, shot_count ) %>% select(  ha_number_1, ha_number_2, scf, sca )

  shots_pairs_group
}






