
# Game Data functions ----------------------------------------------------------
#

#'  Get data frame of goals from playbyplay for a single game.
#'  Handles SO and OT goal cases.
#'  For SO goal, grabs arbitrary goal from winning team.
#'
#' @param pbp_df A data frame of playbyplay
#' @param game_info A data frame except of a single game from stage_game
#' @return Data frame of goals with time as start_cum
#' @export
get_goals_from_pbp <- function( pbp_df, game_info ) {
  goals_df <- pbp_df %>% filter( event_type == "GOAL" )

  winning_team_short <- with( game_info, ifelse( home_score_final > away_score_final, home_team_short, away_team_short ) )

  ## Shootout case: Filter out all SO goals except last one
  if( (game_info$session_id == "1" || game_info$session_id == "2") && max(pbp_df$period) == 5 ) {
    # SO scores keep incrementing and the way the data is written to db, we *don't* know SO winner (oh no)
    ot_score  <- pbp_df %>% filter( event_type == "PEND", period == 4 ) # grab pre-SO score
    so_goals  <- goals_df %>% filter( period == 5 )
    # arbitrarily grab first goal by winning team
    so_winner <- so_goals %>% filter( event_team == winning_team_short ) %>% head(1)

    so_winner$away_score <- ot_score$away_score
    so_winner$home_score <- ot_score$home_score

    goals_df <- goals_df %>% filter( period < 5 )
    goals_df <- bind_rows( goals_df, so_winner )
  }

  goals_df
}


#'  Count for and against stats by player (ha_number) for a
#'  single game.
#'
#' @param pbp A data frame of playbyplay or shots
#' @param output_col_names A vector of names for output table
#' @param reduce_cols Return just results.  True by default
#' @return data frame with columns ha_number, for_col, away_col
#' @export
tally_for_against_by_ha_number <- function( pbp,
  output_col_names = c( "scf", "sca", "sc_net", "sc_total" ),
  reduce_cols = TRUE ) {
  # Error cases first
  if( !nrow( pbp ) ) {
    retval <- data_frame( ha_number=c( "H", "A") )
    retval[ , output_col_names ] <- 0

    return( retval )
  }

  event_ha_col <- unlist( pbp$event_team_ha )
  if( !all( unique( event_ha_col ) %in% c( "H", "A")) ) {
    # Uh oh. we've got some weird events where PBP did not specify a team and nhlscrapr did not grab a team name or H/A.
    bad_events <- pbp %>% filter( !event_team_ha %in% c( "H", "A" ) )

    message( "Removing non-standard event in pbp: ", bad_events$etext )
    pbp <- pbp %>% filter( event_team_ha %in% c( "H", "A" ) )
    event_ha_col <- unlist( pbp$event_team_ha )

    if( !nrow( pbp ) ) {
      retval <- data_frame( ha_number=c( "H", "A") )
      retval[ , output_col_names ] <- 0

      return( retval )
    }
  }

  # let's go
  on_ice_ha_numbers <- paste( "H", "A", pbp$on_ice_ha_numbers, pbp$away_goalie, pbp$home_goalie )
  on_ice_ha_numbers <- sapply( on_ice_ha_numbers, function( my_cell) gsub( " NA|NA ", "", my_cell ) )

  event_df <- data_frame( event_ha=event_ha_col, ha_number=on_ice_ha_numbers )
  event_df <- event_df %>% mutate( ha_number=strsplit( ha_number, " " ) ) %>% unnest()
  ha_table <- event_df %>% group_by( event_ha, ha_number ) %>% dplyr::summarize( count=n() )
  ha_table <- ha_table %>% spread( event_ha, count, fill=0 )

  # if only H or A is represented, we will only get an H or A column but not both.  we want both columns.
  if( !"A" %in% event_ha_col ) {
    ha_table$A <- 0
  } else if( !"H" %in% event_ha_col ) {
    ha_table$H <- 0
  }

  # for and against cols are relative to player's team.  H goal is GF for H player, GA for A player
  ha_table <- ha_table %>% filter( nchar(ha_number) > 0 ) %>%
    mutate( team_ha = substr( ha_number, 1, 1 ) )

  ha_table[ output_col_names[1] ] <- ifelse( ha_table$team_ha == "H", ha_table$H, ha_table$A )
  ha_table[ output_col_names[2] ] <- ifelse( ha_table$team_ha == "A", ha_table$H, ha_table$A )
  ha_table[ output_col_names[3] ] <- ha_table[ output_col_names[1] ] - ha_table[ output_col_names[2] ]
  ha_table[ output_col_names[4] ] <- ha_table[ output_col_names[1] ] + ha_table[ output_col_names[2] ]

  if( reduce_cols ) ha_table <- ha_table[ , c( "ha_number", output_col_names ) ]

  ha_table
}



#'  Add our/their perspective fields to game_info
#'
#' @param game_info A single row from stage_game
#' @param our_team 3-letter team_short format for default perspective
#' @return game_info data frame with additional fields of our_ha, row_ha, col_ha, our_team, their_team
#' @export
supplement_game_info <- function( game_info, our_team="NJD" ) {

  our_ha   <- "H"; row_ha   <- "H"; col_ha   <- "A"
  if( our_team %in% c( game_info$home_team_short, game_info$away_team_short ) ) {
    # an NJD game
    if( our_team == game_info$home_team_short ) {
      our_ha <- "H"
      their_team <- game_info$away_team_short
    } else {
      their_team <- game_info$home_team_short
      our_ha <- "A"; row_ha  <- "A"; col_ha <- "H"
    }
  } else {
    # non-NJD game, set default (row) perspective to Home team
    our_team   <- game_info$home_team_short
    their_team <- game_info$away_team_short
  }

  game_info <- game_info %>% mutate(
    our_ha     = our_ha,
    their_ha   = ifelse( our_ha=="H", "A", "H" ),
    row_ha     = row_ha,
    col_ha     = col_ha,
    our_team   = our_team,
    their_team = their_team
  )

  game_info
}


#' Add a game label to team_score, good for x-axis.
#'
#' @param team_score_subset Excerpt of team_score, filtered already for a team
#' @param our_team 3-letter team_short format for default perspective
#' @return game_info data frame with additional fields of our_ha, row_ha, col_ha, our_team, their_team
#' @export
add_team_score_label <- function( team_score_subset, our_team="NJD" ) {

  team_score_retval <- team_score_subset %>% mutate(
    game_num_date = paste0( "G", game_number, " ", win_loss, "\n", format( game_date, "%m/%d") ),
    game_label    = ifelse( ha=="H", paste0( game_num_date, " ",  opp_team_short, "\n",
                                             ga, "-", gf ),
                                     paste0( game_num_date, " @", opp_team_short, "\n",
                                             gf, "-", ga ) ),
    game_label    = ifelse( overtime=="", game_label, paste0( game_label, " (", overtime, ")" ) )
  )

  # Make a factor so that G10, G11 don't comre before G1, G2
  game_label_factor_levels <- team_score_retval %>% select( game_date, game_label ) %>% arrange( game_date ) %>%
    select( game_label ) %>% unlist(use.names = F)

  team_score_retval$game_label <- factor( team_score_retval$game_label, levels=game_label_factor_levels )

  team_score_retval %>% select(-game_num_date )
}


#' Get every pair combination from a vector of players.
#'
#' @param ha_numbers Vector of ha_number, already sorted alphabetically
#' @return list of every pairwise combination
#' @export
get_pairs_of_ha_numbers <- function( ha_numbers ) {
  ha_numbers %>% combn(2, simplify=F) %>% laply(paste, collapse=" " ) %>% list()
}




