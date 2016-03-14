library(dplyr)
lottery_odds <- data_frame(
  finish = 30:17, # 30th last, 17th just missed playoffs
  p    = c( 0.2, 0.135, 0.115, 0.095, 0.085, 0.075, 0.065, 0.06, 0.05, 0.035, 0.030, 0.025, 0.020, 0.01 ) # new for 2016 draft
)

num_draws <- 100e3
results <- vector( "list", num_draws )
for( draw in 1:num_draws ) {
  # only first 3 picks are lotteried without replacement.
  top_3  <- sample( lottery_odds$finish, 3, prob=lottery_odds$p, replace = F )
  others <- lottery_odds$finish[ !lottery_odds$finish %in% top_3 ]
  results[[draw]] <- c( top_3, others )
}
results_df <- do.call( "rbind", results )

results_matrix <- apply( results_df, 2, function(x) {
  x <- factor(x, levels=17:30 ) # ensures that table() will produce a 0 for picks
  x %>% table() %>% prop.table()
} ) %>% as.matrix()
rownames( results_matrix ) <- lottery_odds$finish %>% rev()

results_matrix %>% round( 3 )

top_3 <- results_matrix[ ,1:3] %>% rowSums()
top_3

# expected pick
results_matrix %*% 1:14
