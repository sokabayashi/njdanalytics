library( zoo )
library( dplyr )

na_vector <- c( 1, 2, NA, 3 )
window_size <- 3

# I want to get
# NA NA 2 NA

rollmean( na_vector, window_size )
rollapply(na_vector, window_size, function(x) mean(x, na.rm=T), fill=NA )
apply( data.frame(na_vector), 2, function(x) stats::filter(x,rep(1/3,3),sides=2) )

dummy_df <- data_frame( index=1:length(na_vector), a=na_vector )
dummy_ts <- ts( na_vector )
dummy_df %>% filter( !is.na(a) ) %>% rollmean( window_size, fill=NA )

dummy_df %>% filter( !is.na(a) ) %>% rollapply( window_size, mean, fill=NA )

dummy_ts  %>% rollapply( window_size, function(x) mean(x,na.rm=T), fill=NA )

