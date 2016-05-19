library( njdanalytics )

season <- "20152016"
session_id <- "2"
game_id4 <- "0012"

es_url <- "http://www.nhl.com/scores/htmlreports/20152016/ES020022.HTM"

es_html <- es_url %>% read_html()
es_tables <- es_html %>% html_nodes( "table" )
es_tables <- es_tables %>% html_table(fill=T)
summary_table <- es_tables[[19]]

names( summary_table ) <- c(
  "jersey_number",
  "position",
  "player",
  "g",
  "a",
  "p",
  "plus_minus",
  "pen",
  "pim",
  "toi",
  "shifts",
  "toi_avg",
  "toi_pp",
  "toi_sh",
  "toi_ev",
  "sog",
  "a_b",
  "ms",
  "ht",
  "gv",
  "tk",
  "bs",
  "fw",
  "fl",
  "f_pct"
)

table_clean <- summary_table %>% slice(-1:-2) %>% filter( !is.na(position), position != "SHF"  ) %>%
  mutate_each( funs(str_trim), everything() ) %>% as_data_frame()

table_taran <- table_clean %>% select(
  player, position, jersey_number,
  toi, toi_pp, toi_sh, sog ) %>%
  separate( toi, c( "ttoim", "ttois"), sep=":" ) %>%
  separate( toi_pp, c( "pptoim", "pptois"), sep=":" ) %>%
  separate( toi_sh, c( "pktoim", "pktois"), sep=":" )

write_csv( table_taran, path="sandbox/ES020022.csv" )
