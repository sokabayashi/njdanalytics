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
  "G",
  "A",
  "P",
  "plus_minus",
  "PEN",
  "PIM",
  "TOI",
  "shifts",
  "avg TOI",
  "TOI PP",
  "TOI SH",
  "TOI EV",
  "SOG",
  "A_B",
  "MS",
  "HT",
  "GV",
  "TK",
  "BS",
  "FW",
  "FL",
  "F_pct"
)

table_clean <- summary_table %>% slice(-1:-2) %>% filter( !is.na(position), position != "SHF"  )
table_converted <- table_clean %>% mutate(
TOI = time_mmss_to_decimal( TOI ),
  `avg TOI` = time_mmss_to_decimal( `avg TOI` ),
  `TOI PP` = time_mmss_to_decimal( `TOI PP` ),
  `TOI SH` = time_mmss_to_decimal( `TOI SH` ),
  `TOI EV` = time_mmss_to_decimal( `TOI EV` )
) %>% mutate_each( funs(str_trim), everything() )



write_csv( table_converted, path="sandbox/ES020022.csv" )
