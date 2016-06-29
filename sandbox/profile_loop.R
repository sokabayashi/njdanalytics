library( njdanalytics )

# Set working directory
nhl_db <- setup_nhl_db()

sandbox_dir <- paste0( nhl_dir$base, "/packages/njdanalytics/sandbox" )
profile_dir <- paste0( sandbox_dir, "/player_profile" )

setwd( sandbox_dir )

# Load packages
require(knitr)
require(markdown)
require(rmarkdown)

# Create .md, .html, and .pdf files
# knit("profile_template.Rmd")
# markdownToHTML('profile_template.md', 'this_player.html', options=c("use_xhml"))
# system("pandoc -s this_player.html -o this_player.pdf")

player_tbl  <- tbl( nhl_db, "player" ) %>% collect()

# player_name <- "Damon Severson"
player_names <- c(
  # "Andy Greene",
  # "David Schlemko",
  #
  # "Brian Campbell",
  # "Alex Goligoski",
  # "Keith Yandle",
  # "Justin Schultz",
  #
  # "Jason Demers",
  # "Michal Jordan",
  # "John-Michael Liles",
  # "Kris Russell",
  # "Matt Tennyson",
  #
  # "Matthew Carle",
  # "Eric Gryba",
  # "Ben Lovejoy",
  # "Kyle Quincey",
  # "Luke Schenn",
  # "Patrick Wiercioch"
  # "Jason Chimera",
  # "Patrick Eaves",
  # "Michael Grabner",
  "Trevor Lewis",
  "Matt Martin"
  # "Mike Santorelli",
  # "Chris Stewart",
  # "Dale Weise",
  # "Matt Cullen"
  # "Dominic Moore",
  # "Riley Nash",
  # "Ben Smith",
  # "Kyle Clifford",
  # "Dwight King",
  # "Jordan Nolan",
  # "Ryan Reaves",
  # "Ryan White",
  # "Vernon Fiddler"
)


for( player_name in player_names ) {
  player_info <- player_tbl %>% filter( first_last_name == player_name )

  filename_base <- paste0( profile_dir, "/", player_info$last_name, "_", player_info$first_name ) %>% tolower()
  filename_pdf <- paste0( filename_base, ".pdf" )
  render("profile_2017.Rmd", "pdf_document", output_file = filename_pdf)
}
