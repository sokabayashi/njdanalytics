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

# player_name <-
player_names <- c(
  "Andy Greene",
  "Damon Severson",
  "David Schlemko"

  # # LD
  # "Dan Hamhuis",
  # "Brian Campbell",
  # "Kris Russell",
  # "Kyle Quincey",
  # "John-Michael Liles",
  # "Matt Bartkowski",
  # "Kevin Connauton",
  # "Michal Jordan",
  # "Matthew Carle",
  # "Patrick Wiercioch",
  #
  # # RD
  # "Ben Lovejoy",
  # "Jason Demers",
  # "Luke Schenn",
  # "Justin Schultz",
  # "Korbinian Holzer",
  # "Eric Gryba",
  # "Yannick Weber",
  # "Matt Tennyson",
  # "Mark Fayne",
  # "Roman Polak"

  # # LW
  # "Loui Eriksson",
  # "Andrew Ladd",
  # "Mikkel Boedker",
  # "Darren Helm",
  # "Viktor Stalberg",
  # "Jason Chimera",
  # "Jamie McGinn",
  # "Matt Martin",
  # "Gabriel Bourque",
  # "Brandon Pirri",
  #
  # ## C
  # "David Backes",
  # "Vernon Fiddler",
  # "Shawn Matthias",
  # "Colton Sceviour",
  # "Riley Nash",
  # "Ryan White",
  # "Michael Latta",
  #
  # # RW
  # "Kyle Okposo",
  # "Troy Brouwer",
  # "Lee Stempniak",
  # "Teddy Purcell",
  # "Michael Grabner",
  # "Dale Weise",
  # "Patrick Eaves",
  # "Brett Connolly",
  # "Josh Jooris",
  # "Dustin Brown",
  # "Landon Ferraro",
  # "Ben Smith",
  # "Sam Gagner",
  # "David Perron"
)


for( player_name in player_names ) {
  player_info <- player_tbl %>% filter( first_last_name == player_name )

  filename_base <- paste0( profile_dir, "/", player_info$last_name, "_", player_info$first_name ) %>% tolower()
  filename_pdf <- paste0( filename_base, ".pdf" )
  render("profile_2017_linemates.Rmd", "pdf_document", output_file = filename_pdf)
}
