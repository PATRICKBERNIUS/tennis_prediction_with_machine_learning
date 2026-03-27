# 0) (Optional) set working directory to where your scripts + CSV live
# setwd("C:/path/to/your/folder")

# 1) Load the Elo functions
source("elo.R")

# 2) Read your single combined CSV
matches_raw <- read.csv("full_matches_data.csv", stringsAsFactors = FALSE)

# 3) Build the matches table the Elo code expects
needed <- c("winner_name","loser_name","tourney_level","tourney_date","match_num")

# If your file is missing match_num, create it (stable within each date)
if (!("match_num" %in% names(matches_raw))) {
  matches_raw$match_num <- ave(matches_raw$tourney_date, matches_raw$tourney_date, FUN = seq_along)
}

stopifnot(all(needed %in% names(matches_raw)))

matches <- matches_raw[needed]
matches$tourney_date <- as.Date(as.character(matches$tourney_date), format = "%Y%m%d")
matches$match_num <- as.integer(matches$match_num)

matches <- matches[order(matches$tourney_date, matches$match_num), ]

# 4) Globals the functions rely on
firstDate <- as.Date("1900-01-01")
playersToElo <- new.env(hash = TRUE)
matchesCount <- new.env(hash = TRUE)

# 5) Run Elo + inspect results
computeElo()

top <- summaryPlayers()
head(top, 25)

# Example: plot (only works if these names exist in your data)
# plotGuys()
