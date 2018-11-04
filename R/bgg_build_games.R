build_games <- function(ids) {
  games <- tibble()
  for (i in ids) {
    games <- rbind(games, build_single_game(i))
    Sys.sleep(2)
  }
  return(games)
}
