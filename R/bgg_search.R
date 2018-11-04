#' Search for board games on BoardGameGeek
#'
#' \code {bgg_search} Returns the results of a search as a tibble.
#' @param query A string. This is the search query.
#' @param include_expansions Boolean. Whether to include board game expansions as part of your search. Set to TRUE by default.
#' @param exact Boolean. Whether the search results should match the query exactly. Set to FALSE by default.
#'
#'
#'
bgg_search <- function(query, include_expansions=TRUE, exact=FALSE)
{
  search_url <- build_bgg_url(type='search', query=query, exact=exact)
  response<-GET(search_url)
  search_text <- content(response)
  ids <- xml_integer(xml_find_all(search_text, xpath='//item/@id'))
  types <- xml_text(xml_find_all(search_text, xpath='//item/@type'))
  ids_types <- data.frame(ids, types)
  if(include_expansions) {
    temp <- ids_types %>% filter(types=='boardgame' | types=='boardgameexpansion')
  } else {
    temp <- ids_types %>% filter(types=='boardgame')
  }

  ids <- temp$ids
  return(build_games(ids))

}
