#' Games in a user's collection by username.
#'
#' \code{bgg_collection} Returns all games in a user's collection, as a tibble.
#'
#' @param username String. BoardGameGeek user's username.
#' @param include_collection_expansions Boolean. Whether or not to include expansions as part of the user's collection. Set to TRUE by default.



bgg_collection <- function(username, include_collection_expansions=TRUE){
  collection_url <- build_bgg_url(type='collection', username=username, include_collection_expansions=include_collection_expansions)
  response <- GET(collection_url)
  collection_text <- content(response)
  ids <- xml_integer(xml_find_all(collection_text, xpath='//item/@objectid'))
  return(build_games(ids))
}










