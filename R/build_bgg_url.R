#this function builds URLs to BGG for different types of requests such as thing, search, and collection. Internal use only.
build_bgg_url <- function(type, id=NULL, query=NULL, exact=FALSE, username=NULL, include_collection_expansions=TRUE) {
  start_url <- "https://www.boardgamegeek.com/xmlapi2/"

  if (type=='thing') {
    url <- paste0(start_url, 'thing?id=', id, "&stats=1")

  } else if(type=='search') {
    query <- gsub(" ", "+", query)
    url <- paste0(start_url, 'search?query=', query)
    if (exact) {
      url <- paste0(url, '&exact=1')
    }}
  else if (type=='collection') {
    url <- paste0(start_url, 'collection?username=', username)
    if(include_collection_expansions==FALSE){
      url <- paste0(url, '&excludesubtype=boardgameexpansion')
    }

  }
  return(url)
}
