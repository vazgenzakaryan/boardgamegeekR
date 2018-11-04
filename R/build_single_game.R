#this function builds a tibble containing details of a single game; internal use only.
build_single_game <- function(id) {
  url <- build_bgg_url(type='thing', id=id)
  response <- GET(url)
  text <- content(response)

  #Get common properties
  id <- xml_integer(xml_find_all(text, xpath="//item/@id"))
  name <- xml_text(xml_find_all(text, xpath="//name/@value"))[1]
  thingtype <- xml_text(xml_find_all(text, xpath="//item/@type"))
  description <- xml_text(xml_find_all(text, xpath="//description"))
  year_published <- xml_integer(xml_find_all(text, xpath="//yearpublished/@value"))[1]
  min_players <- xml_integer(xml_find_all(text, xpath="//minplayers/@value"))[1]
  max_players <- xml_integer(xml_find_all(text, xpath="//maxplayers/@value"))[1]
  playing_time <- xml_integer(xml_find_all(text, xpath="//playingtime/@value"))[1]
  min_play_time <- xml_integer(xml_find_all(text, xpath="//minplaytime/@value"))[1]
  max_play_time <- xml_integer(xml_find_all(text, xpath="//maxplaytime/@value"))[1]
  minimum_age <- xml_integer(xml_find_all(text, xpath="//minage/@value"))[1]

  #  Get statistics
  num_users_rated <- xml_integer(xml_find_all(text, xpath="//usersrated/@value"))[1]
  average <- xml_double(xml_find_all(text, xpath="//average/@value"))[1]
  bayes_average <- xml_double(xml_find_all(text, xpath="//bayesaverage/@value"))[1]

  #Get ranks: the rank_name=='boardgame' part forces all rpg or video options out because it forces integer(0)
  rank_name <- xml_text(xml_find_all(text, xpath="//rank/@name"))
  rank_value <- xml_text(xml_find_all(text, xpath="//rank/@value"))
  rank_df <- data.frame(rank_name, rank_value)
  rank_temp <- rank_df %>% filter(rank_name=='boardgame')%>% select(rank_value)
  rank <- as.integer(as.character(rank_temp$rank_value))

  #Get other properties same as above, rpg and video are forced out
  type <- xml_text(xml_find_all(text, xpath="//link/@type"))
  value <- xml_text(xml_find_all(text, xpath="//link/@value"))
  type_value <- data.frame(type, value)
  cat <- type_value %>% filter(type=='boardgamecategory') %>% select(value)
  categories <- paste0(cat$value, collapse="; ")
  mech <- type_value %>% filter(type=='boardgamemechanic') %>% select(value)
  mechanics <- paste0(mech$value, collapse='; ')
  fam <- type_value %>% filter(type=='boardgamefamily') %>% select(value)
  family <- paste0(fam$value, collapse='; ')
  exp <- type_value %>% filter(type=='boardgameexpansion') %>% select(value)
  expansions <- paste0(exp$value, collapse='; ')
  des  <- type_value %>% filter(type=='boardgamedesigner') %>% select(value)
  designers <- paste0(des$value, collapse='; ')
  art  <- type_value %>% filter(type=='boardgameartist') %>% select(value)
  artists <- paste0(art$value, collapse='; ')
  pub  <- type_value %>% filter(type=='boardgamepublisher') %>% select(value)
  publishers <- paste0(pub$value, collapse='; ')
  game <- tibble(id, name, thingtype, description, year_published, min_players, max_players,
                 playing_time, min_play_time, max_play_time, minimum_age, num_users_rated, average, bayes_average,
                 rank, categories, mechanics, family, expansions, designers, artists, publishers)
  return(game)
}
