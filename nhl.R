library(tidyverse)
library(rvest)
library(xml2)
library(janitor)
library(lubridate)

seasons <- 1998:2009
urls <- paste0("https://www.hockeydb.com/ihdb/draft/nhl", seasons, "e.html")

nhl_all <- dplyr::tibble()
for (i in seq_along(urls)) {

  season <- urls[i] %>%
    stringr::str_remove(".*nhl") %>%
    stringr::str_remove("e.*") %>%
    as.integer()

  url <- read_html(urls[i])

  nhl <- url %>%
    rvest::html_table() %>%
    .[[1]]

  col_names <- nhl[1, ] %>%
    purrr::pmap(~c(...)) %>%
    .[[1]] %>%
    unname()

  remove_round_rows <- c("Round", paste("Round", 1:100))

  nhl_all <- nhl_all %>%
    dplyr::bind_rows(
      nhl %>%
        purrr::set_names(col_names) %>%
        janitor::clean_names() %>%
        dplyr::filter(!(round %in% remove_round_rows)) %>%
        dplyr::filter(round != "") %>%
        dplyr::mutate_at(vars(gp:last_season), ~ifelse(. == "", 0, .)) %>%
        dplyr::mutate(last_season = stringr::str_remove(last_season, "-.*") %>%
                        as.integer(),
                      year_drafted = season,
                      years_played = last_season - year_drafted,
                      years_played = ifelse(years_played <= 0, 0, years_played))
    )

  Sys.sleep(3)
  print(i)

}

# readr::write_csv(nhl_all, "data/nhl_all.csv")

# urls_specific_players
#
# player_names <- nhl_all %>%
#   dplyr::mutate_at(vars(num, pts), ~as.numeric(.)) %>%
#   dplyr::arrange(desc(num), desc(pts)) %>%
#   dplyr::filter(pts > 149) %>%
#   .[1:100, ] %>%
#   dplyr::pull(player) %>%
#   paste0(., collapse = "|")

nhl_all <- read_csv("nhl/data/nhl_all.csv")
seasons <- 1998:2009
urls <- paste0("https://www.hockeydb.com/ihdb/draft/nhl", seasons, "e.html")
urls_specific_players <- c()
for (i in seq_along(urls)) {

  urls_specific_players <- urls[i] %>%
    xml2::read_html() %>%
    rvest::html_elements("a") %>%
    as.character() %>%
    .[stringr::str_detect(., pattern = "players")] %>%
    c(urls_specific_players, .)
  Sys.sleep(2)

}

cleaned_urls <- urls_specific_players %>%
  purrr::map_chr(
    ~ stringr::str_remove(., ".*href=") %>%
      stringr::str_remove("\\>.*") %>%
      stringr::str_remove('\\"') %>%
      paste0("https://www.hockeydb.com", .) %>%
      str_sub(end = -2)
  )

# readr::write_csv(data.frame(links = cleaned_urls), "data/cleaned_urls.csv")
cleaned_links <- readr::read_csv("data/cleaned_urls.csv")$links


#dont run it because it will delete all progress
p_table_all <- dplyr::tibble()


for (i in 1011:length(cleaned_links)) {
  player_page <- cleaned_links[i] %>%
    xml2::read_html()

  position <- player_page %>%
    rvest::html_node(".v1") %>%
    rvest::html_elements("div") %>%
    as.character() %>%
    stringr::str_extract("(Goalie|Forward|Defense|Left Wing|Right Wing|Center|Wing)")

  if(position != "Goalie") {

    p_table <- player_page %>%
      rvest::html_table() %>%
      .[[1]]
    col_names_player <- p_table %>%
      .[1, ] %>%
      purrr::pmap(~c(...)) %>%
      .[[1]] %>%
      unname() %>%
      {c(.[1:9], paste0(.[10:14], "_playoffs"))}
    colnames(p_table) <- col_names_player
    p_table <- p_table %>%
      .[2:(nrow(.)-1), ] %>%
      dplyr::mutate(`+/-` = ifelse(`+/-` == "", "--", `+/-`))

  } else {

    p_table <- player_page %>%
      rvest::html_table() %>%
      .[[1]]
    col_names_player <- p_table %>%
      .[1, ] %>%
      purrr::pmap(~c(...)) %>%
      .[[1]] %>%
      unname() %>%
      {c(.[1:16], paste0(.[17:19], "_playoffs"))}
    colnames(p_table) <- col_names_player
    p_table <- p_table %>%
      .[2:(nrow(.)-1), ]

  }

  player_name <- player_page %>%
    rvest::html_elements("h1") %>%
    as.character() %>%
    stringr::str_extract(">.*<") %>%
    stringr::str_sub(start = 2, end = -2)
  p_table_all <- p_table_all %>%
    dplyr::bind_rows(
      p_table %>%
        dplyr::mutate(player_name = player_name, 
                      position = position)
    )

  print(i)
  Sys.sleep(runif(n = 1, 3, 10))
  readr::write_csv(p_table_all,"data/individual_players_7.csv")

}

p_table_all = read_csv("data/individual_players.csv") %>% 
  dplyr::mutate_all(as.character)




