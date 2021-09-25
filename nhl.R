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

player_names <- nhl_all %>%
  dplyr::mutate_at(vars(num, pts), ~as.numeric(.)) %>%
  dplyr::arrange(desc(num), desc(pts)) %>%
  dplyr::filter(pts > 149) %>%
  .[1:100, ] %>%
  dplyr::pull(player) %>%
  paste0(., collapse = "|")




urls_specific_players <- url %>%
  rvest::html_elements("a") %>%
  as.character() %>%
  .[stringr::str_detect(., pattern = "players")] %>%
  .[stringr::str_detect(., pattern = player_names)]

urls_specific_players[1] %>%
  stringr::str_remove(".*href=") %>%
  stringr::str_remove("\\>.*") %>%
  stringr::str_remove('\\"') %>%
  paste0("https://www.hockeydb.com", .) %>%
  str_sub(end = -2) %>%
  read_html() %>%
  rvest::html_table()
















