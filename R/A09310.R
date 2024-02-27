# general purpose
library(tidyverse)
library(here)
library(fs)

# html processing
library(rvest)

# ALARM Project tools
library(PL94171)
library(tinytiger)
library(censable)


state <- 'NY'

# download html and read into txt
if (!file_exists(here('data-raw/A09310.txt'))) {
  download.file(
    'https://nyassembly.gov/leg/?default_fld=&leg_video=&bn=A09310&term=2023&Text=Y.html',
    here('data-raw/A09310.html') 
  )
  read_html(here('data-raw/A09310.html')) |> 
    html_nodes('pre') |> 
    html_text() |> 
    write_lines(here('data-raw/A09310.txt'))
}

txt <- read_lines(here('data-raw/A09310.txt')) |> 
  str_squish() |> 
  discard(\(x) str_detect(x, '^$')) |> 
  keep(\(x) str_detect(x, '^\\d+')) |> 
  str_remove('^\\d+\\s+') |> 
  tibble(text = _) |> 
  mutate(
    district = str_detect(text, 'Congressional District'),
    district = ifelse(district, str_extract(text, '^\\d+'), NA_character_)
    ) |> 
  fill(district, .direction = 'down') |> 
  filter(!is.na(district))

# remove duplicated bill text (appears twice)
txt <- txt |> 
  slice(1:which(str_detect(txt$text, 'Tract: 022717 Blocks: 1000-1008 2000-2016 3000-3015'))[1])

# add some useful indicators
txt <- txt |> 
  filter(!str_detect(text, 'Congressional District')) |> 
  mutate(
    all_of = str_detect(text, '^All of'),
    all_of_county = all_of & str_detect(text, 'County'),
    within = str_detect(text, '^Within'),
    within_county = within & str_detect(text, 'County'),
    tracts = str_detect(text, 'Tracts: '),
    tract = str_detect(text, '^Tract: '),
    block = str_detect(text, 'Block: '),
    blocks = str_detect(text, 'Blocks: '),
    county = case_when(
      all_of_county ~ str_remove(text, 'All of '),
      within_county ~ str_remove(text, 'Within '),
      TRUE ~ NA_character_
    )
  ) |> 
  group_by(district) |> 
  fill(county, .direction = 'down') |> 
  ungroup()

whole_county <- txt |> 
  filter(all_of_county)

whole_town <- txt |> 
  filter(all_of & !all_of_county)

remainder <- txt |> 
  filter(!all_of & !within) |> 
  select(text, district, county, tracts:blocks)



# set up baf to patch into ----

county_lookup <- fips_2020 |> 
  filter(state == censable::match_fips(.env$state)) |> 
  mutate(
    county_fips = paste0(state, county),
    county_name = name,
    .keep = 'none'
  )

baf <- pl_get_baf(state, 'INCPLACE_CDP')[[1]]

places <- tt_places(state)
town_lookup <- places |> 
  sf::st_drop_geometry() |> 
  select(town_fips = GEOID, town_name = NAME)

target <- baf

