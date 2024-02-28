# general purpose
library(tidyverse)
library(here)
library(fs)
library(sf)

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
    county_name = case_when(
      all_of_county ~ str_remove(text, 'All of '),
      within_county ~ str_remove(text, 'Within '),
      TRUE ~ NA_character_
    )
  ) |> 
  group_by(district) |> 
  fill(county_name, .direction = 'down') |> 
  ungroup() |> 
  mutate(district = as.integer(district))

whole_county <- txt |> 
  filter(all_of_county)

whole_town <- txt |> 
  filter(all_of & !all_of_county)

remainder <- txt |> 
  filter(!all_of & !within) |> 
  select(text, district, county_name, tracts:blocks)


# set up baf to patch into ----

county_lookup <- fips_2020 |> 
  filter(state == censable::match_fips(.env$state)) |> 
  mutate(
    county_fips = paste0(state, county),
    county_name = name,
    .keep = 'none'
  )

baf <- pl_get_baf(state, 'INCPLACE_CDP')[[1]]

mcd <- pl_get_baf(state, 'MCD')[[1]] |> 
  mutate(
    block_fips = BLOCKID,
    mcd_fips = paste0(str_sub(BLOCKID, 1, 2), COUNTYFP, COUSUBFP),
    .keep = 'none'
  )

cousub <- tt_county_subdivisions(state)
town_lookup <- cousub |>
  st_drop_geometry() |>
  select(town_fips = GEOID, town_name = NAMELSAD)

target <- baf |> 
  select(block_fips = BLOCKID) |> 
  mutate(
    state_fips = str_sub(block_fips, 1, 2),
    county_fips = str_sub(block_fips, 1, 5),
    tract_fips = str_sub(block_fips, 1, 11),
    district = NA_integer_
  )

# fill in whole counties ----

target <- whole_county |> 
  select(county_name, district) |> 
  left_join(county_lookup, by = 'county_name') |> 
  select(-county_name) |> 
  rows_patch(x = target, by = 'county_fips')

cli::cli_inform('whole counties account for {round(100 * sum(!is.na(target$district)) / nrow(target), 1)}% of blocks')

# fill in whole towns ----

whole_town_fips <- whole_town |> 
  mutate(
    town_name = str_remove(text, 'All of '),
    town_name = str_replace(town_name, 'Tuscarora Reservation', 'Tuscarora Nation Reservation'),
    town_name = str_replace(town_name, 'Prattsburg town', 'Prattsburgh town'),
    town_name = str_squish(town_name)
  ) |> 
  select(town_name, county_name, district) |> 
  left_join(county_lookup, by = 'county_name') |>
  select(-county_name) |>
  left_join(town_lookup |> mutate(county_fips = str_sub(town_fips, 1, 5)), by = c('town_name', 'county_fips'))

town_baf <- mcd |> 
  left_join(whole_town_fips, by = c('mcd_fips' = 'town_fips')) |>
  filter(!is.na(district)) |> 
  select(block_fips, district)

target <- target |> 
  rows_patch(town_baf, by = 'block_fips')

cli::cli_inform('towns increase to {round(100 * sum(!is.na(target$district)) / nrow(target), 1)}% of blocks')

# do the specified tracts and blocks ----

whole_tracts <- remainder |> 
  mutate(
    text = str_remove(text, 'Tracts: '),
    nchar_first_num = nchar(str_extract(text, '\\d+'))
  ) |> 
  filter(!tract, nchar_first_num == 6) |> 
  mutate(
    text = str_split(text, ' ')
  ) |> 
  unnest_longer(text)

whole_tract <- remainder |> 
  filter(tract, !blocks & !block) |> 
  mutate(
    text = str_remove(text, 'Tract: ')
  )

whole_tracts_all <- whole_tracts |> 
  bind_rows(whole_tract) |> 
  select(
    county_name, tract_fips = text, district
  ) |> 
  left_join(county_lookup, by = 'county_name') |> 
  select(-county_name) |> 
  mutate(
    tract_fips = paste0(county_fips, tract_fips)
  )


part_tracts <- remainder |> 
  mutate(
    nchar_first_num = nchar(str_extract(text, '\\d+'))
  ) |> 
  filter(tract | nchar_first_num == 4, blocks | block | nchar_first_num == 4) |> 
  mutate(
    tract_fips = str_extract(text, '\\d{6}'),
    text = str_remove(text, 'Tract: \\d{6} Blocks: '),
    text = str_remove(text, 'Tract: \\d{6} Block: ')
  )  |> 
  fill(tract_fips, .direction = 'down') |> 
  mutate(
    text = str_split(text, ' ')
  ) |> 
  unnest_longer(text) |> 
  mutate(
    text = str_replace(text, '-', ':')
  ) |> 
  rowwise() |> 
  mutate(text = list(eval(parse(text = text)))) |> 
  unnest_longer(text) |> 
  mutate(
    text = str_pad(text, 4, pad = '0')
  ) |> 
  select(
    county_name, tract_fips, block_fips = text, district
  ) |> 
  left_join(county_lookup, by = 'county_name') |> 
  select(-county_name) |> 
  mutate(
    tract_fips = paste0(county_fips, tract_fips),
    block_fips = paste0(tract_fips, block_fips)
  )

target <- target |> 
  rows_patch(whole_tracts_all, by = c('tract_fips')) |> 
  rows_patch(part_tracts, by = c('block_fips'))

cli::cli_inform('tracts increase to {round(100 * sum(!is.na(target$district)) / nrow(target), 1)}% of blocks')


lakes <- whole_town_fips |> 
  filter(is.na(town_fips)) |> 
  select(county_fips, district)

target <- target |> 
  rows_patch(lakes, by = 'county_fips')

cli::cli_inform('tracts increase to {round(100 * sum(!is.na(target$district)) / nrow(target), 1)}% of blocks')

# write out to file ----
out <- target |> 
  select(
    GEOID = block_fips, district
  ) 

out |> 
  write_csv(here('data/A09310.csv'))


validate <- FALSE
if (validate) {
  library(geomander)
  
  blks <- build_dec(geography = 'block', state = state)
  
  blks <- blks |> 
    left_join(out, by = 'GEOID')
  
  # check pops ----
  blks |> as_tibble() |> group_by(district) |> summarize(pop = sum(pop)) |> pull(pop) |> summary()
  
  # check contiguity ----
  adj <- adjacency(blks)
  cont <- check_contiguity(adj, blks$district)
  
  # two disconnected blocks w/ 0 pop and no neighbors
  blks |> 
    slice(which(cont$component > 1))
  
  blks$district[adj[[6196]]]
  blks$district[adj[[6197]]]
}

make_plots <- FALSE
if (make_plots) {
  library(ggredist)
  library(redist)
  library(alarmdata)
  
  blks <- blks |> 
    st_transform(3857)
  
  p <- blks |> 
    ggplot() +
    geom_district(aes(group = district)) +
    geom_district_text(aes(label = district, group = district)) +
    scale_fill_washington() +
    theme_map()
  
  ggsave(here('data/A09310.png'), p, width = 12, height = 12, dpi = 300)
  
  
  # fifty-states -----
  map <- alarm_50state_map(state)
  plans <- alarm_50state_plans(state)
  plans <- plans |> 
    alarm_add_plan(ref_plan = out |> rename(A09310 = district), map = map, name = 'A09310', calc_polsby = TRUE)
  
  # helper from 50states
  lbl_party <- function(x) {
    if_else(x == 0.5, "Even",
      paste0(if_else(x < 0.5, "R+", "D+"), scales::number(200 * abs(x - 0.5), 1))
    )
  }
  
  
  set.seed(123)
  redist.plot.distr_qtys(plans, e_dvs,
                         color_thresh = 0.5,
                         size = 0.04 - sqrt(8) / 250, alpha = 0.4
  ) +
    geom_hline(yintercept = 0.5, color = '#00000055', size = 0.5) +
    scale_y_continuous('Two-party vote margin', labels = lbl_party) +
    labs(x = 'Simulated districts, ordered by Democratic vote margin') +
    annotate('text',
             x = 1.5, y = min(plans$e_dvs[seq_len(26)]),
             label = 'A09310', hjust = 0.05, size = 3.5,
             color = '#A09310'
    ) +
    annotate('text',
             x = 3.5, y = sort(plans$e_dvs[27:52])[3],
             label = 'CD 2020', hjust = -0.05, size = 3.5,
             color = 'black'
    ) +
    scale_color_manual(values = c('#A09310', 'black', ggredist$partisan[2], ggredist$partisan[14]),
                       labels = c('pt', 'Rep.', 'Dem.'), guide = 'none') +
    theme_bw()
  
  ggsave(here('data/A09310_e_dvs.png'), width = 10, height = 6, dpi = 300)
}

