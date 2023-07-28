#' ---
#' output: github_document
#' ---

library(gganimate)
library(magick)
source(here("map-theme.R"))
source(here("shared-objects.R"))

states_to_map <- census_regions$south_central

hh <- quo(hhi_net)
ii <- quo(cz_id) 
hhi_chg_cz <- 
  hhi_cz %>% 
  select(!!ii, year,!!hh ) %>% 
  mutate(year = paste0("hhi_",year)) %>% 
  spread(year,!!hh) %>% 
  mutate(hhi_2016 = coalesce(hhi_2015,hhi_2016)) %>% 
  mutate(chg_2010 = hhi_2010 - hhi_2010,
         chg_2011 = hhi_2011 - hhi_2010,
         chg_2012 = hhi_2012 - hhi_2010,
         chg_2013 = hhi_2013 - hhi_2010,
         chg_2014 = hhi_2014 - hhi_2010,
         chg_2015 = hhi_2015 - hhi_2010,
         chg_2016 = hhi_2016 - hhi_2010,
         chg_2017 = hhi_2017 - hhi_2010) %>% 
  select(!!ii,contains("chg_")) %>% 
  gather(key,value,-!!ii) %>% 
  mutate(year = as.numeric(paste0(gsub("chg_","",key))))

ii <- quo(cd114fp)
hhi_chg_cd <- 
  hhi_cd %>% 
  select(!!ii, year,!!hh ) %>% 
  mutate(year = paste0("hhi_",year)) %>% 
  spread(year,!!hh) %>% 
  mutate(hhi_2016 = coalesce(hhi_2015,hhi_2016)) %>% 
  mutate(chg_2010 = hhi_2010 - hhi_2010,
         chg_2011 = hhi_2011 - hhi_2010,
         chg_2012 = hhi_2012 - hhi_2010,
         chg_2013 = hhi_2013 - hhi_2010,
         chg_2014 = hhi_2014 - hhi_2010,
         chg_2015 = hhi_2015 - hhi_2010,
         chg_2016 = hhi_2016 - hhi_2010,
         chg_2017 = hhi_2017 - hhi_2010) %>% 
  select(!!ii,contains("chg_")) %>% 
  gather(key,value,-!!ii) %>% 
  mutate(year = as.numeric(paste0(gsub("chg_","",key))))


for (x in names(census_regions[-1])) {
  cat(x) 
  cat("\n\n")
  states_to_map <- census_regions[[x]]
  tmp <- 
    sf_cz %>% 
    left_join(hhi_chg_cz,"cz_id") %>% 
    filter(state_01 %in% states_to_map | state_02 %in% states_to_map  | state_03 %in% states_to_map) %>% 
    mutate(year = as.integer(paste0(year))) %>% 
    filter(!is.na(year)) %>% 
    ggplot() + 
    geom_sf(aes(fill =value)) +
    scale_fill_gradient2(name = "Change in Market\nConcentration Index\n(HHI)",low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 0,limits = c(-3500,3500),
                         breaks = c(-3500,0,3500),
                         labels = c("More Competitive -3,500","No Change 0","More Concentrated +3,500")) + 
    #theme(legend.position = "bottom") +
    geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
    coord_sf(datum=NA) + 
    remove_all_axes +
    ggthemes::theme_tufte(base_family = "Gill Sans") + 
    transition_time(year) +
    ease_aes('linear') + 
    labs(title = "Year: {frame_time}") 
  animate(plot = tmp, nframes = 30,end_pause = 3,duration=10, renderer = gifski_renderer(paste0('output/figures/hhi_2010-to-2017-change_',x,'.gif')))
}


for (x in names(census_regions[-1])) {
  cat(x) 
  cat("\n\n")
  states_to_map <- census_regions[[x]]
  tmp <- 
    sf_cd %>% 
    left_join(hhi_chg_cd,"cd114fp") %>% 
    filter(state %in% states_to_map ) %>% 
    mutate(year = as.integer(paste0(year))) %>% 
    filter(!is.na(year)) %>% 
    ggplot() + 
    geom_sf(aes(fill =value)) +
    scale_fill_gradient2(name = "Change in Hospital Market\nConcentration Index\n(HHI)",low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 0,limits = c(-3500,3500),
                         breaks = c(-3500,0,3500),
                         labels = c("More Competitive -3,500","No Change 0","More Concentrated +3,500")) + 
    #theme(legend.position = "bottom") +
    geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
    coord_sf(datum=NA) + 
    remove_all_axes +
    ggthemes::theme_tufte(base_family = "Gill Sans") + 
    transition_time(year) +
    ease_aes('linear') + 
    labs(title = "Year: {frame_time}") 
  animate(plot = tmp, nframes = 30,end_pause = 3,duration=10, renderer = gifski_renderer(paste0('output/figures/hhi_2010-to-2017-change_congressional-district_',x,'.gif')))
}


hh <- quo(hhi_zip)
ii <- quo(cd114fp)
hhi_chg_cd <- 
  hhi_cd %>% 
  select(!!ii, year,!!hh ) %>% 
  mutate(year = paste0("hhi_",year)) %>% 
  spread(year,!!hh) %>% 
  mutate(hhi_2016 = coalesce(hhi_2015,hhi_2016)) %>% 
  mutate(chg_2010 = hhi_2010 - hhi_2010,
         chg_2011 = hhi_2011 - hhi_2010,
         chg_2012 = hhi_2012 - hhi_2010,
         chg_2013 = hhi_2013 - hhi_2010,
         chg_2014 = hhi_2014 - hhi_2010,
         chg_2015 = hhi_2015 - hhi_2010,
         chg_2016 = hhi_2016 - hhi_2010,
         chg_2017 = hhi_2017 - hhi_2010) %>% 
  select(!!ii,contains("chg_")) %>% 
  gather(key,value,-!!ii) %>% 
  mutate(year = as.numeric(paste0(gsub("chg_","",key))))


sf_cd %>% 
  left_join(hhi_chg_cd,"cd114fp") %>% 
  filter(state %in% states_to_map ) %>% 
  mutate(year = as.integer(paste0(year))) %>% 
  filter(year==2017) %>% 
  filter(!is.na(year)) %>% 
  ggplot() + 
  geom_sf(aes(fill =value)) +
  scale_fill_gradient2(name = "Change in Hospital Market\nConcentration Index\n(HHI)",low = scales::muted("blue"),mid = "white",high = scales::muted("red"),midpoint = 0,limits = c(-3500,3500),
                       breaks = c(-3500,0,3500),
                       labels = c("More Competitive -3,500","No Change 0","More Concentrated +3,500")) + 
  #theme(legend.position = "bottom") +
  geom_sf(data = sf_state %>% filter(stusps %in% states_to_map), alpha = 0,lwd=.7,colour = "black") + 
  coord_sf(datum=NA) + 
  remove_all_axes +
  ggthemes::theme_tufte(base_family = "Gill Sans") 
