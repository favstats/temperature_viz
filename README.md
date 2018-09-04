## Temperature over Time in Stuttgart

This is a quick use-case of gganimate to visualize the rise of average temperature in my home town, Stuttgart, Germany.

## Get Packages

``` r
pacman::p_load(tidyverse, rvest, gganimate)
```

## Get and Save Data

You can get the data from here: https://icdc.cen.uni-hamburg.de/daten/atmosphere/dwd-station.html

> The German Weather Service (DWD) provides climate data for more than 70 observation stations from the measurement network in Germany. The stations provide scheduled, daily and monthly readings of temperatures, rainfall, sunshine duration, wind speed, humidity, barometric pressure and cloud cover, which are updated daily.

In this use-case, we are only interested in the temperature in *Stuttgart*. The available data for the observation station in Stuttgart ranges from 1953 until today.

``` r
url <- "http://icdc.cen.uni-hamburg.de/las/ProductServer.do?xml=%3C%3Fxml+version%3D%221.0%22%3F%3E%3ClasRequest+href%3D%22file%3Alas.xml%22%3E%3Clink+match%3D%22%2Flasdata%2Foperations%2Foperation%5B%40ID%3D%27DBExtractRowset%27%5D%22%3E%3C%2Flink%3E%3Cproperties%3E%3Cferret%3E%3Ccomponents%3E+grw_m+antdyn_m+greendyn_m+antsmb_m+ocn_m+greensmb_m+glac_m+gia_m%3C%2Fcomponents%3E%3Cview%3Exy%3C%2Fview%3E%3C%2Fferret%3E%3C%2Fproperties%3E%3Cargs%3E%3Cconstraint+id%3D%22undefined%22+type%3D%22text%22%3E%3Cv%3EStations_Kennziffer%3C%2Fv%3E%3Cv%3E%253D%3C%2Fv%3E%3Cv%3E10738%3C%2Fv%3E%3C%2Fconstraint%3E%3Clink+match%3D%22%2Flasdata%2Fdatasets%2Fdwd_data%2Fvariables%2Favg_temp_day%22%3E%3C%2Flink%3E%3Cregion%3E%3Crange+type%3D%22t%22+low%3D%2201-Jan-1876%22+high%3D%2227-Aug-2018%22%3E%3C%2Frange%3E%3Crange+type%3D%22x%22+low%3D%220%22+high%3D%2224%22%3E%3C%2Frange%3E%3Crange+type%3D%22y%22+low%3D%2240%22+high%3D%2259%22%3E%3C%2Frange%3E%3C%2Fregion%3E%3C%2Fargs%3E%3C%2FlasRequest%3E"

temp_stgt <- read_html(url) %>% 
  rvest::html_node("table") %>% html_table()

if(!dir.exists("data")) dir.create("data")

temp_stgt <- temp_stgt %>% 
  janitor::clean_names() %>% 
  rename(avg_temp_day = luftemperatur_avg) %>% 
  mutate(t = lubridate::ymd(t)) %>% 
  mutate(month = lubridate::month(t)) %>% 
  mutate(day = lubridate::yday(t)) %>% 
  arrange(t) %>% 
  mutate(year = lubridate::year(t)) %>% 
  mutate(time = year) %>% 
  group_by(year) %>% 
  mutate(avg_temp_year_year = mean(avg_temp_day)) %>% 
  ungroup()

save(temp_stgt, file = "data/temp_stgt.Rdata")
```

## Plot Temperature (Static)

``` r
load("data/temp_stgt.Rdata")

months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

temp_stgt %>% 
  # filter(year %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2018)) %>% 
  ggplot(aes(day, avg_temp_day, color = year, group = year)) +
  geom_point(size = .01, alpha = .1) +
  geom_line(size = .01, alpha = .1) +
  geom_smooth(se = F, size = .01, alpha = .2) +
  viridis::scale_color_viridis("Year", direction = -1, discrete = F,
                               breaks = c(1953, 1985, 2018),
                               labels = c(1953, 1985, 2018)) +
  ggthemes::theme_hc() +
  labs(title = "Average Daily Temperature in Stuttgart (1953 - 2018)", 
       caption = "Data: Der Deutsche Wetterdienst (DWD)",
       y = "Average Daily Temperature", x = "") +
  scale_x_continuous(breaks = seq(0, 365, length.out = 12), labels = months) +
  guides(colour = guide_colourbar(barwidth = 20, label.position = "bottom"))
```

[![](https://github.com/favstats/temperature_viz/blob/master/images/avg_temp_year.png?raw=true)](https://github.com/favstats/temperature_viz/blob/master/images/avg_temp_year.png?raw=true) 


``` r
if(!dir.exists("images")) dir.create("images")

ggsave(filename = "images/avg_temp_year.png", height = 6, width = 10)
```

## Animations

### Average Temperature over Time by Day

``` r
temp_stgt_all <- temp_stgt %>% 
  select(-year)

p1 <- temp_stgt %>% 
  # filter(year %in% c(1960, 1970, 1980, 1990, 2000, 2010, 2018)) %>% 
  ggplot(aes(day, avg_temp_day, color = avg_temp_year, group = year)) +
  geom_smooth(data = temp_stgt_all, aes(day, avg_temp_day, group = time), 
              color = "grey", se = F, size = .1, alpha = .1) +
  geom_point(size = .5, alpha = .7) +
  geom_line(size = .5, alpha = .7) +  
  viridis::scale_color_viridis("Average Yearly Temperature", direction = -1, discrete = F) +
  geom_smooth(se = F, size = 1, alpha = .7) +

  ggthemes::theme_hc()  +
  geom_text(aes(x = 340, y = 28, label = paste0("Year = ", year)), size = 3, 
            hjust = 1, color = "black") +
  geom_text(aes(x = 340, y = 26, 
                label = paste0("Avg. Temperature = ", 
                           sprintf("%.2f", round(avg_temp_year, 2)), "°C")), 
                                  size = 3, color = "black", hjust = 1) +
  labs(title = "Average Daily Temperature in Stuttgart (1953 - 2018)", 
       caption = "Data: Der Deutsche Wetterdienst (DWD)",
       y = "Average Daily Temperature", x = "") +
  scale_x_continuous(breaks = seq(0, 365, length.out = 12), labels = months) +
  guides(colour = guide_colourbar(barwidth = 20, label.position = "bottom")) +
  # Here comes the gganimate code
  transition_time(
    year
  ) +
  enter_fade() + 
  exit_fade() +
  ease_aes('linear')

magick::image_write(
  image = animate(p1,  width = 1000, height = 600, renderer = magick_renderer(), length = 45), 
  path = "images/daily_temp.gif"
)
```


[![](https://github.com/favstats/temperature_viz/blob/master/images/daily_temp.gif?raw=true)](https://github.com/favstats/temperature_viz/blob/master/images/daily_temp.gif?raw=true) 

### Average Temperature over Time by Year

``` r
temp_stgt_allyear <- temp_stgt %>% 
  filter(year != 2018) %>%
  group_by(year) %>% 
  summarise(avg_temp_year = mean(avg_temp_year)) %>% 
  mutate(time = year) %>% 
  select(-year)

p2 <- temp_stgt %>% 
  filter(year != 2018) %>%
  group_by(year) %>% 
  summarise(avg_temp_year = mean(avg_temp_year)) %>% 
  ggplot(aes(year, avg_temp_year)) +
  geom_smooth(data = temp_stgt_allyear, aes(time, avg_temp_year), 
              color = "red", se = F, size = .8) +
  geom_line(data = temp_stgt_allyear, aes(time, avg_temp_year), 
              color = "black", size = .8, alpha = .6) +
  geom_point(data = temp_stgt_allyear, aes(time, avg_temp_year, color = avg_temp_year), size = 2.5) +
  geom_point(aes(color = avg_temp_year), size = 5) +
  ggthemes::theme_hc()  +
  geom_text(aes(x = 1953.5, y = 10.8, 
                label = paste0("Year = ", year)), 
                size = 3, color = "black",
                hjust = 0) +
  geom_text(aes(x = 1953.5, y = 10.7, 
                label = paste0("Avg. Temperature = ", 
                           sprintf("%.2f", round(avg_temp_year, 2)), "°C")), 
                                  size = 3, color = "black", hjust = 0) +
  viridis::scale_color_viridis("Average Yearly Temperature", direction = -1, discrete = F)  +
  labs(title = "Average Yearly Temperature in Stuttgart (1953 - 2017)", 
       caption = "Data: Der Deutsche Wetterdienst (DWD)",
       y = "Average Yearly Temperature", x = "") +
  guides(colour = guide_colourbar(barwidth = 20, label.position = "bottom")) +
  # Here comes the gganimate code
  transition_time(
    year
  ) +
  enter_fade() + 
  exit_fade() +
  ease_aes('linear') +
  ease_aes('linear')

magick::image_write(
  image = animate(p2,  width = 1000, height = 600, renderer = magick_renderer(), length = 45), 
  path = "images/yearly_temp.gif"
)
```


[![](https://github.com/favstats/temperature_viz/blob/master/images/yearly_temp.gif?raw=true)](https://github.com/favstats/temperature_viz/blob/master/images/yearly_temp.gif?raw=true) 

### Average Temperature over Time by Year

``` r
p3 <- temp_stgt %>% 
  filter(year %in% c(1953, 1960, 1990, 2017)) %>%
  ggplot(aes(day, avg_temp_day, group = year, color = year)) +
  geom_line() + 
  geom_segment(aes(xend = 365, yend = avg_temp_day), linetype = 2, colour = 'grey') + 
  geom_point(size = 2) + 
  geom_text(aes(x = 365, label = year), hjust = 0, size = 3, fontface = "bold") + 
  coord_cartesian(clip = 'off') + 
  ggthemes::theme_hc()  +
  labs(title = "Average Daily Temperature in Stuttgart (1953, 1960, 1990, 2017)", 
       caption = "Data: Der Deutsche Wetterdienst (DWD)",
       y = "Average Daily Temperature", x = "") +
  scale_x_continuous(breaks = seq(0, 365, length.out = 12), labels = months) +
  viridis::scale_color_viridis("Average Yearly Temperature", direction = -1, discrete = F) +
  guides(colour = F) +
  theme(title = element_text(size = 15, face = "bold"), 
        axis.text.x = element_text(size = 14, face = "bold"), 
        axis.text.y = element_text(size = 10, face = "italic")) +
  # Here comes the gganimate code
  transition_reveal(year, day) + 
  enter_fade() + 
  exit_fade() +
  ease_aes('linear') 

# create animation
p3 %>% animate(
  nframes = 500, fps = 15, width = 1000, height = 600, detail = 3
)

anim_save("images/daily_temp_reveal.gif")
```


[![](https://github.com/favstats/temperature_viz/blob/master/images/daily_temp_reveal.gif?raw=true)](https://github.com/favstats/temperature_viz/blob/master/images/daily_temp_reveal.gif?raw=true) 


``` r
sessionInfo()
```

    R version 3.5.0 (2018-04-23)
    Platform: x86_64-w64-mingw32/x64 (64-bit)
    Running under: Windows >= 8 x64 (build 9200)
    
    Matrix products: default
    
    locale:
    [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252   
    [3] LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                   
    [5] LC_TIME=German_Germany.1252    
    
    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     
    
    other attached packages:
     [1] bindrcpp_0.2.2       rvest_0.3.2          xml2_1.2.0           forcats_0.3.0       
     [5] stringr_1.3.0        dplyr_0.7.6          purrr_0.2.5          readr_1.1.1         
     [9] tidyr_0.8.1          tibble_1.4.2         tidyverse_1.2.1      gganimate_0.9.9.9999
    [13] ggplot2_3.0.0       
    
    loaded via a namespace (and not attached):
     [1] viridis_0.5.1     httr_1.3.1        jsonlite_1.5      viridisLite_0.3.0
     [5] transformr_0.1.0  modelr_0.1.1      assertthat_0.2.0  cellranger_1.1.0 
     [9] progress_1.2.0    pillar_1.2.1      lattice_0.20-35   glue_1.3.0       
    [13] digest_0.6.16     colorspace_1.4-0  plyr_1.8.4        psych_1.8.3.3    
    [17] lpSolve_5.6.13    pkgconfig_2.0.1   devtools_1.13.5   broom_0.4.4      
    [21] gifski_0.8.3      haven_1.1.2       magick_1.9        patchwork_0.0.1  
    [25] scales_1.0.0      tweenr_0.1.5.9999 git2r_0.23.0      farver_1.0       
    [29] pacman_0.4.6      withr_2.1.2       lazyeval_0.2.1    cli_1.0.0        
    [33] mnormt_1.5-5      magrittr_1.5      crayon_1.3.4      readxl_1.1.0     
    [37] memoise_1.1.0     nlme_3.1-137      foreign_0.8-70    class_7.3-14     
    [41] ggthemes_4.0.0    tools_3.5.0       prettyunits_1.0.2 hms_0.4.2        
    [45] munsell_0.5.0     compiler_3.5.0    e1071_1.6-8       rlang_0.2.2      
    [49] classInt_0.2-3    units_0.6-0       grid_3.5.0        rstudioapi_0.7   
    [53] labeling_0.3      gtable_0.2.0      DBI_1.0.0         curl_3.2         
    [57] reshape2_1.4.3    R6_2.2.2          gridExtra_2.3     lubridate_1.7.4  
    [61] knitr_1.20        bindr_0.1.1       stringi_1.1.7     parallel_3.5.0   
    [65] Rcpp_0.12.18      sf_0.6-3          png_0.1-7         spData_0.2.8.3   
    [69] tidyselect_0.2.4 
