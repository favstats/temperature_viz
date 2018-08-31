Visualizing Temperature in Stuttgart
================
Fabio Votta
31 8 2018

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

``` r
sessionInfo()
```

      ## R version 3.5.0 (2018-04-23)
      ## Platform: x86_64-w64-mingw32/x64 (64-bit)
      ## Running under: Windows >= 8 x64 (build 9200)
      ## 
      ## Matrix products: default
      ## 
      ## locale:
      ## [1] LC_COLLATE=German_Germany.1252 
      ## [2] LC_CTYPE=German_Germany.1252   
      ## [3] LC_MONETARY=German_Germany.1252
      ## [4] LC_NUMERIC=C                   
      ## [5] LC_TIME=German_Germany.1252    
      ## 
      ## attached base packages:
      ## [1] stats     graphics  grDevices utils     datasets  methods  
      ## [7] base     
      ## 
      ## other attached packages:
      ##  [1] gganimate_0.9.9.9999 rvest_0.3.2         
      ##  [3] xml2_1.2.0           forcats_0.3.0       
      ##  [5] stringr_1.3.0        dplyr_0.7.5         
      ##  [7] purrr_0.2.4          readr_1.1.1         
      ##  [9] tidyr_0.8.1          tibble_1.4.2        
      ## [11] ggplot2_3.0.0.9000   tidyverse_1.2.1     
      ## 
      ## loaded via a namespace (and not attached):
      ##  [1] Rcpp_0.12.18      lubridate_1.7.4   lattice_0.20-35  
      ##  [4] prettyunits_1.0.2 png_0.1-7         class_7.3-14     
      ##  [7] digest_0.6.15     assertthat_0.2.0  psych_1.8.3.3    
      ## [10] R6_2.2.2          cellranger_1.1.0  plyr_1.8.4       
      ## [13] e1071_1.6-8       httr_1.3.1        pillar_1.2.1     
      ## [16] rlang_0.2.1       progress_1.2.0    lazyeval_0.2.1   
      ## [19] readxl_1.1.0      rstudioapi_0.7    gifski_0.8.3     
      ## [22] magick_1.9        labeling_0.3      foreign_0.8-70   
      ## [25] munsell_0.4.3     broom_0.4.4       compiler_3.5.0   
      ## [28] modelr_0.1.1      pkgconfig_2.0.1   mnormt_1.5-5     
      ## [31] tidyselect_0.2.4  gridExtra_2.3     lpSolve_5.6.13   
      ## [34] viridisLite_0.3.0 crayon_1.3.4      withr_2.1.2      
      ## [37] sf_0.6-3          grid_3.5.0        nlme_3.1-137     
      ## [40] spData_0.2.8.3    jsonlite_1.5      gtable_0.2.0     
      ## [43] DBI_1.0.0         pacman_0.4.6      magrittr_1.5     
      ## [46] units_0.6-0       scales_0.5.0      cli_1.0.0        
      ## [49] stringi_1.1.7     farver_1.0        reshape2_1.4.3   
      ## [52] ggthemes_4.0.0    viridis_0.5.1     bindrcpp_0.2.2   
      ## [55] transformr_0.1.0  tools_3.5.0       glue_1.3.0       
      ## [58] tweenr_0.1.5.9999 hms_0.4.2         parallel_3.5.0   
      ## [61] colorspace_1.4-0  classInt_0.2-3    knitr_1.20       
      ## [64] bindr_0.1.1       haven_1.1.2       patchwork_0.0.1       
