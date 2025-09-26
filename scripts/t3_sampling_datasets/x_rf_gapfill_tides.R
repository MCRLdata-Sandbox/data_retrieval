## Can I use information from around the area to estimate water levels, 
## temperatures, and salinity in Sequim Bay? The answer is yes. Let's make a series
## of diagnostic plots as well as a series of gapfilled time-series marking when
## sampling occurred. This is basically an extension on the 250815 script for
## Noelani's paper

# 1. Set up environment --------------------------------------------------------

rm(list = ls())

library(devtools)
source("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/scripts/0_setup.R")

require(pacman)
p_load(tidyverse, 
       tictoc,
       ggrepel,
       ranger,
       VulnToolkit)


# 2. Pull NOAA data for PA tides (a complete time-series for gapfilling) -------

start = '2024-01-01'
end = '2025-05-01'


## First, pull information on tides
tic("run script")
rtide_raw <- rtide::tide_height('Port Angeles',
                                    from = as.Date(start),
                                    to = as.Date(end),
                                    minutes = 15, tz ='Etc/GMT+8') %>%
  clean_names() %>%
  rename("time_utc" = "date_time",
         "rtide_height_m" = tide_height)


# 3. Bring in MCRLdata datasets that we want to gapfill ------------------------

## Read in datasets
tide <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250515_tidegauge_L1.csv") %>% 
  mutate(time_pst = round_date(time_pst, "15 min")) %>% 
  group_by(time_pst) %>% 
  summarize(water_level_m_navd88 = mean(water_level_m_navd88, na.rm = T)) %>% 
  ungroup()

ctd <- read_csv("https://raw.githubusercontent.com/MCRLdata-Sandbox/data_prep/refs/heads/main/data/outputs/L1/250815_ctd_water_temp_salinity_L1.csv") %>% 
  mutate(time_pst = round_date(time_pst, "15 min")) %>% 
  group_by(time_pst) %>% 
  summarize(temp_deg_c = mean(temp_deg_c, na.rm = T), 
            salinity_psu_clean = mean(salinity_psu_clean, na.rm = T)) %>% 
  ungroup()


# 4. Join rtide and water levels to see if we can predict ----------------------

df <- left_join(rtide_raw %>% dplyr::select(time_utc, rtide_height_m), 
          tide %>% dplyr::select(-contains("tide")), 
          by = c("time_utc" = "time_pst")) %>% 
  #drop_na() %>% 
  rename("time_pst" = time_utc) %>% #labeled utc, but actually pst
  mutate(rtide_lag8 = lag(rtide_height_m, 8)) %>% 
  mutate(doy = yday(time_pst), 
         hour = as.numeric(hour(time_pst)), 
         month = month(time_pst))

## Used to determine appropriate lag
ccf(df %>% drop_na() %>% pull(rtide_height_m), 
    df %>% drop_na() %>% pull(water_level_m_navd88))

plot_grid(ggplot(df, aes(rtide_height_m, water_level_m_navd88)) + 
  geom_point(), 
  ggplot(df, aes(rtide_lag8, water_level_m_navd88)) + 
    geom_point(), 
  nrow = 1)
 

# 5. Model water level from NOAA water level and gapfill -----------------------

## Resulting model
wl_rf <- ranger(water_level_m_navd88~rtide_height_m+hour+doy, data = df %>% drop_na())
wl_rf #R2 = 0.98 - solid

# Gap-fill function - thx chat
gap_fill_rf <- function(df, model, target_col = "water_level_m_navd88") {
  
  # Get predictions for all rows (will be NA where predictors are missing)
  df$pred <- predict(model, df)$predictions
  
  # Create gap-filled column
  df <- df %>%
    mutate(
      data_filled = case_when(
        !is.na(.data[[target_col]]) ~ .data[[target_col]],  # Keep original values
        !is.na(pred) ~ pred,                                # Fill gaps with predictions
        TRUE ~ NA_real_                                     # Keep NA if can't predict
      ),
      is_filled = is.na(.data[[target_col]]) & !is.na(pred)
    )
  
  return(df)
}

# Apply gap-filling
wl_filled <- gap_fill_rf(df, wl_rf) %>% 
  rename("water_level_filled" = data_filled)


# 6. Use gapfilled water levels to gap-fill salinity and temp ------------------

wl_filled_15min <- wl_filled %>% 
  mutate(time_pst = round_date(time_pst, "15 min")) %>% 
  group_by(time_pst) %>% 
  summarize(water_level_filled = mean(water_level_filled, na.rm = T)) %>% 
  ungroup()

df1 <-  full_join(wl_filled %>% dplyr::select(time_pst, water_level_filled),
                  ctd %>% filter(time_pst >= min(wl_filled$time_pst) & 
                                   time_pst <= max(wl_filled$time_pst)), 
                  by = "time_pst") %>% 
  mutate(doy = yday(time_pst), 
         hour = as.numeric(hour(time_pst)), 
         month = month(time_pst))

rf_temp <- ranger(temp_deg_c~water_level_filled+hour+doy, data = df1 %>% drop_na())
rf_sal <- ranger(salinity_psu_clean~water_level_filled+hour+doy, data = df1 %>% drop_na())

# Function to gap-fill multiple parameters at once
gap_fill_multiple <- function(df, model_list, target_cols, suffix = "_filled") {
  
  for (i in seq_along(target_cols)) {
    target_col <- target_cols[i]
    model <- model_list[[i]]
    
    # Get predictions
    predictions <- predict(model, df)$predictions
    filled_col <- paste0(target_col, suffix)
    is_filled_col <- paste0(target_col, "_is_filled")
    
    # Add filled columns
    df <- df %>%
      mutate(
        !!filled_col := case_when(
          !is.na(.data[[target_col]]) ~ .data[[target_col]],
          !is.na(predictions) ~ predictions,
          TRUE ~ NA_real_
        ),
        !!is_filled_col := is.na(.data[[target_col]]) & !is.na(predictions)
      )
  }
  
  return(df)
}

df1_filled <- gap_fill_multiple(
  df1, 
  model_list = list(rf_temp, rf_sal),
  target_cols = c("temp_deg_c", "salinity_psu_clean"))

plot_grid(ggplot(wl_filled, aes(x = time_pst)) + 
  geom_line(aes(y = water_level_filled), color = "blue", alpha = 0.7) + 
  geom_line(aes(y = water_level_m_navd88), color = "gray"), 
  ggplot(df1_filled, aes(x = time_pst)) + 
    geom_line(aes(y = temp_deg_c_filled), color = "blue", alpha = 0.7) + 
    geom_line(aes(y = temp_deg_c), color = "gray"),
  ggplot(df1_filled, aes(x = time_pst)) + 
    geom_line(aes(y = salinity_psu_clean_filled), color = "blue", alpha = 0.7) + 
    geom_line(aes(y = salinity_psu_clean), color = "gray"),
  ncol = 1) 
ggsave("scripts/t3_sampling_datasets/rf_gapfilling.png", width = 6, height = 9)


# 7. Bring in sampling datetimes -----------------------------------------------

sampling_dates <- read_csv("/Users/regi350/Downloads/250815_noelani_sampling_dates.csv") %>% 
  mutate(sampling_date = parsedate::parse_date(sampling_date)) %>% 
  mutate(sampling_datetime = round_date(sampling_date + sampling_time, "15 min"))

toc()


# 8. Make plots ----------------------------------------------------------------

make_plot <- function(var, ylab, offset){
  ggplot(df1_filled, aes(time_pst, {{var}})) + 
    geom_line(color = "gray90") + 
    geom_segment(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
                 aes(y = {{var}}, yend = offset), linetype = "dashed") + 
    geom_point(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
               color = "gray20") + 
    ggrepel::geom_label_repel(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
                              aes(label = round({{var}}, 1)), 
                              max.overlaps = 13,
                              nudge_y = -0.5) + 
    geom_point(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
               aes(y = offset), alpha = 0.7, size = 3, fill = "blue", pch = 25) + 
    labs(x = "", y = ylab)
}

plot_grid(make_plot(water_level_filled, "Water level (m NAVD88)", 3), 
          make_plot(temp_deg_c_filled, "Water temp (C)", 19), 
          make_plot(salinity_psu_clean_filled, "Salinity (PSU)", 33), 
          ncol = 1)
ggsave("scripts/t3_sampling_datasets/sampling_plots.png", width = 8, height = 8, dpi = 300)
ggsave("scripts/t3_sampling_datasets/sampling_plots.jpg", width = 8, height = 8, dpi = 300)

# 9. Make no-label plots -------------------------------------------------------

make_plot_no_labels <- function(var, ylab, offset){
  ggplot(df1_filled, aes(time_pst, {{var}})) + 
    geom_line(color = "gray90") + 
    geom_segment(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
                 aes(y = {{var}}, yend = offset), linetype = "dashed") + 
    geom_point(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
               color = "gray20") + 
    #ggrepel::geom_label_repel(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
    #                          aes(label = round({{var}}, 1)), 
    #                          max.overlaps = 13,
    #                          nudge_y = -0.5) + 
    geom_point(data = df1_filled %>% filter(time_pst %in% sampling_dates$sampling_date), 
               aes(y = offset), alpha = 0.7, size = 3, fill = "blue", pch = 25) + 
    labs(x = "", y = ylab)
}

plot_grid(make_plot_no_labels(water_level_filled, "Water level (m NAVD88)", 3), 
          make_plot_no_labels(temp_deg_c_filled, "Water temp (C)", 19), 
          make_plot_no_labels(salinity_psu_clean_filled, "Salinity (PSU)", 33), 
          ncol = 1)
ggsave("scripts/t3_sampling_datasets/sampling_plots_no_labels.png", width = 8, height = 8, dpi = 300)
ggsave("scripts/t3_sampling_datasets/sampling_plots_no_labels.jpg", width = 8, height = 8, dpi = 300)

## 50k before forcing ctd to 15 min at top of script
## 100k after forcing ctd to 15 min at top of script??
## 
df1_filled %>% filter(temp_deg_c_is_filled == TRUE)





