# Check duplicates for site, date.

lapply(c("tidyverse", "ggsci", "ggpubr"), require, character.only = T)

my_theme <- theme_bw() + 
  theme(axis.text = element_text(color = "black"), 
        axis.ticks = element_line(color = "black"), 
        panel.grid = element_blank(),
        strip.background = element_rect(size= 0.75, fill = "white"),
        legend.position = "bottom")

theme_set(my_theme)

# Loading data.
# Change the source to AllMasterVars.R or whatever the file is called in your data set.
source("C://Users/dgschwentner2/OneDrive - University of Nebraska-Lincoln/PhD/Aim1Biogeochem/R Code/LRock_AllMasterVars.R")

# Pivot to include all vars for plotting.
master_wide <- ALL_CNP_VARS %>%
  pivot_wider(id_cols = c(DATE_COL, SITE_ID,  LAT, LON, ECO_TYPE), # Gotta drop units here.
              names_from = VARIABLE, values_from = RESULT, values_fn = dplyr::n)

# Check for nr. of duplicates.
duplicates_nr <- ALL_CNP_VARS %>%
  dplyr::group_by(DATE_COL, SITE_ID, LAT, LON, ECO_TYPE, VARIABLE) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 

# Summary of the duplicates.
summary(duplicates_nr$n) # Ok, duplicates range from 2 to 38 per site, day.

# Histogram
duplicates_nr %>%
  ggplot(aes(n)) + 
  geom_histogram(col = "black", fill = "white")

# Nr of duplicate observations == 2
nrow(duplicates_nr[duplicates_nr$n == 2, ])/nrow(duplicates_nr) * 100
nrow(duplicates_nr[duplicates_nr$n > 10, ])/nrow(duplicates_nr) * 100

# Check out distribution of values within the duplicates. Gonna use a cut-off of 10 to make life easier for myself
duplicated10 <- duplicates_nr %>% 
  filter(n >= 10) %>%
  mutate(unique_id = paste0(DATE_COL, SITE_ID, LAT, LON, ECO_TYPE, VARIABLE))

# Extract duplicated values from master data set.
duplicated_values <- ALL_CNP_VARS %>%
  mutate(unique_id = paste0(DATE_COL, SITE_ID, LAT, LON, ECO_TYPE, VARIABLE)) %>%# Create unique id in master data set to pull out data.)
  filter(unique_id %in% duplicated10$unique_id) %>%
  mutate(unique_id2 = paste0(DATE_COL, SITE_ID, LAT, LON, ECO_TYPE) ) # Need this for faceting down the line

# Now create density distributions to check what the data is up to

for(i in unique(duplicated_values$unique_id2)){
  plot(duplicated_values %>% filter(unique_id2 == i) %>%
    ggplot(aes(RESULT)) + 
    geom_density() + 
    facet_wrap(VARIABLE~., scales = "free"))
}
        
