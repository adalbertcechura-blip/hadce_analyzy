#MAIN DATA ANALYSIS BRANCH

library(tidyverse)
library(janitor)
library(readxl)
library(readr)


# Data import -------------------------------------------------------------

env <- read_delim("data/serp_env.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE)

spe <- read_csv("data/serp_spe.csv") %>% 
  clean_names()


# Nomenclature check ------------------------------------------------------

#nomenclature <- read_csv("data/nomenclature_20251108.csv") %>%
#  clean_names()  

#nomenclature_check <-  nomenclature %>% 
#  left_join(spe %>% 
#              count(species_nr)) %>% 
#  filter(!is.na(n))

#write.csv(nomenclature_check, "data/nomenclature_check.csv")

nomenclature <- read_csv("data/nomenclature_check.csv")

spe <- spe %>% 
  left_join(nomenclature) %>% 
  select(releve_nr, cover_code, layer, species, nonvascular) 

# Cover -------------------------------------------------------------------

cover <- read_csv("data/table_cover.csv") %>% clean_names()  
cover

cover %>% 
  filter(str_starts(cover_scale_name, "Braun")) %>% 
  print(n=20)

#cover codes to percentages
spe <- spe %>%
  left_join(env %>% select(poradi), by = c("releve_nr" = "poradi")) %>% 
  mutate(coverscale = "02") %>% 
  left_join(cover %>% select(coverscale, cover_code, cover_perc))

# Merging of species covers across layers ---------------------------------

spe %>%    
  group_by(releve_nr, species, layer) %>%    
  count() %>%   
  filter(n>1)       # zero duplicates in one layer

spe %>%    
  group_by(releve_nr, species) %>%    
  count() %>%   
  filter(n>1)       # 17 duplicates across layers

combine_cover <- function(x){
  while (length(x)>1){
    x[2] <- x[1]+(100-x[1])*x[2]/100
    x <- x[-1]
  }
  return(x)
}

spe <- spe %>%    
  group_by(releve_nr, species, layer, nonvascular) %>%    
  summarize(cover_perc_new = combine_cover(cover_perc))

#spe %>% 
#  write_csv("data/spe_merged_covers.csv")

spe_merged_covers <- read_csv("Data/spe_merged_covers.csv")

spe_vas <- spe %>% 
  filter(nonvascular == 0)      # only vascular plants

spe_nonv <- spe %>% 
  filter(nonvascular == 1)      # only nonvascular plants


# Analysis ----------------------------------------------------------------

richness <- spe %>%
  ungroup() %>% 
  group_by(releve_nr) %>%
  summarise(n = n_distinct(species))

env %>%
  left_join(richness, by = c("poradi" = "releve_nr")) %>%
  ggplot(aes(x = s_ph_h2o, y = n)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Effect of Soil pH on Species Richness",
       x = "Soil pH", 
       y = "Number of Species per Relevé")

env %>%
  left_join(richness, by = c("poradi" = "releve_nr")) %>%
  lm(n ~ s_ph_h2o, data = .) %>%
  summary() 
# linear regression analysis showed no significant relationship between soil pH and species richness
# R-squared:  0.0001469; F-statistic: 0.01469 on 1 and 100 DF;  p-value: 0.9038


# Serp. specialists -------------------------------------------------------

serp_spe <- read_excel("Data/hadcovi_specialisti.xlsx")
spe_merged_covers |> left_join(serp_spe, by= c("species" = "species_name")) |> 
  mutate(serpentine_affinity = replace_na(serpentine_affinity, 0)) |> 
  count(releve_nr, serpentine_affinity) |>
  complete(releve_nr, serpentine_affinity, fill = list(n = 0)) |> 
  group_by(releve_nr) |> 
  mutate(podil=n/sum(n)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate (2)", "Facultative (1)", "Generalists (0)"))) |> 
  ggplot(aes(x = factor(releve_nr), y = podil, fill = affinity)) +
  geom_col() +
  facet_wrap(~ affinity, ncol = 1, scales = "free_y") +
  scale_x_discrete(labels = function(x) {
    sapply(strsplit(x, ""), function(chars) paste(rev(chars), collapse = "\n"))
  })+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(
    values = c(
      "Obligate (2)" = "#d95f02",
      "Facultative (1)" = "#7570b3",
      "Generalists (0)" = "#999999"
    )
  ) +
  labs(
    x = "Relevé Number",
    y = "Proportion of Species in the Plot",
    title = "Proportion of Serpentine Specialists in Relevés"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, size = 7),
    panel.grid.major.x = element_blank())
