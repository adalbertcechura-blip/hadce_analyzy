#MAIN DATA ANALYSIS BRANCH

library(tidyverse)
library(janitor)
library(readxl)
library(readr)


# Data import -------------------------------------------------------------

env <- read_delim("data/serp_env.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Windows-1250"))

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

serp_spe <- read_excel("Data/hadcovi_specialisti_edit.xlsx")
spe_merged_covers <- read_csv("Data/spe_merged_covers.csv")
spe_merged_covers |> left_join(serp_spe, by= c("species" = "species_name")) |> 
  left_join(env |> select(poradi, locality, locality_detail), by=c("releve_nr"="poradi"))-> spe_merged_covers

spe_merged_covers |> count(releve_nr, serpentine_affinity) |>
  complete(releve_nr, serpentine_affinity, fill = list(n = 0)) |> 
  group_by(releve_nr) |> 
  mutate(podil=n/sum(n)) |>
  ungroup() |> 
  filter(serpentine_affinity != 0) |>
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1),
      labels = c("Obligate", "Facultative"))) -> serp_spe_count

serp_spe_count |> ggplot(aes(x = factor(releve_nr), y = podil, fill = affinity)) +
  geom_col() +
  scale_x_discrete(labels = function(x) {
    sapply(strsplit(x, ""), function(chars) paste(rev(chars), collapse = "\n"))
  })+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name = NULL,
    values = c(
      "Obligate" = "#d95f02",
      "Facultative" = "#7570b3"
    )
  ) +
  labs(
    x = "Relevé Number",
    y = "Proportion of Species in the Plot",
    title = "Proportion of Serpentine Specialists in Relevés"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, size = 7, face="bold"))
   # panel.grid.major.x = element_blank())

#GEOM_COL PRO POČTY DRUHŮ Z JEDNOTLIVÝCH KATEGORIÍ PRO CELKY

spe_merged_covers |> filter(nonvascular==0) |>
  count(releve_nr, locality, serpentine_affinity) |> 
  complete(nesting(releve_nr, locality), serpentine_affinity, fill = list(n = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(locality, affinity) |> 
  summarise(mean_n = mean(n), .groups = "drop") |> 
  group_by(locality) |> 
  mutate(spec_celkem = sum(mean_n[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    locality = fct_reorder(locality, spec_celkem),
    affinity = fct_rev(affinity)) |> 
  ggplot(aes(x = mean_n, y = locality, fill = affinity)) +
  geom_col(width = 0.75) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Obligate" = "#d95f02",
      "Facultative" = "#7570b3",
      "Generalists" = "#cccccc"
    )
  ) +
  labs(
    title = "Mean Number of Vascular Plants in Plot grouped by localities",
    x = "Mean Number of Species in Plot",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8, face = "bold"),
    panel.grid.major.y = element_blank()
  )


#GEOM_COL PRO POČTY DRUHŮ Z JEDNOTLIVÝCH KATEGORIÍ PRO LOKALITY

spe_merged_covers |> filter(nonvascular==0) |>
  count(releve_nr, locality_detail, serpentine_affinity) |> 
  complete(nesting(releve_nr, locality_detail), serpentine_affinity, fill = list(n = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(locality_detail, affinity) |> 
  summarise(mean_n = mean(n), .groups = "drop") |>
  group_by(locality_detail) |> 
  mutate(spec_celkem = sum(mean_n[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    locality_detail = fct_reorder(locality_detail, spec_celkem),
    affinity = fct_rev(affinity)
  ) |> 
  ggplot(aes(x = mean_n, y = locality_detail, fill = affinity)) +
  geom_col(width = 0.75) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Obligate" = "#d95f02",
      "Facultative" = "#7570b3",
      "Generalists" = "#cccccc"
    )
  ) +
  labs(
    title = "Mean Number of Vascular Plants in Plot grouped by localities",
    x = "Mean Number of Species in Plot",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8, face = "bold"),
    panel.grid.major.y = element_blank()
  )

#GEOM_COL PRO POKRYVNOSTI DRUHŮ Z JEDNOTLIVÝCH KATEGORIÍ PRO CELKY

spe_merged_covers|> filter(nonvascular==0) |> 
  group_by(releve_nr, locality, serpentine_affinity) |> 
  summarise(suma_cover = sum(cover_perc_new, na.rm = TRUE), .groups = "drop") |> 
  complete(nesting(releve_nr, locality), serpentine_affinity, fill = list(suma_cover = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(locality, affinity) |> 
  summarise(mean_cover = mean(suma_cover), .groups = "drop") |> 
  group_by(locality) |> 
  mutate(spec_celkem = sum(mean_cover[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    locality = fct_reorder(locality, spec_celkem),
    affinity = fct_rev(affinity)
  ) |> 
  ggplot(aes(x = mean_cover, y = locality, fill = affinity)) +
  geom_col(width = 0.75) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Obligate" = "#d95f02",
      "Facultative" = "#7570b3",
      "Generalists" = "#cccccc"
    )
  ) +
  labs(
    title = "Mean Cumulative Cover of Vascular Plants in Plot grouped by localities",
    x = "Mean Cumulative Cover in Plot",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8, face = "bold"),
    panel.grid.major.y = element_blank()
  )

#GEOM_COL PRO POKRYVNOSTI DRUHŮ Z JEDNOTLIVÝCH KATEGORIÍ PRO LOKALITY

spe_merged_covers|> filter(nonvascular==0) |> 
  group_by(releve_nr, locality_detail, serpentine_affinity) |> 
  summarise(suma_cover = sum(cover_perc_new, na.rm = TRUE), .groups = "drop") |> 
  complete(nesting(releve_nr, locality_detail), serpentine_affinity, fill = list(suma_cover = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(locality_detail, affinity) |> 
  summarise(mean_cover = mean(suma_cover), .groups = "drop") |> 
  group_by(locality_detail) |> 
  mutate(spec_celkem = sum(mean_cover[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    locality_detail = fct_reorder(locality_detail, spec_celkem),
    affinity = fct_rev(affinity)
  ) |> 
  ggplot(aes(x = mean_cover, y = locality_detail, fill = affinity)) +
  geom_col(width = 0.75) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Obligate" = "#d95f02",
      "Facultative" = "#7570b3",
      "Generalists" = "#cccccc"
    )
  ) +
  labs(
    title = "Mean Cumulative Cover of Vascular Plants in Plot grouped by localities",
    x = "Mean Cumulative Cover in Plot",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.y = element_text(size = 8, face = "bold"),
    panel.grid.major.y = element_blank()
  )
# Průměry lokality ----

names(env)
spe_merged_covers <- read_csv("Data/spe_merged_covers.csv")
spe_merged_covers |> 
  left_join(env, by=c("releve_nr"="poradi")) -> spe_merged_covers_env

spe_merged_covers_env |>
  group_by(locality_detail) |>
  ggplot(aes(y = locality_detail, x = s_ph_h2o)) +
  geom_boxplot()

# 1. Výpočet průměrů na lokalitu
env_averages_locality <- env |>
  group_by(locality) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |> 
  select(-c(poradi,deg_lat, deg_lon))

# 1. Výpočet průměrů na lokalitu
env_averages_detail <- env |>
  group_by(locality_detail) |>
  summarise(across(where(is.numeric), \(x) mean(x, na.rm = TRUE))) |> 
  select(-c(poradi,deg_lat, deg_lon))
