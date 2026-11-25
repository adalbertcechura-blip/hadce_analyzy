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

localities <- read_excel("Data/Hadce-lokality-regiony-strucne-nazvy.xlsx", sheet = "List1") |> clean_names()

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
env <- read_delim("data/serp_env.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "Windows-1250"))
env |> left_join(localities, by = c("locality_detail" = "jmeno_od_vojty")) -> env
spe_merged_covers |> left_join(serp_spe, by= c("species" = "species_name")) -> spe_merged_covers 
spe_merged_covers |>
left_join(env |> select(poradi, region, site), by=c("releve_nr"="poradi"))-> spe_merged_covers

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
  count(releve_nr, region, serpentine_affinity) |> 
  complete(nesting(releve_nr, region), serpentine_affinity, fill = list(n = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(region, affinity) |> 
  summarise(mean_n = mean(n), .groups = "drop") |> 
  group_by(region) |> 
  mutate(spec_celkem = sum(mean_n[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    region = fct_reorder(region, spec_celkem),
    affinity = fct_rev(affinity)) |> 
  ggplot(aes(x = mean_n, y = region, fill = affinity)) +
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
  count(releve_nr, site, serpentine_affinity) |> 
  complete(nesting(releve_nr, site), serpentine_affinity, fill = list(n = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(site, affinity) |> 
  summarise(mean_n = mean(n), .groups = "drop") |>
  group_by(site) |> 
  mutate(spec_celkem = sum(mean_n[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    site = fct_reorder(site, spec_celkem),
    affinity = fct_rev(affinity)
  ) |> 
  ggplot(aes(x = mean_n, y = site, fill = affinity)) +
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
  group_by(releve_nr, region, serpentine_affinity) |> 
  summarise(suma_cover = sum(cover_perc_new, na.rm = TRUE), .groups = "drop") |> 
  complete(nesting(releve_nr, region), serpentine_affinity, fill = list(suma_cover = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(region, affinity) |> 
  summarise(mean_cover = mean(suma_cover), .groups = "drop") |> 
  group_by(region) |> 
  mutate(spec_celkem = sum(mean_cover[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    region = fct_reorder(region, spec_celkem),
    affinity = fct_rev(affinity)
  ) |> 
  ggplot(aes(x = mean_cover, y = region, fill = affinity)) +
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
  group_by(releve_nr, site, serpentine_affinity) |> 
  summarise(suma_cover = sum(cover_perc_new, na.rm = TRUE), .groups = "drop") |> 
  complete(nesting(releve_nr, site), serpentine_affinity, fill = list(suma_cover = 0)) |> 
  mutate(
    affinity = factor(
      serpentine_affinity,
      levels = c(2, 1, 0),
      labels = c("Obligate", "Facultative", "Generalists")
    )
  ) |> 
  group_by(site, affinity) |> 
  summarise(mean_cover = mean(suma_cover), .groups = "drop") |> 
  group_by(site) |> 
  mutate(spec_celkem = sum(mean_cover[affinity != "Generalists"])) |> 
  ungroup() |> 
  mutate(
    site = fct_reorder(site, spec_celkem),
    affinity = fct_rev(affinity)
  ) |> 
  ggplot(aes(x = mean_cover, y = site, fill = affinity)) +
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
# Průměry lokality --------------------------------------------------------

names(env)
spe_merged_covers <- read_csv("Data/spe_merged_covers.csv")
spe_merged_covers |> 
  left_join(env, by=c("releve_nr"="poradi")) -> spe_merged_covers_env

spe_merged_covers_env |>
  group_by(site) |>
  ggplot(aes(y = site, x = s_ph_h2o)) +
  geom_boxplot()

# 1. Výpočet průměrů na lokalitu
env_averages_region <- env |>
  select(-c(poradi, deg_lat, deg_lon)) |> 
  group_by(region) |>
  summarise(across(where(is.numeric), list(
    mean = \(x) mean(x, na.rm = TRUE),
    min = \(x) min(x, na.rm = TRUE),
    max = \(x) max(x, na.rm = TRUE)
  ))) |> 
  rename_with(
  .cols = ends_with("_mean"),
  .fn   = \(x) str_to_title(str_remove(x, "_mean"))
)

# 1. Výpočet průměrů na lokalitu
env_averages_site <- env |>
  select(-c(poradi, deg_lat, deg_lon)) |> 
  group_by(site) |>
  summarise(across(where(is.numeric), list(
    mean = \(x) mean(x, na.rm = TRUE),
    min = \(x) min(x, na.rm = TRUE),
    max = \(x) max(x, na.rm = TRUE)
  ))) |> 
  rename_with(
  .cols = ends_with("_mean"),
  .fn   = \(x) str_to_title(str_remove(x, "_mean"))
)

env_averages_region |> write_csv("Data/env_averages_region.csv")
env_averages_site |> write_csv("Data/env_averages_site.csv")

env_averages_region |> 
  mutate(region = fct_reorder(region, altitude_mean)) |> 
  ggplot(aes(x = altitude_mean, y = region)) +
  geom_linerange(aes(xmin = altitude_min, xmax = altitude_max), color = "grey60", linewidth = 0.8)+
  geom_point(size = 2.8, color = "#2b5c8f")+
  labs(
    title = "Mean Altitude of Relevés by Region",
    x = "Mean Altitude (m)",
    y = NULL
  )+
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8.5, face = "bold"),
    panel.grid.major.y = element_line(color = "grey92"),
    panel.grid.minor = element_blank()
  )

#definiton of groups of variables for boxplots
names(env_averages_region)

# 1. Číselník vybraných proměnných (název sloupce průměru -> hezký popisek v grafu + skupina)
vybrane_promenne <- tribble(
  ~var_mean,                               ~label,                                     ~skupina,
  # --- Geographical (3 proměnné) ---
  "Altitude",                              "Altitude (m a.s.l.)",                      "Geographical",
  "Exposition",                            "Exposition (°)",                           "Geographical",
  "Inclinatio",                            "Slope inclination (°)",                    "Geographical",

  # --- Climate (14 proměnných - CHELSA / Bioclim) ---
  "Bio1_annual_temperature_mean",          "Annual Mean Temp (Bio1, °C)",            "Climate",
  "Bio4_temperature_seasonality",          "Temp Seasonality (Bio4)",                  "Climate",
  "Bio5_max_temperature_warmest_month",    "Max Temp Warmest Month (Bio5, °C)",        "Climate",
  "Bio6_min_temperature_coldest_month",    "Min Temp Coldest Month (Bio6, °C)",        "Climate",
  "Bio7_temperature_annual_range",         "Temp Annual Range (Bio7, °C)",             "Climate",
  "Bio12_annual_precipitation",            "Annual Precipitation (Bio12, mm)",         "Climate",
  "Bio15_precipitation_seasonality",       "Precip Seasonality (Bio15)",               "Climate",
  "Bio18_precipitation_warmest_quarter",   "Precip Warmest Quarter (Bio18, mm)",       "Climate",
  "Ai_aridity_index",                      "Aridity Index (AI)",                       "Climate",
  "Cmi_climate_moisture_index_mean",       "Climate Moisture Index (CMI)",             "Climate",
  "Gsl_growing_season_length",             "Growing Season Length (days)",             "Climate",
  "Gdd5_growing_degree_days",              "Growing Degree Days >5°C (GDD5)",          "Climate",
  "Scd_snow_cover_days",                   "Snow Cover Days (days)",                   "Climate",
  "Vpd_vapor_pressure_deficit_mean",       "Vapor Pressure Deficit (VPD)",             "Climate",

  # --- Edaphic (15 proměnných - chemismus a fyzika půdy) ---
  "S_ph_h2o",                              "Soil pH (H2O)",                            "Edaphic",
  "Kond",                                  "Conductivity (µS/cm)",                     "Edaphic",
  "Toc",                                   "Total Organic Carbon (TOC)",               "Edaphic",
  "Tn",                                    "Total Nitrogen (TN)",                      "Edaphic",
  "Ppo43_miii",                            "Phosphorus (Mehlich III)",                 "Edaphic",
  "Ca_miii",                               "Calcium (Mehlich III)",                    "Edaphic",
  "Mg_miii",                               "Magnesium (Mehlich III)",                  "Edaphic",
  "K",                                     "Potassium (K)",                            "Edaphic",
  "Fe_miii",                               "Iron (Mehlich III)",                       "Edaphic",
  "Mn_miii",                               "Manganese (Mehlich III)",                  "Edaphic",
  "Cr_miii",                               "Chromium (Mehlich III)",                   "Edaphic",
  "Ni_miii",                               "Nickel (Mehlich III)",                     "Edaphic",
  "Co_miii",                               "Cobalt (Mehlich III)",                     "Edaphic",
  "Al_miii",                               "Aluminium (Mehlich III)",                  "Edaphic",
  "Soildepavg",                            "Mean Soil Depth (cm)",                     "Edaphic",

  # --- Vegetation & Microhabitat / VEGETAČNÍ (11 proměnných - pokryvnosti pater a povrchu) ---
  "Cov_trees",                             "Tree Layer Cover (%)",                     "Vegetation & Microhabitat",
  "Cov_shrubs",                            "Shrub Layer Cover (%)",                    "Vegetation & Microhabitat",
  "Cov_herbs",                             "Herb Layer Cover (%)",                     "Vegetation & Microhabitat",
  "Cov_mosses",                            "Moss Layer Cover (%)",                     "Vegetation & Microhabitat",
  "Cov_lichen",                            "Lichen Layer Cover (%)",                   "Vegetation & Microhabitat",
  "Cov_litter",                            "Litter Cover (%)",                         "Vegetation & Microhabitat",
  "Herb_high",                             "Mean Herb Height (cm)",                    "Vegetation & Microhabitat",
  "Rock",                                  "Rock Outcrops (%)",                        "Vegetation & Microhabitat",
  "Gravel",                                "Gravel Cover (%)",                         "Vegetation & Microhabitat",
  "Bare_soil",                             "Bare Soil Cover (%)",                      "Vegetation & Microhabitat",
  "Npp_net_primary_production",            "Net Primary Production (NPP)",             "Vegetation & Microhabitat"
) |> 
  mutate(
    # Automatické sestavení přesných názvů sloupců _min a _max (včetně opravy Bio1/2/8/9/10/11)
    base_lower = str_to_lower(var_mean) |> 
      str_replace("^(bio\\d+)_(.*)_mean$", "\\1_mean_\\2"),
    var_min = paste0(base_lower, "_min"),
    var_max = paste0(base_lower, "_max")
  )

env_long_region <- env_averages_region |> 
  mutate(region = fct_reorder(region, Altitude)) |> 
  # Vezmeme jen ty sloupce, které máme v číselníku vybrane_promenne
  select(region, all_of(c(vybrane_promenne$var_mean, vybrane_promenne$var_min, vybrane_promenne$var_max))) |> 
  # Sjednotíme názvy na stejný vzor (promenna___mean, promenna___min, promenna___max)
  rename_with(~ paste0(.x, "___mean"), all_of(vybrane_promenne$var_mean)) |> 
  rename_with(~ paste0(vybrane_promenne$var_mean[match(.x, vybrane_promenne$var_min)], "___min"), all_of(vybrane_promenne$var_min)) |> 
  rename_with(~ paste0(vybrane_promenne$var_mean[match(.x, vybrane_promenne$var_max)], "___max"), all_of(vybrane_promenne$var_max)) |> 
  pivot_longer(
    cols = -region,
    names_to = c("var_mean", ".value"),
    names_sep = "___"
  ) |> 
  left_join(vybrane_promenne, by = "var_mean") |> 
  mutate(label = fct_inorder(label))

env_long_site <- env_averages_site |> 
  mutate(site = fct_reorder(site, Altitude)) |> 
  # Vezmeme jen ty sloupce, které máme v číselníku vybrane_promenne
  select(site, all_of(c(vybrane_promenne$var_mean, vybrane_promenne$var_min, vybrane_promenne$var_max))) |> 
  # Sjednotíme názvy na stejný vzor (promenna___mean, promenna___min, promenna___max)
  rename_with(~ paste0(.x, "___mean"), all_of(vybrane_promenne$var_mean)) |> 
  rename_with(~ paste0(vybrane_promenne$var_mean[match(.x, vybrane_promenne$var_min)], "___min"), all_of(vybrane_promenne$var_min)) |> 
  rename_with(~ paste0(vybrane_promenne$var_mean[match(.x, vybrane_promenne$var_max)], "___max"), all_of(vybrane_promenne$var_max)) |> 
  pivot_longer(
    cols = -site,
    names_to = c("var_mean", ".value"),
    names_sep = "___"
  ) |> 
  left_join(vybrane_promenne, by = "var_mean") |> 
  mutate(label = fct_inorder(label))

# 3. Function for panel
vykresli_skupinu <- function(data, nazev_skupiny, y_var = region, pocet_sloupcu = 4) {
  data |> 
    filter(skupina == nazev_skupiny) |> 
    group_by(label) |>
    mutate(overall_mean = mean(mean, na.rm = TRUE)) |>
    ungroup() |>
    ggplot(aes(x = mean, y = {{ y_var }})) +
    geom_vline(aes(xintercept = overall_mean), color = "firebrick", linetype = "dashed", linewidth = 0.5, alpha = 0.7) +
    geom_linerange(aes(xmin = min, xmax = max), color = "grey65", linewidth = 0.7) +
    geom_point(size = 2.2, color = "#2b5c8f") +
    facet_wrap(~ label, scales = "free_x", ncol = pocet_sloupcu) +
    labs(
      subtitle = "Point = mean, line = range (min–max), dashed line = overall mean",
      x = NULL,
      y = NULL
    ) +
    theme_minimal() +
    theme(
  plot.title = element_text(size = 16, face = "bold"),
  plot.subtitle = element_text(size = 12, color = "grey30", margin = margin(b = 12)),
  strip.text = element_text(size = 10.5, face = "bold"),
  axis.text.y = element_text(size = 8.5, face = "bold"),
  axis.text.x = element_text(size = 8.5),
  panel.spacing.x = unit(1.2, "lines"),
  panel.spacing.y = unit(1, "lines")
)
}

#vykreslení pro regiony
geo_region <- vykresli_skupinu(env_long_region, "Geographical",y_var = region, pocet_sloupcu = 3)
clima_region <- vykresli_skupinu(env_long_region, "Climate", y_var = region, pocet_sloupcu = 4)
soil_region <- vykresli_skupinu(env_long_region, "Edaphic", y_var = region, pocet_sloupcu = 4)
vege_region <- vykresli_skupinu(env_long_region, "Vegetation & Microhabitat", y_var = region, pocet_sloupcu = 4)

#vykreslení pro lokality
geo_site <- vykresli_skupinu(env_long_site, "Geographical", y_var = site, pocet_sloupcu = 3)
clima_site <- vykresli_skupinu(env_long_site, "Climate", y_var = site, pocet_sloupcu = 4)
soil_site <- vykresli_skupinu(env_long_site, "Edaphic", y_var = site, pocet_sloupcu = 4)
vege_site <- vykresli_skupinu(env_long_site, "Vegetation & Microhabitat", y_var = site, pocet_sloupcu = 4)

graph_averages <- function(nazev_grafu, vyska = 16) {
  jmeno_souboru <- deparse(substitute(nazev_grafu))
  
  ggsave(
    filename = paste0("plots/", jmeno_souboru, "_characteristics.png"),
    plot = nazev_grafu,
    width = 14,
    height = vyska,
    units = "in",
    dpi = 300,
    bg = "white"
  )
}

graph_averages(geo_region)
graph_averages(clima_region)
graph_averages(soil_region)
graph_averages(vege_region)
graph_averages(geo_site)
graph_averages(clima_site)
graph_averages(soil_site)
graph_averages(vege_site)



# linear regression -------------------------------------------------------
library(broom)
library(psych)
library(vegan)
library(gordi)

richness <- spe_vas %>%
  ungroup() %>% 
  group_by(releve_nr) %>%
  summarise(n = n_distinct(species))

# Complete named vector of English labels for all soil chemistry variables
soil_labels <- c(
  # General soil properties & carbon/nitrogen
  "s_ph_h2o"   = "Soil pH (H2O)",
  "kond"       = "Conductivity (µS/cm)",
  "tn"         = "Total Nitrogen (TN)",
  "tc"         = "Total Carbon (TC)",
  "ic"         = "Inorganic Carbon (IC)",
  "toc"        = "Total Organic Carbon (TOC)",
  
  # Water-extractable nitrogen & phosphorus forms
  "nno2"       = "Nitrite-N (N-NO2-)",
  "nno3"       = "Nitrate-N (N-NO3-)",
  "nnh4"       = "Ammonium-N (N-NH4+)",
  "ppo43"      = "Phosphate-P (P-PO43-)",
  
  # Water-extractable ions
  "cl"         = "Chloride (Cl-)",
  "no2"        = "Nitrite (NO2-)",
  "no3"        = "Nitrate (NO3-)",
  "so42"       = "Sulfate (SO42-)",
  "hco3"       = "Bicarbonate (HCO3-)",
  "nh4"        = "Ammonium (NH4+)",
  "k"          = "Water-soluble K+",
  "na"         = "Water-soluble Na+",
  "mg2"        = "Water-soluble Mg2+",
  "ca2"        = "Water-soluble Ca2+",
  
  # Mehlich III extractable elements (plant-available nutrients & heavy metals)
  "ppo43_miii" = "Phosphate (Mehlich III)",
  "ca_miii"    = "Calcium (Mehlich III)",
  "mg_miii"    = "Magnesium (Mehlich III)",
  "fe_miii"    = "Iron (Mehlich III)",
  "mn_miii"    = "Manganese (Mehlich III)",
  "cu_miii"    = "Copper (Mehlich III)",
  "cr_miii"    = "Chromium (Mehlich III)",
  "ni_miii"    = "Nickel (Mehlich III)",
  "co_miii"    = "Cobalt (Mehlich III)",
  "al_miii"    = "Aluminium (Mehlich III)"
)

soil_vars <- names(soil_labels)

soil_model <- env |> 
  left_join(richness, by = c("poradi" = "releve_nr")) |> 
  select(poradi, n, all_of(soil_vars))

regression_results <- soil_model |>
  pivot_longer(cols = all_of(soil_vars), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  nest() |>
  mutate(
    model = map(data, ~ lm(n ~ value, data = .x)),
    tidied = map(model, tidy)
  ) |>
  unnest(tidied) |>
  filter(term == "value") |>
  select(variable, estimate, std.error, statistic, p.value) |>
  arrange(p.value)

# Display the full table with all columns
print(regression_results, n = 30)

# Merge data with p-values so we know which panels are significant
soil_plot_data <- soil_model |> 
  pivot_longer(cols = all_of(soil_vars), names_to = "variable", values_to = "value") |> 
  left_join(regression_results %>% select(variable, p.value), by = "variable") |> 
  mutate(is_significant = p.value < 0.05)

# Plot all variables in a multi-panel figure
ggplot(soil_plot_data, aes(x = value, y = n)) +
  geom_point(color = "black", size = 1) +
  # Draw linear regression line ONLY for statistically significant relationships (p < 0.05)
  geom_smooth(
    data = ~ filter(.x, is_significant == TRUE),
    method = "lm",
    color = "red",
    fill = "red",
    se = TRUE
  ) +
  facet_wrap(
    ~ variable, 
    scales = "free_x", 
    labeller = labeller(variable = soil_labels)
  ) +
  theme_bw() +
  labs(
    title = "Effects of Soil Chemistry on Vascular Plant Species Richness",
    x = "Soil Parameter Value",
    y = "Number of Species per Relevé"
  )

# log linear regression ---------------------------------------------------

soil_model_log <- env |> 
  left_join(richness, by = c("poradi" = "releve_nr")) |> 
  select(poradi, n, all_of(soil_vars)) |> 
  mutate(across(
    .cols = all_of(setdiff(soil_vars, "s_ph_h2o")), 
    .fns = ~ log10(.x + 1)
  ))

regression_results_log <- soil_model_log |>
  pivot_longer(cols = all_of(soil_vars), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  nest() |>
  mutate(
    model = map(data, ~ lm(n ~ value, data = .x)),
    tidied = map(model, tidy)
  ) |>
  unnest(tidied) |>
  filter(term == "value") |>
  select(variable, estimate, std.error, statistic, p.value) |>
  arrange(p.value)

# Print statistical results
print(regression_results_log, n = 30)

# Merge transformed data with p-values
soil_plot_data_log <- soil_model_log |>
  pivot_longer(cols = all_of(soil_vars), names_to = "variable", values_to = "value") |>
  left_join(regression_results_log |> select(variable, p.value), by = "variable") |>
  mutate(is_significant = p.value < 0.05)

# 5. Plot log-transformed variables in a multi-panel figure
ggplot(soil_plot_data_log, aes(x = value, y = n)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(
    data = ~ filter(.x, is_significant == TRUE),
    method = "lm",
    color = "red",
    fill = "red",
    se = TRUE
  ) +
  facet_wrap(
    ~ variable, 
    scales = "free_x",
    labeller = labeller(variable = soil_labels)
  ) +
  theme_bw() +
  labs(
    title = "Effects of Soil Chemistry (log10-transformed) on Vascular Plant Species Richness",
    x = "Log10(Value + 1) [Soil pH untransformed]",
    y = "Number of Species per Relevé")

# loglog linear regression  -----------------------------------------------

soil_model_loglog <- env |> 
  left_join(richness, by = c("poradi" = "releve_nr")) |> 
  select(poradi, n, all_of(soil_vars)) |> 
  mutate(
    n_log = log10(n), # Log10-transform species richness (y-axis)
    across(
      .cols = all_of(setdiff(soil_vars, "s_ph_h2o")), 
      .fns = ~ log10(.x + 1) # Log10(x + 1) for soil chemistry except pH
    ))

regression_results_loglog <- soil_model_loglog |>
  pivot_longer(cols = all_of(soil_vars), names_to = "variable", values_to = "value") |>
  group_by(variable) |>
  nest() |>
  mutate(
    model = map(data, ~ lm(n_log ~ value, data = .x)),
    tidied = map(model, tidy)
  ) |>
  unnest(tidied) |>
  filter(term == "value") |>
  select(variable, estimate, std.error, statistic, p.value) |>
  arrange(p.value)

# Print statistical results
print(regression_results_loglog, n = 30)

# Merge transformed data with p-values
soil_plot_data_loglog <- soil_model_loglog |>
  pivot_longer(cols = all_of(soil_vars), names_to = "variable", values_to = "value") |>
  left_join(regression_results_loglog |> select(variable, p.value), by = "variable") |>
  mutate(is_significant = p.value < 0.05)

# Plot log-log transformed data in a multi-panel figure
ggplot(soil_plot_data_loglog, aes(x = value, y = n_log)) +
  geom_point(color = "black", size = 1) +
  geom_smooth(
    data = ~ filter(.x, is_significant == TRUE),
    method = "lm",
    color = "red",
    fill = "red",
    se = TRUE
  ) +
  facet_wrap(
    ~ variable, 
    scales = "free_x",
    labeller = labeller(variable = soil_labels)
  ) +
  theme_bw() +
  labs(
    title = "Effects of Soil Chemistry on Vascular Plant Species Richness (Log-Log)",
    x = "Log10(Soil Parameter + 1) [Soil pH untransformed]",
    y = "Log10(Number of Species per Relevé)"
  )


# ordination --------------------------------------------------------------

spe_wide <- spe_vas |>
  group_by(releve_nr, species) |>
  summarise(cover_perc_new = sum(cover_perc_new, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(
    names_from = species,
    values_from = cover_perc_new,
    values_fill = 0
  ) |>
  arrange(releve_nr)

spe_matrix <- spe_wide |>
  ungroup() |> 
  select(-releve_nr) |>
  sqrt()

env_gordi <- env |>
  filter(poradi %in% spe_wide$releve_nr) |>
  arrange(poradi) |>
  rename(
    "Soil pH"      = s_ph_h2o,
    "Conductivity" = kond,
    "TOC"          = toc,
    "TN"           = tn,
    "P"            = ppo43_miii,
    "Ca"           = ca_miii,
    "Mg"           = mg_miii,
    "Ni"           = ni_miii,
    "Cr"           = cr_miii,
    "Co"           = co_miii,)

# PCoA ordination model
pcoa_bc <- capscale(spe_matrix ~ 1, distance = "bray", sqrt.dist = TRUE)

summary(eigenvals(pcoa_bc))[, 1:4]

gordi_read(m = pcoa_bc, spe = spe_matrix, env = env_gordi, correlation = TRUE, show = F) |>
  gordi_fit(slice_max = 30, abs_frequency = 5) |>
  gordi_species(label = TRUE, colour = "#5C6B53") |>
  gordi_label(what = "species", shortcut = "upper.lower", repel_label = TRUE, max.overlaps = Inf, show.legend = FALSE) |>
  gordi_corr(variables = c("Soil pH", "Conductivity", "TOC", "P", "Ca", "Mg", "Ni", "Cr"), 
             show_label = TRUE, 
             repel_label = TRUE, 
             colour = "red") |> 
  gordi_plot() + 
  theme_bw() +
  labs(
    title = "PCoA of Vascular Plant Communities with Soil Chemistry Vectors")

