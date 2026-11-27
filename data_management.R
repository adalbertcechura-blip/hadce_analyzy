# SKRIPT PRO NAČTENÍ A PŘÍPRAVU DAT

library(tidyverse)
library(janitor)
library(readxl)
library(readr)

# 1. Import surových dat --------------------------------------------------

# Environmentální data
env_raw <- read_delim("data/serp_env.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE, 
                      locale = locale(encoding = "Windows-1250"))

# Druhová data
spe_raw <- read_csv("data/serp_spe.csv") %>% 
  clean_names()

# číselník
nomenclature <- read_csv("data/nomenclature_check.csv")

# Převodní tabulka pokryvností
cover_scale <- read_csv("data/table_cover.csv") %>% clean_names()

# Informace o lokalitách a regionech
localities <- read_excel("Data/Hadce-lokality-regiony-strucne-nazvy.xlsx", sheet = "List1") %>% 
  clean_names()

# Seznam serpentinových specialistů
serp_specialists <- read_excel("Data/hadcovi_specialisti_edit.xlsx")


# 2. Úprava environmentálních dat -----------------------------------------

env <- env_raw %>%
  left_join(localities, by = c("locality_detail" = "jmeno_od_vojty"))


# 3. Úprava druhových dat a nomenklatury ----------------------------------

spe <- spe_raw %>% 
  left_join(nomenclature, by = "species_nr") %>% 
  select(releve_nr, cover_code, layer, species, nonvascular) 


# 4. Přepočet pokryvností -------------------------------------------------

# Připojení procentuálních hodnot pokryvnosti
spe <- spe %>%
  left_join(env %>% select(poradi), by = c("releve_nr" = "poradi")) %>% 
  mutate(coverscale = "02") %>% # Braun-Blanquet scale code
  left_join(cover_scale %>% select(coverscale, cover_code, cover_perc), 
            by = c("coverscale", "cover_code"))


# 5. Slučování pater (Merging layers) -------------------------------------

# Funkce pro kombinovanou pokryvnost (Fischer 1937)
combine_cover <- function(x){
  while (length(x) > 1){
    x[2] <- x[1] + (100 - x[1]) * x[2] / 100
    x <- x[-1]
  }
  return(x)
}

# Sloučení druhů vyskytujících se ve více patrech v jednom snímku
spe_merged <- spe %>%    
  group_by(releve_nr, species, nonvascular) %>%    
  summarize(cover_perc_new = combine_cover(cover_perc), .groups = "drop")


# 6. Integrace specialistů a prostředí ------------------------------------

# Přidání afinity k hadcům a regionálních informací
spe <- spe_merged %>%
  left_join(serp_specialists, by = c("species" = "species_name")) %>%
  left_join(env %>% select(poradi, region, site, s_ph_h2o, altitude), 
            by = c("releve_nr" = "poradi"))

# Rozdělení na cévnaté a nevévnaté rostliny
spe_vas <- spe %>% filter(nonvascular == 0)
spe_nonv <- spe %>% filter(nonvascular == 1)


# 7. Výpočet bohatosti (Richness) -----------------------------------------

richness <- spe_vas %>%
  group_by(releve_nr) %>%
  summarise(n = n_distinct(species), .groups = "drop")


# 8. Export zpracovaných dat ----------------------------------------------

save(env, spe, spe_vas, spe_nonv, richness, file = "data/processed_data.RData")

# Finální vyčištění objektů v prostředí (volitelné)
# rm(env_raw, spe_raw, cover_scale, nomenclature, spe_merged)

