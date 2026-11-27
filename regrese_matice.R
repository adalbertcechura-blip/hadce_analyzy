#Regrese

library(tidyverse)
library(janitor)
library(readxl)
library(vegan)
library(GGally)
library(corrplot)
library(broom)

# 1. PŘÍPRAVA DAT (Zkrácená verze importu z hlavního skriptu) -----------------

env <- read_delim("data/serp_env.csv", delim = ";", locale = locale(encoding = "Windows-1250")) %>% clean_names()
localities <- read_excel("Data/Hadce-lokality-regiony-strucne-nazvy.xlsx") %>% clean_names()
spe <- read_csv("data/spe_merged_covers.csv") %>% 
  clean_names()
serp_spe <- read_excel("Data/hadcovi_specialisti_edit.xlsx")

# Propojení s lokalitami
env <- env %>% 
  left_join(localities, by = c("locality_detail" = "jmeno_od_vojty"))

# 2. VÝPOČET NOVÝCH PROMĚNNÝCH (Circular, Ratio, Specialists) ----------------

env <- env %>%
  mutate(
    # TOC/TN Ratio
    toc_tn = toc / tn,
    #heat load index
    heat_load = (0.339+0.808*cos(deg_lat*pi/180)*cos(inclinatio*pi/180)-0.196*sin(deg_lat*pi/180)*sin(inclinatio*pi/180)-0.482*cos((abs(180-abs(exposition-225)))*pi/180)*sin(inclinatio*pi/180)
  ))

# Výpočet metrik specialistů (kategorie 1 a 2)
spe <- spe %>% # Předpokládáme 'spe' z hadce_analyzy.R (long format s cover_perc_new)
  left_join(serp_spe, by = c("species" = "species_name")) %>%
  group_by(releve_nr) %>%
  summarise(
    richness_all = n_distinct(species),
    # Počty specialistů
    n_spec_oblig = sum(serpentine_affinity == 2, na.rm = TRUE),
    n_spec_facult = sum(serpentine_affinity == 1, na.rm = TRUE),
    n_spec_total = sum(serpentine_affinity %in% c(1, 2), na.rm = TRUE),
    # Pokryvnosti specialistů
    cov_spec_total = sum(cover_perc_new[serpentine_affinity %in% c(1, 2)], na.rm = TRUE),
    # Podíly
    prop_n_spec = n_spec_total / richness_all,
    prop_cov_spec = cov_spec_total / sum(cover_perc_new, na.rm = TRUE)
  )

# Finální dataset pro regrese
env_all <- env %>%
  left_join(spe, by = c("poradi" = "releve_nr"))

write_csv(env_all, "data/env_all.csv") # Uložení pro další analýzy

# KORELAČNÍ MATICE A REDUKCE PROMĚNNÝCH -----------------------------------

# 1. Klimatické proměnné (sloupce 13 až 88)

to.cor <- env[, 13:88]

cor_mat <- round(cor(to.cor, use = "pairwise.complete.obs"), 3)

cor_mat[upper.tri(cor_mat, diag = TRUE)] <- NA

write_csv2(as_tibble(cor_mat, rownames = "variable"), "Data/cor_klima_triangle.csv", na = "")
write_csv2(as_tibble(cor_mat, rownames = "variable"), "Data/cor_klima_matrix.csv")

# Výběr proměnných pro regresní modely
clima_selected <- c("bio1_mean_annual_temperature", 
                    "bio2_mean_diurnal_range", "bio8_mean_temperature_wettest_quarter", "bio18_precipitation_warmest_quarter",
                    "rsds_solar_radiation_mean", "fcf_frost_change_frequency", 
                    "wind_wind_speed_mean")

# Funkce pro korelační matici
panel.cor <- function(x, y, method= "pearson", digits=2, cex.cor=5, no.col=FALSE) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use = "pairwise.complete.obs", method=method)
  ra <- cor.test(x, y, method=method)$p.value
  txt <- round(r, digits)
  prefix <- ""
  if(ra <= 0.1) prefix <- "."
  if(ra <= 0.05) prefix <- "*"
  if(ra <= 0.01) prefix <- "**"
  if(ra <= 0.001) prefix <- "***"
  
  if(no.col) {
    color <- 1
    if(r < 0) { if(ra <= 0.001) sig <- 4 else sig <- 3 }
    else { if(ra <= 0.001) sig <- 2 else sig <- 1 }
  } else {
    sig <- 1
    if(ra <= 0.001) sig <- 2
    color <- 2
    if(r < 0) color <- 4
  }
  txt <- paste(txt, prefix, sep="\n")
  text(0.5, 0.5, txt, cex = cex.cor, font=sig, col=color)
}

panel.hist <- function(x, no.col=FALSE, cex=10, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  his <- hist(x, plot=FALSE)
  breaks <- his$breaks; nB <- length(breaks)
  y <- his$counts
  y <- y/max(y)
  if(no.col) rect(breaks[-nB], 0, breaks[-1], y, col="gray", ...)
  else rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.smooth <- function (x, y, col=par("col"), bg=NA, pch=par("pch"), 
                          cex=5, col.smooth="black", span=2/3, iter=3, ...)  {
  points(x, y, pch=pch, col=col, bg=bg, cex=cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth, ...)
}

final_cor <- env_all |> 
  select(
    all_of(clima_selected), 
    toc_tn, n_spec_total, cov_spec_total, prop_n_spec, prop_cov_spec, 
    s_ph_h2o, altitude, richness_all
  ) |> 
  set_names(c(
    "bio1", "bio2", "bio8", "bio18", "rsds", "fcf", "wind", 
    "toc_tn", "n_spec", "cov_spec", "p_n_spec", "p_cov_spec", 
    "pH", "altitude", "richness"
  ))

png("plots/cor_full.png", width = 8000, height = 8000, res = 300)
pairs(final_cor, 
      lower.panel = panel.smooth, 
      upper.panel = panel.cor, 
      diag.panel  = panel.hist,
      cex.labels  = 3,      # Velikost názvů v diagonále
      label.pos   = 0.5, 
      main        = "Correlation Matrix: Selected Variables")
dev.off()

warnings(50)
