# LIBRARIES

library(haven)
library(moderndive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gt)
library(effectsize)

#DATA
datacp <- read_sav("datacp.sav")

datacp <- datacp %>% 
  filter(Progress > 95) %>% 
  glimpse()

class(datacp$A_idioma_)
str(datacp$A_idioma_)

#RECODS
datacp <- datacp %>%
  mutate(
    simp = case_when(A_idioma_ == 1 ~ A_simp_, A_idioma_ == 2 ~ A_simp_.0, TRUE ~ NA_real_),
    antisimp = case_when(A_idioma_ == 1 ~ A_antisimp_, A_idioma_ == 2 ~ A_antisimp_.0, TRUE ~ NA_real_),
    gen = case_when(A_idioma_ == 1 ~ A_gen_, A_idioma_ == 2 ~ A_gen_.0, TRUE ~ NA_real_),
    confia = case_when(A_idioma_ == 1 ~ A_confia__4, A_idioma_ == 2 ~ A_confia__4.0, TRUE ~ NA_real_),
    ideo = case_when(A_idioma_ == 1 ~ A_ideo__1, A_idioma_ == 2 ~ A_ideo__1.0, TRUE ~ NA_real_),
    indep = case_when(A_idioma_ == 1 ~ A_indep_, A_idioma_ == 2 ~ A_indep_.0, TRUE ~ NA_real_),
    credi = case_when(A_idioma_ == 1 ~ A_credi__3, A_idioma_ == 2 ~ A_credi__3.0, TRUE ~ NA_real_),
    share = case_when(A_idioma_ == 1 ~ A_share__1, A_idioma_ == 2 ~ A_share__1.0, TRUE ~ NA_real_),
    confpar_psc    = case_when(A_idioma_ == 1 ~ A_confpar__1, A_idioma_ == 2 ~ A_confpar__1.0),
    confpar_junts  = case_when(A_idioma_ == 1 ~ A_confpar__2, A_idioma_ == 2 ~ A_confpar__2.0),
    confpar_erc    = case_when(A_idioma_ == 1 ~ A_confpar__3, A_idioma_ == 2 ~ A_confpar__3.0),
    confpar_pp     = case_when(A_idioma_ == 1 ~ A_confpar__4, A_idioma_ == 2 ~ A_confpar__4.0),
    confpar_vox    = case_when(A_idioma_ == 1 ~ A_confpar__5, A_idioma_ == 2 ~ A_confpar__5.0),
    confpar_comuns = case_when(A_idioma_ == 1 ~ A_confpar__6, A_idioma_ == 2 ~ A_confpar__6.0),
    confpar_cup    = case_when(A_idioma_ == 1 ~ A_confpar__7, A_idioma_ == 2 ~ A_confpar__7.0),
    confpar_ac     = case_when(A_idioma_ == 1 ~ A_confpar__8, A_idioma_ == 2 ~ A_confpar__8.0)
  ) %>%
  mutate(across(starts_with("confpar_"), ~ replace_na(.x, 0)))  # NAs a 0

datacp <- datacp %>%
  mutate(
    simp_red = case_when(
      simp == 1 ~ "PSC", simp == 2 ~ "Junts", simp == 3 ~ "ERC",
      simp == 4 ~ "PP",  simp == 5 ~ "Vox",   simp == 6 ~ "Comuns",
      simp == 7 ~ "CUP", simp == 8 ~ "AC",    simp == 9 ~ "Otros",
      simp == 10 ~ "Ninguno", simp == 11 ~ NA_character_
    ),
    antisimp_red = case_when(
      antisimp == 1 ~ "PSC", antisimp == 2 ~ "Junts", antisimp == 3 ~ "ERC",
      antisimp == 4 ~ "PP",  antisimp == 5 ~ "Vox",   antisimp == 6 ~ "Comuns",
      antisimp == 7 ~ "CUP", antisimp == 8 ~ "AC",    antisimp == 9 ~ "Otros",
      antisimp == 10 ~ "Ninguno", antisimp == 11 ~ NA_character_
    )
  )

datacp <- datacp %>%
  mutate(
    ideo_endo = case_when(
      simp_red %in% c("Comuns", "CUP") ~ "Extrema izquierda",
      simp_red %in% c("PSC", "ERC")    ~ "Centro izquierda",
      simp_red %in% c("Junts", "PP")   ~ "Centro derecha",
      simp_red %in% c("Vox", "AC")     ~ "Extrema derecha",
      TRUE ~ NA_character_
    ),
    ideo_exo = case_when(
      antisimp_red %in% c("Comuns", "CUP") ~ "Extrema izquierda",
      antisimp_red %in% c("PSC", "ERC")    ~ "Centro izquierda",
      antisimp_red %in% c("Junts", "PP")   ~ "Centro derecha",
      antisimp_red %in% c("Vox", "AC")     ~ "Extrema derecha",
      TRUE ~ NA_character_
    )
  )

datacp <- datacp %>%
  mutate(
    ideo_red = case_when(
      ideo < 3 ~ "Extrema izquierda",
      ideo >= 3 & ideo < 5 ~ "Centro izquierda",
      ideo > 5 & ideo <= 8 ~ "Centro derecha",
      ideo > 8 ~ "Extrema derecha"
    ),
    indep_red = case_when(
      indep == 1 ~ "Región de España",
      indep == 2 ~ "Comunidad autónoma",
      indep == 3 ~ "Estado federal",
      indep == 4 ~ "Estado independiente"
    ),
    gen_red = case_when(
      gen == 1 ~ "Hombre",
      gen == 2 ~ "Mujer"
    ),
    # Para análisis de subgrupos extremos (H3)
    extremo_ideo = ifelse(ideo_red %in% c("Extrema izquierda", "Extrema derecha"), "Extremo", "Moderado"),
    extremo_indep = ifelse(indep_red %in% c("Región de España", "Estado independiente"), "Extremo", "Moderado")
  )

datacp <- datacp %>%
  mutate(
    tema = ifelse(grepl("VIV", endo_cond), "Vivienda", "Inmigración"),
    label = ifelse(grepl("1$", endo_cond), "Con IA", "Sin IA") # $ para coger el último dígito
  )

datacp <- datacp %>%
  mutate(
    conf_endo = case_when(
      simp_red == "PSC" ~ confpar_psc, simp_red == "Junts" ~ confpar_junts,
      simp_red == "ERC" ~ confpar_erc, simp_red == "PP" ~ confpar_pp,
      simp_red == "Vox" ~ confpar_vox, simp_red == "Comuns" ~ confpar_comuns,
      simp_red == "CUP" ~ confpar_cup, simp_red == "AC" ~ confpar_ac,
      TRUE ~ NA_real_
    ),
    conf_exo = case_when(
      antisimp_red == "PSC" ~ confpar_psc, antisimp_red == "Junts" ~ confpar_junts,
      antisimp_red == "ERC" ~ confpar_erc, antisimp_red == "PP" ~ confpar_pp,
      antisimp_red == "Vox" ~ confpar_vox, antisimp_red == "Comuns" ~ confpar_comuns,
      antisimp_red == "CUP" ~ confpar_cup, antisimp_red == "AC" ~ confpar_ac,
      TRUE ~ NA_real_
    ),
    dif_conf = conf_endo - conf_exo   # positivo = sesgo endogrupal
  )

datacp <- datacp %>%
  mutate(label_bin = ifelse(label == "Con IA", 1, 0))

datacp <- datacp %>%
  mutate(
    simp_red1 = case_when(
      simp_red %in% c("PSC", "ERC")         ~ "Izquierda",
      simp_red %in% c("Junts", "PP")        ~ "Derecha",
      simp_red %in% c("Comuns", "CUP")      ~ "Extrema Izquierda",
      simp_red %in% c("Vox", "AC")          ~ "Extrema Derecha",
      simp_red %in% c("Otros", "Ninguno")   ~ NA_character_,
      TRUE ~ NA_character_
    ),
    antisimp_red1 = case_when(
      antisimp_red %in% c("PSC", "ERC")         ~ "Izquierda",
      antisimp_red %in% c("Junts", "PP")        ~ "Derecha",
      antisimp_red %in% c("Comuns", "CUP")      ~ "Extrema Izquierda",
      antisimp_red %in% c("Vox", "AC")          ~ "Extrema Derecha",
      antisimp_red %in% c("Otros", "Ninguno")   ~ NA_character_,
      TRUE ~ NA_character_
    )
  )

datacp <- datacp %>%
  mutate(
    ideo_endo = factor(ideo_endo, levels = c("Extrema izquierda", "Centro izquierda", "Centro derecha", "Extrema derecha")),
    ideo_exo  = factor(ideo_exo,  levels = c("Extrema izquierda", "Centro izquierda", "Centro derecha", "Extrema derecha")),
    ideo_red  = factor(ideo_red,  levels = c("Extrema izquierda", "Centro izquierda", "Centro derecha", "Extrema derecha")),
    indep_red = factor(indep_red, levels = c("Estado independiente", "Estado federal", "Comunidad autónoma", "Región de España"))
  )
