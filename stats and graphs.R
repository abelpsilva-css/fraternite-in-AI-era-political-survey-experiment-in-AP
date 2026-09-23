#STATS

#### REGRESIONES


# 1. Filtrar solo participantes con endogrupo definido
#    (excluimos "Otros", "Ninguno", NS/NC)
dat_reg <- datacp %>%
  filter(!is.na(ideo_endo), !is.na(ideo_exo))   # solo los que sí tienen ideología endo/exo

# 2. Variable dummy para mujer
dat_reg <- dat_reg %>%
  mutate(mujer = ifelse(gen_red == "Mujer", 1, 0))

# 3. Tratar el independentismo sin NAs
#    indep tiene valores 1-5; 5 = NS/NC. Lo convertimos en factor con todas las categorías.
dat_reg <- dat_reg %>%
  mutate(indep_factor = factor(
    case_when(
      indep == 4 ~ "Estado independiente",
      indep == 3 ~ "Estado federal",
      indep == 2 ~ "Comunidad autónoma",
      indep == 1 ~ "Región de España",
      indep == 5 ~ NA_character_,
      TRUE ~ NA_character_
    ),
    levels = c("Estado independiente", "Estado federal", "Comunidad autónoma", "Región de España")
  ))

# 4. Ajuste de los modelos lineales (usamos factor() para las variables categóricas)
lm_credi <- lm(credi ~ label_bin + confia + mujer + 
                 label_bin:confia + label_bin:mujer + 
                 ideo + factor(indep_factor) + factor(ideo_endo) + factor(ideo_exo),
               data = dat_reg)

lm_share <- lm(share ~ label_bin + confia + mujer + 
                 label_bin:confia + label_bin:mujer + 
                 ideo + factor(indep_factor) + factor(ideo_endo) + factor(ideo_exo),
               data = dat_reg)

lm_dif_conf <- lm(dif_conf ~ label_bin + confia + mujer + 
                    label_bin:confia + label_bin:mujer + 
                    ideo + factor(indep_factor) + factor(ideo_endo) + factor(ideo_exo),
                  data = dat_reg)

# 5. Diagnóstico rápido (opcional)
cat("N en regresiones:", nrow(dat_reg), "\n")

# 6. Visualización compacta de los coeficientes
library(texreg)
screenreg(list(lm_credi, lm_share, lm_dif_conf),
          custom.model.names = c("Credibilidad", "Intención compartir", 
                                 "Dif. confianza (PA)"),
          single.row = TRUE,
          digits = 2)


wordreg(
  list(lm_credi, lm_share, lm_dif_conf),
  file = "tabla_regresiones.docx",   # <- aquí el nombre del archivo
  custom.model.names = c("Credibilidad", "Intención compartir", 
                         "Dif. confianza (PA)"),
  single.row = TRUE,
  digits = 2
)



### MEDIAS DESCRIPTIVAS POR CONDICIÓN
tabla_medias <- datacp %>%
  group_by(tema, label) %>%
  summarise(
    n = n(),
    media_credi = mean(credi, na.rm = TRUE),
    sd_credi = sd(credi, na.rm = TRUE),
    media_share = mean(share, na.rm = TRUE),
    sd_share = sd(share, na.rm = TRUE),
    media_dif_conf = mean(dif_conf, na.rm = TRUE),
    sd_dif_conf = sd(dif_conf, na.rm = TRUE),
    .groups = "drop"
  )

# Convertir a formato largo para la tabla compacta
tabla_resumen <- tabla_medias %>%
  pivot_longer(
    cols = c(media_credi, media_share, media_dif_conf),
    names_to = "variable",
    values_to = "media"
  ) %>%
  left_join(
    tabla_d %>% rename(variable = var),
    by = c("tema", "variable")
  )

### COHEN D


datacp <- datacp %>%
  mutate(label_bin = ifelse(label == "Con IA", 1, 0))

datacp <- datacp %>%
  mutate(label_f = factor(label_bin, levels = c(0, 1), labels = c("Sin IA", "Con IA")))

calc_d <- function(data, var_resp) {
  d_res <- cohens_d(as.formula(paste(var_resp, "~ label_f")), 
                    data = data, pooled_sd = TRUE)
  data.frame(
    var = var_resp,
    d   = d_res$Cohens_d,
    CI_low = d_res$CI_low,
    CI_high = d_res$CI_high,
    row.names = NULL
  )
}

d_global <- bind_rows(
  calc_d(datacp, "credi"),
  calc_d(datacp, "share"),
  calc_d(datacp, "dif_conf")
) %>% mutate(tema = "Global")

d_tema <- bind_rows(
  datacp %>% filter(tema == "Vivienda") %>% calc_d("credi") %>% mutate(tema = "Vivienda"),
  datacp %>% filter(tema == "Vivienda") %>% calc_d("share") %>% mutate(tema = "Vivienda"),
  datacp %>% filter(tema == "Vivienda") %>% calc_d("dif_conf") %>% mutate(tema = "Vivienda"),
  datacp %>% filter(tema == "Inmigración") %>% calc_d("credi") %>% mutate(tema = "Inmigración"),
  datacp %>% filter(tema == "Inmigración") %>% calc_d("share") %>% mutate(tema = "Inmigración"),
  datacp %>% filter(tema == "Inmigración") %>% calc_d("dif_conf") %>% mutate(tema = "Inmigración")
)

tabla_d <- bind_rows(d_global, d_tema)



d_global <- bind_rows(
  calc_d(datacp, "credi"),
  calc_d(datacp, "share"),
  calc_d(datacp, "dif_conf")
) %>% mutate(tema = "Global")

d_tema <- bind_rows(
  datacp %>% filter(tema == "Vivienda") %>% calc_d("credi") %>% mutate(tema = "Vivienda"),
  datacp %>% filter(tema == "Vivienda") %>% calc_d("share") %>% mutate(tema = "Vivienda"),
  datacp %>% filter(tema == "Vivienda") %>% calc_d("dif_conf") %>% mutate(tema = "Vivienda"),
  datacp %>% filter(tema == "Inmigración") %>% calc_d("credi") %>% mutate(tema = "Inmigración"),
  datacp %>% filter(tema == "Inmigración") %>% calc_d("share") %>% mutate(tema = "Inmigración"),
  datacp %>% filter(tema == "Inmigración") %>% calc_d("dif_conf") %>% mutate(tema = "Inmigración")
)

tabla_d <- bind_rows(d_global, d_tema)

d_confia <- bind_rows(
  datacp %>% filter(confia_grupo == "Alta") %>% calc_d("credi") %>% mutate(subgrupo = "Alta"),
  datacp %>% filter(confia_grupo == "Alta") %>% calc_d("share") %>% mutate(subgrupo = "Alta"),
  datacp %>% filter(confia_grupo == "Alta") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Alta"),
  datacp %>% filter(confia_grupo == "Baja") %>% calc_d("credi") %>% mutate(subgrupo = "Baja"),
  datacp %>% filter(confia_grupo == "Baja") %>% calc_d("share") %>% mutate(subgrupo = "Baja"),
  datacp %>% filter(confia_grupo == "Baja") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Baja")
)

d_ideo_4cat <- bind_rows(
  datacp %>% filter(ideo_red == "Extrema izquierda") %>% calc_d("credi") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_red == "Extrema izquierda") %>% calc_d("share") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_red == "Extrema izquierda") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_red == "Centro izquierda") %>% calc_d("credi") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_red == "Centro izquierda") %>% calc_d("share") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_red == "Centro izquierda") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_red == "Centro derecha") %>% calc_d("credi") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_red == "Centro derecha") %>% calc_d("share") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_red == "Centro derecha") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_red == "Extrema derecha") %>% calc_d("credi") %>% mutate(subgrupo = "Extrema derecha"),
  datacp %>% filter(ideo_red == "Extrema derecha") %>% calc_d("share") %>% mutate(subgrupo = "Extrema derecha"),
  datacp %>% filter(ideo_red == "Extrema derecha") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Extrema derecha")
)

d_indep_4cat <- bind_rows(
  datacp %>% filter(indep_red == "Región de España") %>% calc_d("credi") %>% mutate(subgrupo = "Región de España"),
  datacp %>% filter(indep_red == "Región de España") %>% calc_d("share") %>% mutate(subgrupo = "Región de España"),
  datacp %>% filter(indep_red == "Región de España") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Región de España"),
  datacp %>% filter(indep_red == "Comunidad autónoma") %>% calc_d("credi") %>% mutate(subgrupo = "Comunidad autónoma"),
  datacp %>% filter(indep_red == "Comunidad autónoma") %>% calc_d("share") %>% mutate(subgrupo = "Comunidad autónoma"),
  datacp %>% filter(indep_red == "Comunidad autónoma") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Comunidad autónoma"),
  datacp %>% filter(indep_red == "Estado federal") %>% calc_d("credi") %>% mutate(subgrupo = "Estado federal"),
  datacp %>% filter(indep_red == "Estado federal") %>% calc_d("share") %>% mutate(subgrupo = "Estado federal"),
  datacp %>% filter(indep_red == "Estado federal") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Estado federal"),
  datacp %>% filter(indep_red == "Estado independiente") %>% calc_d("credi") %>% mutate(subgrupo = "Estado independiente"),
  datacp %>% filter(indep_red == "Estado independiente") %>% calc_d("share") %>% mutate(subgrupo = "Estado independiente"),
  datacp %>% filter(indep_red == "Estado independiente") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Estado independiente")
)

d_genero <- bind_rows(
  datacp %>% filter(gen_red == "Mujer") %>% calc_d("credi") %>% mutate(subgrupo = "Mujer"),
  datacp %>% filter(gen_red == "Mujer") %>% calc_d("share") %>% mutate(subgrupo = "Mujer"),
  datacp %>% filter(gen_red == "Mujer") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Mujer"),
  datacp %>% filter(gen_red == "Hombre") %>% calc_d("credi") %>% mutate(subgrupo = "Hombre"),
  datacp %>% filter(gen_red == "Hombre") %>% calc_d("share") %>% mutate(subgrupo = "Hombre"),
  datacp %>% filter(gen_red == "Hombre") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Hombre")
)

d_ideo_endo <- bind_rows(
  datacp %>% filter(ideo_endo == "Extrema izquierda") %>% calc_d("credi") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_endo == "Extrema izquierda") %>% calc_d("share") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_endo == "Extrema izquierda") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_endo == "Centro izquierda") %>% calc_d("credi") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_endo == "Centro izquierda") %>% calc_d("share") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_endo == "Centro izquierda") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_endo == "Centro derecha") %>% calc_d("credi") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_endo == "Centro derecha") %>% calc_d("share") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_endo == "Centro derecha") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_endo == "Extrema derecha") %>% calc_d("credi") %>% mutate(subgrupo = "Extrema derecha"),
  datacp %>% filter(ideo_endo == "Extrema derecha") %>% calc_d("share") %>% mutate(subgrupo = "Extrema derecha"),
  datacp %>% filter(ideo_endo == "Extrema derecha") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Extrema derecha")
)

d_ideo_exo <- bind_rows(
  datacp %>% filter(ideo_exo == "Extrema izquierda") %>% calc_d("credi") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_exo == "Extrema izquierda") %>% calc_d("share") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_exo == "Extrema izquierda") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Extrema izquierda"),
  datacp %>% filter(ideo_exo == "Centro izquierda") %>% calc_d("credi") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_exo == "Centro izquierda") %>% calc_d("share") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_exo == "Centro izquierda") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Centro izquierda"),
  datacp %>% filter(ideo_exo == "Centro derecha") %>% calc_d("credi") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_exo == "Centro derecha") %>% calc_d("share") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_exo == "Centro derecha") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Centro derecha"),
  datacp %>% filter(ideo_exo == "Extrema derecha") %>% calc_d("credi") %>% mutate(subgrupo = "Extrema derecha"),
  datacp %>% filter(ideo_exo == "Extrema derecha") %>% calc_d("share") %>% mutate(subgrupo = "Extrema derecha"),
  datacp %>% filter(ideo_exo == "Extrema derecha") %>% calc_d("dif_conf") %>% mutate(subgrupo = "Extrema derecha")
)



calc_media_subgrupo <- function(data, var_subgrupo, nombre_hipotesis) {
  data %>%
    filter(!is.na(.data[[var_subgrupo]]), !is.na(label)) %>%
    group_by(subgrupo = .data[[var_subgrupo]], label) %>%
    summarise(
      n_credi = sum(!is.na(credi)),
      media_credi = mean(credi, na.rm = TRUE),
      n_share = sum(!is.na(share)),
      media_share = mean(share, na.rm = TRUE),
      n_dif = sum(!is.na(dif_conf)),
      media_dif_conf = mean(dif_conf, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = starts_with("media_"),
      names_to = "var",
      values_to = "media"
    ) %>%
    mutate(
      var = gsub("media_", "", var),
      hipotesis = nombre_hipotesis,
      n = case_when(
        var == "credi"    ~ n_credi,
        var == "share"    ~ n_share,
        var == "dif_conf" ~ n_dif
      )
    ) %>%
    select(hipotesis, subgrupo, label, var, media, n)
}

medias_confia <- calc_media_subgrupo(datacp, "confia_grupo", "Confianza en IA")
medias_ideo   <- calc_media_subgrupo(datacp, "ideo_red", "Ideología")
medias_indep  <- calc_media_subgrupo(datacp, "indep_red", "Eje territorial")
medias_gen    <- calc_media_subgrupo(datacp, "gen_red", "Género")

medias_todas <- bind_rows(medias_confia, medias_ideo, medias_indep, medias_gen)

medias_wide <- medias_todas %>%
  pivot_wider(names_from = label, values_from = c(media, n)) %>%
  rename(
    media_sin = `media_Sin IA`,
    media_con = `media_Con IA`,
    n_sin     = `n_Sin IA`,
    n_con     = `n_Con IA`
  ) %>%
  mutate(n_total = n_sin + n_con)

d_todos_con_medias <- d_todos %>%
  left_join(medias_wide, by = c("hipotesis", "subgrupo", "var"))

# Medias para ideología del endogrupo
medias_ideo_endo <- calc_media_subgrupo(datacp, "ideo_endo", "Ideología endogrupo")
# Medias para ideología del exogrupo
medias_ideo_exo  <- calc_media_subgrupo(datacp, "ideo_exo", "Ideología exogrupo")


tabla_gt <- tabla_resumen %>%
  pivot_wider(
    names_from = label,
    values_from = media
  ) %>%
  rename(
    media_sin = `Sin IA`,
    media_con = `Con IA`
  ) %>%
  mutate(
    d_text = ifelse(!is.na(d), 
                    paste0(round(d, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]"), 
                    "-")
  ) %>%
  select(tema, variable, media_sin, media_con, d_text)

tabla_gt %>%
  gt() %>%
  tab_header(
    title = "Efecto de la etiqueta de IA sobre credibilidad, intención de compartir y polarización afectiva",
    subtitle = "Medias crudas y d de Cohen [IC 95%]"
  ) %>%
  cols_label(
    tema = "Tema",
    variable = "Variable",
    media_sin = "Media sin etiqueta",
    media_con = "Media con etiqueta",
    d_text = "d de Cohen [IC 95%]"
  ) %>%
  fmt_number(columns = where(is.numeric), decimals = 2)


#### TABLA DETALLADA POR SUBGRUPOS (medias y d de Cohen)
# 1. Medias globales (sin distinguir tema) ---------------------------------
# Combinar d y medias para ideología del endogrupo
d_ideo_endo_medias <- d_ideo_endo %>%
  mutate(hipotesis = "Ideología endogrupo") %>%
  left_join(
    medias_ideo_endo %>%
      pivot_wider(names_from = label, values_from = c(media, n)) %>%
      rename(
        media_sin = `media_Sin IA`,
        media_con = `media_Con IA`,
        n_sin     = `n_Sin IA`,
        n_con     = `n_Con IA`
      ) %>%
      mutate(n_total = n_sin + n_con),
    by = c("hipotesis", "subgrupo", "var")
  )

# Combinar d y medias para ideología del exogrupo
d_ideo_exo_medias <- d_ideo_exo %>%
  mutate(hipotesis = "Ideología exogrupo") %>%
  left_join(
    medias_ideo_exo %>%
      pivot_wider(names_from = label, values_from = c(media, n)) %>%
      rename(
        media_sin = `media_Sin IA`,
        media_con = `media_Con IA`,
        n_sin     = `n_Sin IA`,
        n_con     = `n_Con IA`
      ) %>%
      mutate(n_total = n_sin + n_con),
    by = c("hipotesis", "subgrupo", "var")
  )

d_todos_extendido <- bind_rows(
  d_todos_con_medias,
  d_ideo_endo_medias,
  d_ideo_exo_medias
)

datacp <- datacp %>%
  mutate(label_f = factor(label_bin, levels = c(0, 1), labels = c("Sin IA", "Con IA")))

calc_d <- function(data, var_resp) {
  d_res <- cohens_d(as.formula(paste(var_resp, "~ label_f")), data = data, pooled_sd = TRUE)
  data.frame(var = var_resp, d = d_res$Cohens_d, CI_low = d_res$CI_low, CI_high = d_res$CI_high, row.names = NULL)
}




# 2. Unir con las medias por temática (tabla_medias ya existe) ------------
# Nos quedamos solo con las columnas necesarias de tabla_medias
medias_topics <- tabla_medias %>%
  select(tema, label, n, starts_with("media_"))

# Combinamos ambas fuentes
medias_all <- bind_rows(medias_global, medias_topics) %>%
  pivot_longer(cols = starts_with("media_"), names_to = "var", values_to = "media") %>%
  mutate(var = gsub("media_", "", var))

# 3. Formato ancho: medias y n para "Sin IA" y "Con IA" -------------------
medias_wide_principal <- medias_all %>%
  pivot_wider(names_from = label, values_from = c(media, n)) %>%
  rename(
    media_sin = `media_Sin IA`,
    media_con = `media_Con IA`,
    n_sin     = `n_Sin IA`,
    n_con     = `n_Con IA`
  ) %>%
  mutate(n_total = n_sin + n_con)

# 4. Unir con tabla_d (efectos globales y por tema) ------------------------
d_principal <- tabla_d %>%
  left_join(medias_wide_principal, by = c("tema", "var")) %>%
  mutate(
    hipotesis = "Efecto principal",
    subgrupo = tema
  )

# 5. Combinar con los datos de subgrupos (d_todos_con_medias) --------------
d_todos_extendido <- bind_rows(
  d_todos_con_medias,
  d_ideo_endo_medias,
  d_ideo_exo_medias
)

d_todos_completo <- bind_rows(
  d_principal,
  d_todos_extendido
)



# 6. Construir la tabla final ----------------------------------------------
tabla_completa <- d_todos_completo %>%
  mutate(
    d_ic = ifelse(!is.na(d), paste0(round(d, 2), " [", round(CI_low, 2), ", ", round(CI_high, 2), "]"), "-"),
    media_sin = round(media_sin, 2),
    media_con = round(media_con, 2)
  ) %>%
  select(hipotesis, subgrupo, var, n_total, media_sin, media_con, d_ic)

tabla_completa %>%
  gt(groupname_col = "hipotesis") %>%
  tab_header(
    title = "Efecto de la verificación IA: global, por temática y por subgrupos",
    subtitle = "Medias, tamaño muestral y d de Cohen [IC 95%]"
  ) %>%
  cols_label(
    subgrupo  = "Grupo",
    var       = "Variable dependiente",
    n_total   = "N total",
    media_sin = "Media sin IA",
    media_con = "Media con IA",
    d_ic      = "d de Cohen [IC 95%]"
  ) %>%
  fmt_number(columns = c(media_sin, media_con), decimals = 2) %>%
  fmt_missing(columns = everything(), missing_text = "-")

d_todos_completo <- d_todos_completo %>%
  mutate(hipotesis = factor(hipotesis,
                            levels = c("Efecto principal", "Confianza en IA", "Ideología",
                                       "Ideología endogrupo", "Ideología exogrupo",
                                       "Eje territorial", "Género")
  ))

# Construir la tabla final y asignarla a un objeto
tabla_final_gt <- tabla_completa %>%
  gt(groupname_col = "hipotesis") %>%
  tab_header(
    title = "Efecto de la verificación IA: global, por temática y por subgrupos",
    subtitle = "Medias, tamaño muestral y d de Cohen [IC 95%]"
  ) %>%
  cols_label(
    subgrupo  = "Grupo",
    var       = "Variable dependiente",
    n_total   = "N total",
    media_sin = "Media sin IA",
    media_con = "Media con IA",
    d_ic      = "d de Cohen [IC 95%]"
  ) %>%
  fmt_number(columns = c(media_sin, media_con), decimals = 2) %>%
  fmt_missing(columns = everything(), missing_text = "-")

# Exportar a Word
gtsave(tabla_final_gt, "tabla_cohen_subgrupos.docx")
```


### GRÁFICOS 

# GRÁFICOS COHEN
# Forest plot global y por tema
tabla_d %>%
  ggplot(aes(x = d, y = var, color = tema)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2) +
  facet_wrap(~ tema, ncol = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Tamaños de efecto (d de Cohen) de la verificación con IA",
       x = "d de Cohen", y = "Variable dependiente") +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Por subgrupos: confianza en IA
d_todos %>%
  filter(hipotesis == "Confianza en IA") %>%
  ggplot(aes(x = d, y = var, color = subgrupo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Efecto de la verificación IA según confianza en IA",
       x = "d de Cohen", y = "Variable") +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Ideología (4 niveles)
d_todos %>%
  filter(hipotesis == "Ideología") %>%
  ggplot(aes(x = d, y = var, color = subgrupo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Efecto de la verificación IA según ideología personal",
       x = "d de Cohen", y = "Variable dependiente") +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Eje territorial (4 niveles)
d_todos %>%
  filter(hipotesis == "Eje territorial") %>%
  ggplot(aes(x = d, y = var, color = subgrupo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Efecto de la verificación IA según posición territorial",
       x = "d de Cohen", y = "Variable dependiente") +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Género
d_todos %>%
  filter(hipotesis == "Género") %>%
  ggplot(aes(x = d, y = var, color = subgrupo)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Efecto de la verificación IA según género",
       x = "d de Cohen", y = "Variable dependiente") +
  scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  theme_minimal() +
  theme(legend.position = "bottom")

d_global_only <- tabla_d %>% filter(tema == "Global")

ggplot(d_global_only, aes(x = d, y = var)) +
  geom_point(size = 3, color = "steelblue") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.2, color = "steelblue") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Efecto global de la verificación con IA (d de Cohen)",
       x = "d de Cohen", y = "Variable dependiente") +
  scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  theme_minimal()


### __________________ CREDIBILIDAD __________________________

# Función para añadir frecuencias encima de las cajas
add_n <- function(x) {
  return(c(y = max(x, na.rm = TRUE) + 0.5, label = length(x)))
}
ggplot(datacp_re %>% filter(!is.na(simp_red1)),
       aes(x = endo_cond, y = credi)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ simp_red1) +
  labs(title = "Credibilidad por condición endógena según ideología del endogrupo",
       x = "Condición endógena", y = "Credibilidad (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(antisimp_red1)),
       aes(x = endo_cond, y = credi)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ antisimp_red1) +
  labs(title = "Credibilidad por condición endógena según ideología del exogrupo",
       x = "Condición endógena", y = "Credibilidad (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(ideo_red)),
       aes(x = endo_cond, y = credi)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ ideo_red) +
  labs(title = "Credibilidad por condición endógena según ideología personal",
       x = "Condición endógena", y = "Credibilidad (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(gen_red)),
       aes(x = endo_cond, y = credi)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ gen_red) +
  labs(title = "Credibilidad por condición endógena según género",
       x = "Condición endógena", y = "Credibilidad (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(indep_red)),
       aes(x = endo_cond, y = credi)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ indep_red) +
  labs(title = "Credibilidad por condición endógena según eje territorial Cataluña-España",
       x = "Condición endógena", y = "Credibilidad (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### ____________________ SHARE _______________________________

ggplot(datacp_re %>% filter(!is.na(simp_red1)),
       aes(x = endo_cond, y = share)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ simp_red1) +
  labs(title = "Intención de compartir por condición endógena según ideología del endogrupo",
       x = "Condición endógena", y = "Intención de compartir (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(antisimp_red1)),
       aes(x = endo_cond, y = share)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ antisimp_red1) +
  labs(title = "Intención de compartir por condición endógena según ideología del exogrupo",
       x = "Condición endógena", y = "Intención de compartir (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(ideo_red)),
       aes(x = endo_cond, y = share)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ ideo_red) +
  labs(title = "Intención de compartir por condición endógena según ideología personal",
       x = "Condición endógena", y = "Intención de compartir (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp_re %>% filter(!is.na(gen_red)),
       aes(x = endo_cond, y = share)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ gen_red) +
  labs(title = "Intención de compartir por condición endógena según género",
       x = "Condición endógena", y = "Intención de compartir (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(datacp_re %>% filter(!is.na(indep_red)),
       aes(x = endo_cond, y = share)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ indep_red) +
  labs(title = "Intención de compartir por condición endógena según eje territorial Cataluña-España",
       x = "Condición endógena", y = "Intención de compartir (0-10)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### __________________ CONFIANZA __________________________

ggplot(datacp %>% filter(!is.na(simp_red1)),
       aes(x = endo_cond, y = dif_conf)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ simp_red1) +
  labs(title = "Diferencia de confianza exo/endogrupo según ideología del endogrupo",
       x = "Condición endógena", y = "Confianza (0-10)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp %>% filter(!is.na(antisimp_red1)),
       aes(x = endo_cond, y = dif_conf)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ antisimp_red1) +
  labs(title = "Diferencia de confianza exo/endogrupo según ideología del exogrupo",
       x = "Condición endógena", y = "Confianza (0-10)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp %>% filter(!is.na(ideo_red)),
       aes(x = endo_cond, y = dif_conf)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ ideo_red) +
  labs(title = "Diferencia de confianza exo/endogrupo según ideología personal",
       x = "Condición endógena", y = "Confianza (0-10)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp %>% filter(!is.na(gen_red)),
       aes(x = endo_cond, y = dif_conf)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ gen_red) +
  labs(title = "Diferencia de confianza exo/endogrupo según género",
       x = "Condición endógena", y = "Confianza (0-10)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(datacp %>% filter(!is.na(indep_red)),
       aes(x = endo_cond, y = dif_conf)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6) +
  geom_jitter(width = 0.1, alpha = 0.3, size = 1.5) +
  stat_summary(fun.data = function(x) data.frame(y = max(x, na.rm = TRUE) + 0.3, 
                                                 label = paste0("n=", length(x))),
               geom = "text", size = 3) +
  facet_wrap(~ indep_red) +
  labs(title = "Diferencia de confianza exo/endogrupo según eje territorial",
       x = "Condición endógena", y = "Confianza (0-10)") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
```