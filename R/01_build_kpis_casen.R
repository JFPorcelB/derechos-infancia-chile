# R/01_build_kpis_casen.R
library(dplyr)
library(tidyr)
library(tibble)
library(haven)
library(ggplot2)

# -----------------------
# Helpers
# -----------------------
w_mean <- function(x, w){
  ok <- !is.na(x) & !is.na(w)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

# Mapeo regiones Chile (códigos típicos CASEN)
region_names <- c(
  "1"="Tarapacá",
  "2"="Antofagasta",
  "3"="Atacama",
  "4"="Coquimbo",
  "5"="Valparaíso",
  "6"="O'Higgins",
  "7"="Maule",
  "8"="Biobío",
  "9"="La Araucanía",
  "10"="Los Lagos",
  "11"="Aysén",
  "12"="Magallanes",
  "13"="Metropolitana",
  "14"="Los Ríos",
  "15"="Arica y Parinacota",
  "16"="Ñuble"
)

# Colores suaves (ajústalos a tu paleta)
col_chile  <- "#8F8AE6"  # lila suave
col_region <- "#C7C4FF"  # lila más pálido

# -----------------------
# 1) Preparar datos base
# -----------------------
df <- casen_2024 %>%
  mutate(
    w = expr,
    region_code = as.integer(region),
    
    # OJO: pobreza / asiste pueden venir haven_labelled -> pásalos a numeric primero
    pobreza_num = as.numeric(zap_labels(pobreza)),
    pobre_mdim_num = as.numeric(zap_labels(pobreza_multi)),
    asiste_num = as.numeric(zap_labels(asiste)),
    
    poor_inc  = as.integer(pobreza_num %in% c(1, 2)),
    poor_mdim = as.integer(pobre_mdim_num == 1),
    attend    = as.integer(asiste_num == 1),
    no_attend = as.integer(asiste_num == 2),
    
    region_name = recode(as.character(region_code), !!!region_names, .default = as.character(region_code))
  )

# -----------------------
# 2) Definir indicadores (metadata)
# -----------------------
ind_meta <- tribble(
  ~indicator_id,     ~indicator_label,                         ~source,      ~universe, ~higher_is_worse,
  "casen_pov_inc",   "Pobreza por ingresos (0–17)",            "CASEN 2024",  "0–17",    TRUE,
  "casen_pov_mdim",  "Pobreza multidimensional (0–17)",        "CASEN 2024",  "0–17",    TRUE,
  "casen_attend",    "Asistencia a establecimiento (6–17)",    "CASEN 2024",  "6–17",    FALSE,
  "casen_noatt",     "No asiste a establecimiento (6–17)",     "CASEN 2024",  "6–17",    TRUE
)

# -----------------------
# 3) Calcular Chile (nacional)
# -----------------------
nat <- tibble(
  indicator_id = c("casen_pov_inc","casen_pov_mdim","casen_attend","casen_noatt"),
  chile = c(
    100*w_mean(df$poor_inc[df$edad <= 17], df$w[df$edad <= 17]),
    100*w_mean(df$poor_mdim[df$edad <= 17], df$w[df$edad <= 17]),
    100*w_mean(df$attend[df$edad >= 6 & df$edad <= 17], df$w[df$edad >= 6 & df$edad <= 17]),
    100*w_mean(df$no_attend[df$edad >= 6 & df$edad <= 17], df$w[df$edad >= 6 & df$edad <= 17])
  )
)

# -----------------------
# 4) Calcular por región (todas)
# -----------------------
reg <- df %>%
  group_by(region_code, region_name) %>%
  summarise(
    casen_pov_inc  = 100*w_mean(poor_inc[edad <= 17], w[edad <= 17]),
    casen_pov_mdim = 100*w_mean(poor_mdim[edad <= 17], w[edad <= 17]),
    casen_attend   = 100*w_mean(attend[edad >= 6 & edad <= 17], w[edad >= 6 & edad <= 17]),
    casen_noatt    = 100*w_mean(no_attend[edad >= 6 & edad <= 17], w[edad >= 6 & edad <= 17]),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("casen_"),
    names_to = "indicator_id",
    values_to = "region"
  )

# -----------------------
# 5) Armar tabla final kpis (una fila por indicador x región)
# -----------------------
kpis_all <- reg %>%
  left_join(nat, by = "indicator_id") %>%
  left_join(ind_meta, by = "indicator_id") %>%
  mutate(
    delta_pp = region - chile
  ) %>%
  select(
    indicator_id, indicator_label, source, universe, higher_is_worse,
    chile, region, delta_pp,
    region_code, region_name
  )

# Guardar para el sitio
dir.create("data", showWarnings = FALSE)
saveRDS(kpis_all, "data/kpis_casen_2024_all_regions.rds")
message("OK: guardado data/kpis_casen_2024_all_regions.rds")

# -----------------------
# 6) (Opcional recomendado) Mini-gráficos por indicador x región
#     -> quedan listos para usarlos en cards con <img src="figures/...">
# -----------------------
dir.create("docs/figures", recursive = TRUE, showWarnings = FALSE)

make_mini_plot <- function(ind_id, ind_lbl, chile, region_val, region_name, region_code){
  dfp <- tibble(
    zona = c("Chile", region_name),
    valor = c(chile, region_val)
  ) %>%
    mutate(zona = factor(zona, levels = unique(zona)))  # <- evita duplicados
  
  vals <- c("Chile" = col_chile, region_name = col_region)
  
  p <- ggplot(dfp, aes(x = zona, y = valor, fill = zona)) +
    geom_col(width = 0.62) +
    geom_text(aes(label = sprintf("%.1f%%", valor)), vjust = -0.35, size = 3.4) +
    scale_fill_manual(values = vals) +
    scale_y_continuous(limits = c(0, max(dfp$valor)*1.2), expand = expansion(mult = c(0, 0.05))) +
    labs(x = NULL, y = NULL, title = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "grey88"),
      axis.text.x = element_text(color = "grey45", size = 9),
      axis.text.y = element_blank(),
      plot.margin = margin(6, 6, 2, 6)
    )
  
  out <- file.path("docs/figures", sprintf("kpi_%s_r%02d.png", ind_id, region_code))
  ggsave(out, p, width = 3.0, height = 2.2, dpi = 200, bg = "transparent")
  out
}

kpis_all %>%
  group_by(region_code, region_name, indicator_id, indicator_label) %>%
  summarise(
    chile = first(chile),
    region = first(region),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    file = make_mini_plot(indicator_id, indicator_label, chile, region, region_name, region_code)
  ) %>%
  ungroup()

message("OK: mini-gráficos en docs/figures/kpi_<id>_rXX.png")

# -----------------------
# 7) Exportar JSON para el mockup web (docs/)
# -----------------------
library(jsonlite)

dir.create("docs/data", recursive = TRUE, showWarnings = FALSE)

# a) KPIs por región (estructura cómoda para JS)
kpis_json <- kpis_all %>%
  arrange(region_code, indicator_id) %>%
  group_by(region_code, region_name) %>%
  summarise(
    kpis = list(
      tibble(
        indicator_id = indicator_id,
        indicator_label = indicator_label,
        source = source,
        universe = universe,
        higher_is_worse = higher_is_worse,
        chile = round(chile, 1),
        region = round(region, 1),
        delta_pp = round(delta_pp, 1),
        # ruta del mini-gráfico ya creado (coincide con tu naming)
        fig = sprintf("figures/kpi_%s_r%02d.png", indicator_id, region_code)
      )
    ),
    .groups = "drop"
  )

payload <- list(
  generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  regions = kpis_json
)

write_json(payload, "docs/data/kpis.json", auto_unbox = TRUE, pretty = TRUE)
message("OK: exportado docs/data/kpis.json")
