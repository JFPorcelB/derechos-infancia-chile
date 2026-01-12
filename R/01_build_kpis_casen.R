# R/01_build_kpis_casen.R
library(dplyr)
library(tibble)
library(haven)

# 2) Función promedio ponderado
w_mean <- function(x, w){
  ok <- !is.na(x) & !is.na(w)
  sum(w[ok] * x[ok]) / sum(w[ok])
}

# 3) Calcular 4 KPIs Chile vs Coquimbo (región 4)
df <- casen_2024 %>%
  mutate(
    w = expr,
    poor_inc  = as.integer(pobreza %in% c(1, 2)),
    poor_mdim = as.integer(pobreza_multi == 1),
    attend    = as.integer(asiste == 1),
    no_attend = as.integer(asiste == 2)
  )

region_code <- 4
region_label <- "Coquimbo"

kpis <- tibble::tribble(
  ~indicator_id,     ~indicator_label,                      ~source,     ~universe, ~higher_is_worse, ~chile, ~region,
  "casen_pov_inc",   "Pobreza por ingresos (0–17)",         "CASEN 2024", "0–17",    TRUE,
  100*w_mean(df$poor_inc[df$edad<=17], df$w[df$edad<=17]),
  100*w_mean(df$poor_inc[df$edad<=17 & df$region==region_code],
             df$w[df$edad<=17 & df$region==region_code]),
  
  "casen_pov_mdim",  "Pobreza multidimensional (0–17)",     "CASEN 2024", "0–17",    TRUE,
  100*w_mean(df$poor_mdim[df$edad<=17], df$w[df$edad<=17]),
  100*w_mean(df$poor_mdim[df$edad<=17 & df$region==region_code],
             df$w[df$edad<=17 & df$region==region_code]),
  
  "casen_attend",    "Asistencia a establecimiento (6–17)", "CASEN 2024", "6–17",    FALSE,
  100*w_mean(df$attend[df$edad>=6 & df$edad<=17], df$w[df$edad>=6 & df$edad<=17]),
  100*w_mean(df$attend[df$edad>=6 & df$edad<=17 & df$region==region_code],
             df$w[df$edad>=6 & df$edad<=17 & df$region==region_code]),
  
  "casen_noatt",     "No asiste a establecimiento (6–17)",  "CASEN 2024", "6–17",    TRUE,
  100*w_mean(df$no_attend[df$edad>=6 & df$edad<=17], df$w[df$edad>=6 & df$edad<=17]),
  100*w_mean(df$no_attend[df$edad>=6 & df$edad<=17 & df$region==region_code],
             df$w[df$edad>=6 & df$edad<=17 & df$region==region_code])
) %>%
  mutate(
    region_name = region_label,
    delta_pp = region - chile
  )

# 4) Guardar resultado liviano para el sitio
dir.create("data", showWarnings = FALSE)
saveRDS(kpis, "data/kpis_casen_2024.rds")

message("OK: guardado data/kpis_casen_2024.rds")
print(kpis)
