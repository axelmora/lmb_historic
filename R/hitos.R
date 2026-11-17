# ---------------------------------------------------------------------------
# hitos.R : carga y validación de los resultados de etl_milestones
#   hitos_bateo/pitcheo.csv     -> hitos cruzados en la temporada y aspirantes a un hito redondo
#   cercanos_bateo/pitcheo.csv  -> cercanía a los líderes históricos (QESQ)
# Las columnas carrera_AAAA y temporada_AAAA se detectan solas (año de corte).
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({ library(dplyr); library(stringr); library(tibble); library(purrr) })

HITOS_STAT <- tribble(
  ~stat, ~rol,      ~etiqueta,                  ~corto,
  "H",   "Bateo",   "Hits",                     "Hits",
  "HR",  "Bateo",   "Cuadrangulares",           "HR",
  "CP",  "Bateo",   "Carreras producidas",      "CP",
  "CA",  "Bateo",   "Carreras anotadas",        "CA",
  "BR",  "Bateo",   "Bases robadas",            "BR",
  "G",   "Pitcheo", "Juegos ganados",           "Ganados",
  "JS",  "Pitcheo", "Juegos salvados",          "Salvados",
  "SO",  "Pitcheo", "Ponches",                  "Ponches",
  "CJ",  "Pitcheo", "Cierres de juego",         "CJ")

# "DE LA CRUZ, JOSÉ" -> "José de la Cruz"
fmt_jugador <- function(x) {
  part <- c("de", "del", "la", "las", "los", "y", "da", "di", "van", "von", "san")
  one <- function(s) {
    p <- str_split(s, ",\\s*", n = 2)[[1]]
    nm <- if (length(p) == 2) paste(p[2], p[1]) else s
    w <- str_split(str_to_lower(str_squish(nm)), " ")[[1]]
    w <- ifelse(w %in% part & seq_along(w) > 1, w, str_to_title(w))
    paste(w, collapse = " ")
  }
  vapply(x, one, "", USE.NAMES = FALSE)
}

load_hitos <- function(dir = "data/hitos") {
  f <- file.path(dir, c("hitos_bateo.csv", "hitos_pitcheo.csv", "cercanos_bateo.csv", "cercanos_pitcheo.csv"))
  if (!all(file.exists(f))) return(NULL)
  rd <- function(p) utils::read.csv(p, fileEncoding = "UTF-8", stringsAsFactors = FALSE, check.names = FALSE) |> as_tibble()
  hb <- rd(f[1]) |> mutate(rol = "Bateo"); hp <- rd(f[2]) |> mutate(rol = "Pitcheo")
  cb <- rd(f[3]) |> mutate(rol = "Bateo"); cp <- rd(f[4]) |> mutate(rol = "Pitcheo")

  cols <- names(hb)
  yr_s <- as.integer(str_match(cols[str_detect(cols, "^temporada_\\d{4}$")], "(\\d{4})")[, 2])
  yr_c <- as.integer(str_match(cols[str_detect(cols, "^carrera_\\d{4}$")],  "(\\d{4})")[, 2])
  stopifnot(length(yr_s) == 1, length(yr_c) == 1)
  norm <- \(d) d |> rename(carrera_prev = all_of(paste0("carrera_", yr_c)), temporada = all_of(paste0("temporada_", yr_s)))
  ascii <- \(x) str_to_lower(stringi::stri_trans_general(x, "Latin-ASCII"))

  hitos <- bind_rows(norm(hb), norm(hp)) |>
    left_join(HITOS_STAT |> select(stat, etiqueta, corto), by = "stat") |>
    mutate(nombre = fmt_jugador(jugador), llave = ascii(nombre),
           hito = paste(format(umbral, big.mark = ","), corto), avance = if_else(tipo == "cruzado", 1, floor(100 * carrera_actual / umbral) / 100))
  lideres <- bind_rows(cb, cp) |>
    left_join(HITOS_STAT |> select(stat, etiqueta, corto), by = "stat") |>
    mutate(nombre = fmt_jugador(jugador), llave = ascii(nombre), llave_lider = ascii(lider_objetivo)) |>
    # si el líder también juega hoy, su marca del libro puede estar desactualizada: se compara con su carrera actual
    left_join(hitos |> transmute(llave_lider = llave, stat, lider_actual = carrera_actual), by = c("llave_lider", "stat")) |>
    mutate(lider_activo = !is.na(lider_actual), lider_desactualizado = lider_activo & lider_actual > lider_valor,
           nota = case_when(lider_desactualizado ~ paste0("El líder sigue activo y ya suma ", lider_actual, "; la marca está desactualizada"),
                            lider_activo ~ "El líder sigue activo", TRUE ~ ""))

  qa <- list()
  qa$lider_desactualizado <- lideres |> filter(lider_desactualizado) |> select(nombre, stat, carrera_actual, lider_objetivo, lider_valor, lider_actual)
  qa$suma <- hitos |> filter(carrera_actual != carrera_prev + temporada) |> select(nombre, stat, carrera_prev, temporada, carrera_actual)
  qa$cruzados <- hitos |> filter(tipo == "cruzado", !(carrera_prev < umbral & carrera_actual >= umbral)) |> select(nombre, stat, umbral, carrera_prev, carrera_actual)
  qa$aspirantes <- hitos |> filter(tipo == "aspirante", !(carrera_actual < umbral & umbral - carrera_actual == distancia)) |> select(nombre, stat, umbral, carrera_actual, distancia)
  qa$duplicados <- hitos |> count(jugador, stat) |> filter(n > 1)
  qa$lider_distancia <- lideres |> filter(lider_valor - carrera_actual != distancia) |> select(nombre, stat, carrera_actual, lider_valor, distancia)
  qa$sin_hitos <- lideres |> filter(!jugador %in% hitos$jugador) |> distinct(nombre, stat)
  qa$lideres_por_stat <- lideres |> group_by(stat, etiqueta) |>
    summarise(jugadores = n_distinct(jugador), lideres_distintos = n_distinct(lider_objetivo), lider_mas_frecuente = names(sort(table(lider_objetivo), decreasing = TRUE))[1],
              jugadores_a_ese_lider = as.integer(max(table(lider_objetivo))), mediana_distancia = median(distancia), .groups = "drop") |>
    mutate(revisar = if_else(lideres_distintos == 1 & jugadores > 50, "Un solo líder para casi todos los jugadores", ""))
  list(hitos = hitos, lideres = lideres, anio_corte = yr_s, anio_base = yr_c, qa = qa)
}
