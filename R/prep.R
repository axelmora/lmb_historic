# ---------------------------------------------------------------------------
# prep.R : lectura, limpieza y reglas de contabilidad de la LMB (1925-2026)
# Identidad de equipo = team_id del catálogo. El nombre que se muestra en cada
# temporada sale de la hoja equipos_long_format (nombre_temporada).
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(readxl); library(purrr); library(tibble)
})

RONDA_LV    <- c("WildCard / Primer Play Off", "Segundo Play Off / Series de Zona",
                 "Campeonato de Zona", "Serie del Rey / Serie Final")
RONDA_CORTA <- c("Comodín / 1er PO", "2do PO / Serie de Zona", "Campeonato de Zona", "Serie del Rey")

key_of       <- function(x) tolower(stringi::stri_trans_general(str_squish(x), "Latin-ASCII"))
season_year  <- function(s) floor(s + 1e-6)
season_label <- function(s) {
  y <- season_year(s); f <- round((s - y) * 10)
  ifelse(f > 0, paste0(y, "-", f), as.character(y))
}
fmt_pct <- function(p) ifelse(is.na(p), "", sub("^0", "", sprintf("%.3f", p)))
fix_name <- function(x) str_replace_all(str_squish(x), "\\b(de|del)(?=[A-ZÁÉÍÓÚÑ])", "\\1 ")

# Excel guardó marcadores tipo "2-1" como fecha (1 de febrero). Se revierte a "mes-día".
parse_score <- function(x) {
  num <- suppressWarnings(as.numeric(x)); ser <- !is.na(num) & num > 1000
  d <- as.Date(num[ser], origin = "1899-12-30")
  x[ser] <- paste0(as.integer(format(d, "%m")), "-", as.integer(format(d, "%d"))); x
}
parse_jv <- function(x) {
  x <- str_replace_all(x, "½", ".5"); x[str_detect(x, "^[—–-]+$")] <- "0"
  suppressWarnings(as.numeric(str_replace_all(x, "[^0-9.]", "")))
}
# "1925-1926; 1928" -> c(1925, 1926, 1928)
parse_years <- function(t) {
  if (is.na(t)) return(integer())
  t <- str_replace_all(t, "(\\d{4})\\.0\\b", "\\1")
  unlist(lapply(str_split(t, "[;,]")[[1]], function(p) {
    p <- str_squish(p); m <- str_match(p, "^(\\d{4})\\s*[–-]\\s*(\\d{4})$")
    if (!is.na(m[1])) return(seq(as.integer(m[2]), as.integer(m[3])))
    if (str_detect(p, "^\\d{4}$")) as.integer(p) else integer() }))
}
compact_years <- function(y) {
  y <- sort(unique(y)); if (!length(y)) return("")
  g <- cumsum(c(1, diff(y) != 1))
  paste(tapply(y, g, \(v) if (length(v) == 1) as.character(v) else paste0(min(v), "–", max(v))), collapse = "; ")
}

# Asigna team_id a pares (año, nombre escrito). Pasos: 1) nombre exacto de esa temporada en la hoja
# long, 2) nombre del catálogo activo ese año, 3) por descarte (1 sin asignar vs 1 equipo de la hoja
# long sin fila), 4) equipo provisional.
resolve_ids <- function(pairs, lf_keys, cat_keys, active, lf, descarte = FALSE) {
  pairs <- distinct(pairs, yr, nm) |> mutate(key = key_of(nm))
  pick <- function(x) x |> group_by(yr, nm) |> summarise(n = n_distinct(team_id), team_id = first(team_id), .groups = "drop")
  r1 <- pairs |> inner_join(lf_keys, by = c("yr", "key")) |> pick() |> mutate(metodo = "Nombre de la temporada (hoja long)")
  rest <- anti_join(pairs, r1, by = c("yr", "nm"))
  r2 <- rest |> inner_join(cat_keys, by = "key", relationship = "many-to-many") |> inner_join(active, by = c("team_id", "yr")) |>
    pick() |> mutate(metodo = "Nombre del catálogo")
  rest <- anti_join(rest, r2, by = c("yr", "nm"))
  r3 <- tibble(yr = integer(), nm = character(), n = integer(), team_id = integer(), metodo = character())
  if (descarte && nrow(rest)) {
    got <- bind_rows(r1, r2) |> distinct(yr, team_id)
    libres <- lf |> distinct(yr, team_id) |> anti_join(got, by = c("yr", "team_id"))
    r3 <- rest |> group_by(yr) |> filter(n() == 1) |> ungroup() |> inner_join(libres |> group_by(yr) |> filter(n() == 1) |> ungroup(), by = "yr") |>
      transmute(yr, nm, n = 1L, team_id, metodo = "Por descarte (único sin asignar ese año)")
    rest <- anti_join(rest, r3, by = c("yr", "nm"))
    # mismo nombre escrito ya asignado sin ambigüedad en otras temporadas, y el equipo está libre ese año
    mapa <- bind_rows(r1, r2, r3) |> distinct(nm, team_id)
    r3b <- rest |> inner_join(mapa, by = "nm") |> inner_join(libres, by = c("yr", "team_id")) |>
      group_by(yr, nm) |> filter(n() == 1) |> ungroup() |>
      transmute(yr, nm, n = 1L, team_id, metodo = "Mismo nombre asignado en otras temporadas")
    r3 <- bind_rows(r3, r3b); rest <- anti_join(rest, r3b, by = c("yr", "nm"))
  }
  r4 <- rest |> distinct(nm) |> mutate(team_id = 9000L + row_number(), n = 1L, metodo = "SIN CATÁLOGO") |> right_join(rest |> select(yr, nm), by = "nm")
  bind_rows(r1, r2, r3, r4) |> select(yr, nm, team_id, n, metodo)
}

load_lmb <- function(path) {
  rd <- function(sh) read_excel(path, sheet = sh, col_types = "text", na = c("", "NA", "NaN"))
  qa <- list(); acciones <- tibble(donde = character(), detalle = character())
  log_a <- function(donde, detalle) acciones <<- add_row(acciones, donde = donde, detalle = detalle)

  # ---------------- Catálogo y hoja long ----------------
  cat <- rd("catalogo_equipos") |> transmute(team_id = as.integer(team_id), equipo = str_squish(equipo),
            temporadas_txt = temporadas, sede = sede_principal, nombres_usados)
  lf0 <- rd("equipos_long_format") |> transmute(team_id = as.integer(team_id), canon_long = str_squish(equipo_canonico),
            yr = as.integer(season), nombre_raw = str_squish(nombre_temporada), sede = sede_principal)
  lf <- lf0 |> mutate(nombre = fix_name(nombre_raw))
  ch <- lf |> filter(nombre != nombre_raw) |> distinct(nombre_raw, nombre)
  if (nrow(ch)) log_a("equipos_long_format", paste0("Se corrigió el espacio faltante en: ", paste0("'", ch$nombre_raw, "' → '", ch$nombre, "'", collapse = "; "), "."))

  lf_keys <- bind_rows(lf |> transmute(yr, key = key_of(canon_long), team_id), lf |> transmute(yr, key = key_of(nombre), team_id)) |> distinct()
  cat_keys <- bind_rows(cat |> transmute(key = key_of(equipo), team_id),
                        cat |> filter(!is.na(nombres_usados)) |> transmute(team_id, nm = str_split(nombres_usados, ";")) |> unnest(nm) |> transmute(key = key_of(nm), team_id),
                        lf |> distinct(team_id, canon_long) |> transmute(key = key_of(canon_long), team_id)) |> distinct()
  active <- bind_rows(lf |> distinct(team_id, yr), cat |> transmute(team_id, yr = map(temporadas_txt, parse_years)) |> unnest(yr)) |> distinct()
  nombre_de <- function(id, yr) {
    m <- lf$nombre[match(paste(id, yr), paste(lf$team_id, lf$yr))]
    coalesce(m, cat$equipo[match(id, cat$team_id)])
  }
  sede_de <- function(id, yr) lf$sede[match(paste(id, yr), paste(lf$team_id, lf$yr))]

  # ---------------- Standings ----------------
  st <- rd("standings_1925_2026") |>
    transmute(season = as.numeric(season), zona = coalesce(zona, ""), division = coalesce(division, ""),
              split = coalesce(split, ""), nm_src = str_squish(equipo),
              G = as.integer(G), P = as.integer(P), pct_src = as.numeric(PCT),
              jv_src = parse_jv(JV), puntos = as.numeric(puntos)) |>
    mutate(yr = as.integer(season_year(season)))
  fp <- !is.na(st$puntos) & !between(st$season, 1993, 2010)
  if (any(fp)) log_a("standings", sprintf("Se ignoró %d valor(es) de 'puntos' fuera de 1993-2010 (%s).", sum(fp),
                                          paste(unique(paste(season_label(st$season[fp]), st$nm_src[fp])), collapse = ", ")))
  st$puntos[fp] <- NA

  rs <- resolve_ids(st |> transmute(yr, nm = nm_src), lf_keys, cat_keys, active, lf, descarte = TRUE)
  st <- st |> left_join(rs |> select(yr, nm_src = nm, team_id, metodo), by = c("yr", "nm_src"))
  st$team  <- nombre_de(st$team_id, st$yr); st$team[is.na(st$team)] <- st$nm_src[is.na(st$team)]
  st$canon <- cat$equipo[match(st$team_id, cat$team_id)]; st$canon[is.na(st$canon)] <- st$nm_src[is.na(st$canon)]
  cat <- bind_rows(cat, tibble(team_id = unique(st$team_id[st$team_id >= 9000]), equipo = st$nm_src[match(unique(st$team_id[st$team_id >= 9000]), st$team_id)],
                               temporadas_txt = NA_character_, sede = NA_character_, nombres_usados = NA_character_))
  qa$resolucion <- st |> filter(metodo != "Nombre de la temporada (hoja long)") |>
    group_by(temporada = yr, `nombre en standings` = nm_src, `asignado a` = paste0(canon, " (id ", team_id, ")"), metodo) |>
    summarise(filas = n(), .groups = "drop") |> arrange(metodo, temporada) |> mutate(temporada = as.character(temporada))
  qa$duplicados <- st |> filter(duplicated(cbind(season, zona, division, split, team_id)) | duplicated(cbind(season, zona, division, split, team_id), fromLast = TRUE)) |>
    transmute(temporada = season_label(season), zona, split, equipo = team, G, P)

  st <- st |>
    mutate(cancelada = ave(G + P, season, FUN = sum) == 0,
           pct = G / (G + P),
           bloque = case_when(zona == "GLOBAL" ~ "Global", zona == "EXTRAORDINARIA" ~ "Temporada extraordinaria",
                              zona == "" & division == "" ~ "Tabla general",
                              TRUE ~ str_squish(paste(str_to_title(str_remove(zona, "^ZONA ")), str_to_title(division)))),
           vuelta = case_when(split == "PRIMERA MITAD" ~ "Primera vuelta", split == "SEGUNDA MITAD" ~ "Segunda vuelta",
                              split == "FINAL" ~ "Final (suma de vueltas)", split == "ROUND ROBIN" ~ "Round robin", TRUE ~ ""),
           bloque_id = paste(zona, division, split, sep = "|")) |>
    group_by(season, zona) |> mutate(zona_tiene_final = any(split == "FINAL")) |> ungroup() |>
    mutate(rol = case_when(
      cancelada ~ "cancelada",
      zona == "GLOBAL" ~ "info_global", split == "ROUND ROBIN" ~ "postemporada_rr",
      split %in% c("PRIMERA MITAD", "SEGUNDA MITAD") & zona_tiene_final ~ "info_vuelta",
      split %in% c("PRIMERA MITAD", "SEGUNDA MITAD") ~ "regular_vuelta", TRUE ~ "regular"),
      cuenta = rol %in% c("regular", "regular_vuelta"),
      rol_txt = recode(rol, regular = "Cuenta en temporada regular",
        regular_vuelta = "Cuenta: las dos vueltas se suman (no hay standing final)",
        info_vuelta = "Informativo: ya está incluida en el standing Final",
        info_global = "Informativo: no se suma (ya está en el standing por zona)",
        postemporada_rr = "Cuenta como postemporada (round robin)",
        cancelada = "Temporada programada que no se disputó")) |>
    group_by(season, bloque_id) |> mutate(pos = row_number(), jv = ((G[1] - G) + (P - P[1])) / 2) |> ungroup()
  st$bloque_txt <- str_squish(paste(st$bloque, if_else(st$vuelta == "", "", paste0("- ", st$vuelta))))

  # ---------------- Series ----------------
  raw_sr <- rd("series_result")
  n_dates <- sum(!is.na(suppressWarnings(as.numeric(raw_sr$marcador))))
  log_a("series_result", sprintf("%d marcadores estaban guardados como fecha en Excel (p. ej. 2-1 = 1 de febrero); se restauraron a 'ganados-perdidos'.", n_dates))
  sr <- raw_sr |> transmute(season = as.numeric(season), yr = as.integer(season_year(season)), ronda,
              nw = str_squish(ganador_equipo_catalogo), nl = str_squish(perdedor_equipo_catalogo), score = parse_score(marcador)) |>
    separate(score, c("w", "l"), sep = "-", convert = TRUE, remove = FALSE) |>
    mutate(ronda_id = match(ronda, RONDA_LV), ronda_corta = RONDA_CORTA[ronda_id])
  rs2 <- resolve_ids(bind_rows(sr |> transmute(yr, nm = nw), sr |> transmute(yr, nm = nl)), lf_keys, cat_keys, active, lf, descarte = FALSE)
  sr <- sr |> left_join(rs2 |> select(yr, nw = nm, id_w = team_id), by = c("yr", "nw")) |>
    left_join(rs2 |> select(yr, nl = nm, id_l = team_id), by = c("yr", "nl")) |>
    mutate(ganador = coalesce(nombre_de(id_w, yr), nw), perdedor = coalesce(nombre_de(id_l, yr), nl))
  qa$series_resolucion <- rs2 |> filter(metodo != "Nombre de la temporada (hoja long)") |>
    transmute(temporada = as.character(yr), `nombre en series` = nm, asignado = coalesce(cat$equipo[match(team_id, cat$team_id)], "—"), metodo)
  qa$marcador_invalido <- filter(sr, is.na(w) | is.na(l) | w <= l)

  # ---------------- Notas ----------------
  nt_raw <- rd("lmb_seasons_notes")
  zflag  <- if ("zone_champions" %in% names(nt_raw)) toupper(str_squish(nt_raw$zone_champions)) %in% c("TRUE", "1", "VERDADERO", "SI", "SÍ") else rep(FALSE, nrow(nt_raw))
  nt <- nt_raw |> transmute(season = as.numeric(season), manager = str_squish(manager_champion),
                            notas = notes, formato_campeonato = championship_format) |> mutate(zone_champions = zflag)

  # ---------------- Regular por equipo-temporada ----------------
  ts <- st |> filter(cuenta) |> mutate(extra = zona == "EXTRAORDINARIA") |>
    group_by(season, team_id, extra) |>
    summarise(team = first(team), canon = first(canon), G = sum(G), P = sum(P),
              regla = case_when(any(rol == "regular_vuelta") ~ "Dos vueltas sumadas",
                                any(split == "FINAL") ~ "Standing Final (vueltas no se suman)",
                                any(zona == "EXTRAORDINARIA") ~ "Temporada extraordinaria", TRUE ~ "Standing único"),
              zona = first(bloque), pos_bloque = if_else(n() == 1 | any(split == "FINAL"), pos[split == "FINAL" | n() == 1][1], NA_integer_),
              .groups = "drop") |> mutate(pct = G / (G + P))
  ts <- left_join(ts, st |> filter(rol == "info_global") |> select(season, team_id, pos_global = pos), by = c("season", "team_id"))
  ts$sede <- sede_de(ts$team_id, as.integer(season_year(ts$season)))

  # ---------------- Postemporada ----------------
  tp <- bind_rows(
    sr |> transmute(season, ronda_id, ronda_corta, team_id = id_w, team = ganador, opp = perdedor, gw = w, gl = l, gano = 1L),
    sr |> transmute(season, ronda_id, ronda_corta, team_id = id_l, team = perdedor, opp = ganador, gw = l, gl = w, gano = 0L))
  rr <- st |> filter(rol == "postemporada_rr") |> select(season, team_id, team, G, P)

  # ---------------- Campeones ----------------
  fin <- sr |> filter(ronda_id == 4) |> group_by(season) |> slice_tail(n = 1) |> ungroup()
  qa$finales_multiples <- sr |> filter(ronda_id == 4) |> count(season) |> filter(n > 1)
  best <- function(s, extra_only) {
    d <- ts |> filter(season == s, if (extra_only) extra else TRUE) |> group_by(team_id) |> summarise(team = first(team), G = sum(G), P = sum(P), .groups = "drop") |> mutate(pct = G / (G + P))
    if (!nrow(d)) return(list(NA_integer_, NA_character_))
    top <- d[abs(d$pct - max(d$pct)) < 1e-9, ]
    list(if (nrow(top) == 1) top$team_id else NA_integer_, paste(top$team, collapse = " / "))
  }
  seasons <- nt |> mutate(anio = season_year(season), etiqueta = season_label(season)) |>
    left_join(fin |> select(season, id_f = id_w, campeon_f = ganador, sub_f = perdedor, marcador_final = score), by = "season") |>
    mutate(hay_datos = season %in% st$season, cancelada = season %in% unique(st$season[st$cancelada]), extra_only = str_detect(coalesce(notas, ""), "EXTRAORDINARIA"))
  mr <- map2(seasons$season, seasons$extra_only, \(s, e) if (s %in% ts$season) best(s, e) else list(NA_integer_, NA_character_))
  seasons <- seasons |> mutate(id_mr = map_int(mr, 1), campeon_mr = map_chr(mr, 2),
      campeon = coalesce(campeon_f, campeon_mr), campeon_id = coalesce(id_f, id_mr),
      campeon_fuente = case_when(cancelada ~ "Temporada cancelada", !hay_datos ~ "Sin temporada", !is.na(campeon_f) ~ "Serie del Rey / Final",
                                 str_detect(campeon, " / ") ~ "Empate en récord (definido en serie extra, no incluida)", TRUE ~ "Mejor récord de la temporada"),
      campeon_unico = !is.na(campeon_id)) |> select(-campeon_f, -campeon_mr, -id_f, -id_mr)
  est <- st |> group_by(season) |> summarise(estructura = {
    z <- setdiff(unique(zona), c("", "GLOBAL", "EXTRAORDINARIA"))
    p <- c(if (any(split == "FINAL")) "Dos vueltas por puntos + Final" else if (any(split %in% c("PRIMERA MITAD", "SEGUNDA MITAD"))) "Dos vueltas sumadas",
           if (any(division != "")) "Zonas y divisiones" else if (length(z)) "Zonas" else if (!any(split != "")) "Tabla única",
           if (any(zona == "EXTRAORDINARIA")) "Zona extraordinaria", if (any(zona == "GLOBAL")) "Global informativo", if (any(split == "ROUND ROBIN")) "Round robin")
    paste(p, collapse = " + ") }, .groups = "drop")
  seasons <- left_join(seasons, est, by = "season") |>
    mutate(estructura = if_else(cancelada, "Temporada cancelada", coalesce(estructura, "Sin temporada")))

  # ---------------- Campeones de zona (finalistas, cuando zone_champions = TRUE) ----------------
  zmap <- st |> filter(cuenta, zona != "EXTRAORDINARIA", zona != "") |>
    mutate(zn = str_to_title(str_remove(zona, "^ZONA "))) |> distinct(season, team_id, zn) |> group_by(season, team_id) |> slice(1) |> ungroup()
  zsrc <- seasons |> filter(zone_champions) |> select(season) |>
    inner_join(fin |> transmute(season, w = id_w, l = id_l, nw = ganador, nl = perdedor), by = "season")
  zc <- bind_rows(zsrc |> transmute(season, team_id = w, team = nw, papel = "Campeón"),
                  zsrc |> transmute(season, team_id = l, team = nl, papel = "Subcampeón")) |>
    left_join(zmap, by = c("season", "team_id")) |> arrange(season, zn)
  zt <- zc |> group_by(season) |> summarise(zc_txt = paste0(team, if_else(is.na(zn), "", paste0(" (", zn, ")")), collapse = " · "), .groups = "drop")
  seasons <- left_join(seasons, zt, by = "season")
  qa$zonas <- bind_rows(
    seasons |> filter(zone_champions, !season %in% fin$season) |> transmute(temporada = etiqueta, problema = "Marcada con campeones de zona, pero no hay serie final: no se identifican los campeones de zona"),
    zc |> filter(is.na(zn)) |> transmute(temporada = as.character(season_label(season)), problema = paste0(team, ": finalista sin zona en los standings")),
    zc |> group_by(season) |> filter(n_distinct(zn, na.rm = TRUE) == 1, n() == 2) |> slice(1) |> ungroup() |> transmute(temporada = as.character(season_label(season)), problema = "Los dos finalistas pertenecen a la misma zona"))

  ts <- left_join(ts, zc |> select(season, team_id, zona_camp = zn, papel_final = papel) |> mutate(es_zc = TRUE), by = c("season", "team_id")) |>
    mutate(es_zc = coalesce(es_zc, FALSE))

  # ---------------- Nombres por equipo (para histórico) ----------------
  yrs_con_temp <- unique(st$yr)
  yrs_jugados  <- unique(as.integer(season_year(ts$season)))
  nombres <- lf |> filter(yr %in% yrs_jugados) |> arrange(team_id, yr) |> group_by(team_id) |>
    mutate(run = cumsum(nombre != lag(nombre, default = first(nombre)))) |> group_by(team_id, run, nombre) |>
    summarise(desde = min(yr), hasta = max(yr), anios = compact_years(yr), n = n(), .groups = "drop") |> arrange(team_id, desde)

  runs <- lf |> arrange(team_id, yr) |> group_by(team_id) |>
    mutate(brk = nombre != lag(nombre, default = first(nombre)) | yr - lag(yr, default = first(yr)) > 1, run = cumsum(brk),
           idx = match(nombre, unique(nombre))) |>
    group_by(team_id, run, idx, nombre) |> summarise(desde = min(yr), hasta = max(yr), sede = first(sede), .groups = "drop") |>
    left_join(cat |> select(team_id, canon = equipo), by = "team_id") |> arrange(team_id, desde)

  # ---------------- Control de calidad ----------------
  qa$acciones <- acciones
  qa$pct <- st |> filter(abs(round(G / (G + P), 3) - pct_src) > 0.0015) |>
    transmute(temporada = season_label(season), bloque = bloque_txt, equipo = team, G, P, pct_fuente = pct_src, pct_recalculado = round(pct, 3))
  h <- st |> filter(split %in% c("PRIMERA MITAD", "SEGUNDA MITAD")) |> group_by(season, zona, team_id) |> summarise(Gv = sum(G), Pv = sum(P), .groups = "drop")
  qa$final_vs_vueltas <- st |> filter(split == "FINAL") |> select(season, zona, team_id, team, Gf = G, Pf = P) |>
    inner_join(h, by = c("season", "zona", "team_id")) |> filter(Gf != Gv | Pf != Pv) |>
    transmute(temporada = season_label(season), zona, equipo = team, final = paste0(Gf, "-", Pf), suma_vueltas = paste0(Gv, "-", Pv))
  qa$balance <- ts |> group_by(season, extra) |> summarise(G = sum(G), P = sum(P), .groups = "drop") |> filter(G != P) |>
    transmute(temporada = season_label(season), extraordinaria = extra, G, P, diferencia = G - P)
  qa$series_sin_standing <- tp |> distinct(season, team_id, team) |> anti_join(st |> distinct(season, team_id), by = c("season", "team_id")) |>
    filter(season %in% st$season) |> transmute(temporada = season_label(season), equipo = team) |> arrange(temporada)
  stid <- st |> distinct(yr, team_id)
  qa$long_sin_standing <- lf |> filter(yr %in% yrs_con_temp) |> anti_join(stid, by = c("yr", "team_id")) |>
    transmute(temporada = as.character(yr), team_id, equipo = canon_long, nombre_temporada = nombre) |> arrange(temporada)
  qa$long_sin_temporada <- lf |> filter(!yr %in% yrs_con_temp) |> group_by(temporada = as.character(yr)) |>
    summarise(equipos = n(), nota = paste0("Sin standings ni series", if (isTRUE(any(nt$season == first(yr) & str_detect(coalesce(nt$notas, ""), "NO HUBO"), na.rm = TRUE))) " (las notas dicen: no hubo temporada)" else ""), .groups = "drop")
  qa$catalogo_vs_long <- cat |> filter(team_id < 9000) |> left_join(lf |> distinct(team_id, canon_long), by = "team_id") |> filter(key_of(equipo) != key_of(canon_long)) |>
    transmute(team_id, `catálogo` = equipo, `hoja long` = canon_long)
  g <- lf |> group_by(team_id) |> summarise(ys = list(sort(unique(yr))))
  qa$catalogo_temporadas <- cat |> filter(team_id < 9000) |> left_join(g, by = "team_id") |>
    mutate(cat_y = map(temporadas_txt, parse_years), solo_catalogo = map2_chr(cat_y, ys, \(a, b) compact_years(setdiff(a, b))),
           solo_long = map2_chr(cat_y, ys, \(a, b) compact_years(setdiff(b, a)))) |>
    filter(solo_catalogo != "" | solo_long != "") |> transmute(team_id, equipo, `solo en catálogo` = solo_catalogo, `solo en hoja long` = solo_long)
  qa$temporadas_sin_serie <- seasons |> filter(hay_datos, !cancelada, !season %in% sr$season) |> transmute(temporada = etiqueta, campeon, fuente = campeon_fuente)

  qa$acciones <- acciones
  list(st = st, sr = sr, tp = tp, rr = rr, nt = nt, ts = ts, seasons = seasons, qa = qa, cat = cat, lf = lf, nombres = nombres, runs = runs, zc = zc, nombre_de = nombre_de)
}

# ---------------- Historial por equipo (team_id) ----------------
build_history <- function(D) {
  ts <- D$ts; tp <- D$tp; rr <- D$rr; champs <- D$seasons |> filter(campeon_unico)
  resumen <- ts |> group_by(team_id) |>
    summarise(temporadas = n_distinct(season_year(season)), primera = min(season_year(season)), ultima = max(season_year(season)), G = sum(G), P = sum(P), .groups = "drop") |>
    mutate(pct = G / (G + P)) |>
    left_join(bind_rows(tp |> select(team_id, gw, gl), rr |> transmute(team_id, gw = G, gl = P)) |> group_by(team_id) |> summarise(pG = sum(gw), pP = sum(gl), .groups = "drop"), by = "team_id") |>
    left_join(tp |> group_by(team_id) |> summarise(sG = sum(gano), sP = sum(1 - gano), finales = sum(ronda_id == 4), .groups = "drop"), by = "team_id") |>
    left_join(count(champs, team_id = campeon_id, name = "titulos"), by = "team_id") |>
    left_join(count(D$zc, team_id, name = "zonas"), by = "team_id") |>
    mutate(across(c(pG, pP, sG, sP, finales, titulos, zonas), \(x) coalesce(as.numeric(x), 0)), ppct = if_else(pG + pP > 0, pG / (pG + pP), NA_real_)) |>
    left_join(D$cat |> select(team_id, equipo, sede), by = "team_id") |>
    left_join(D$nombres |> group_by(team_id) |> summarise(n_nombres = n_distinct(nombre), .groups = "drop"), by = "team_id") |>
    mutate(n_nombres = coalesce(n_nombres, 1L))
  list(resumen = resumen)
}

# Hasta dónde llegó cada equipo-temporada
reach_table <- function(D) {
  s <- D$tp |> group_by(season, team_id) |> arrange(ronda_id) |> slice_tail(n = 1) |> ungroup() |>
    mutate(llego = case_when(ronda_id == 4 & gano == 1 ~ "Campeón", ronda_id == 4 ~ "Subcampeón",
                             gano == 1 ~ paste("Ganó", ronda_corta), TRUE ~ paste("Cayó en", ronda_corta))) |> select(season, team_id, llego)
  rr <- D$rr |> anti_join(s, by = c("season", "team_id")) |> transmute(season, team_id, llego = "Round robin")
  ch <- D$seasons |> filter(campeon_unico, campeon_fuente != "Serie del Rey / Final") |> transmute(season, team_id = campeon_id, llego = "Campeón")
  bind_rows(s, rr) |> anti_join(ch, by = c("season", "team_id")) |> bind_rows(ch)
}


# Récord por identidad (nombre usado) dentro de cada equipo del catálogo
build_identities <- function(D) {
  k <- c("team_id", "nombre")
  reg <- D$ts |> transmute(team_id, nombre = team, season, G, P) |> group_by(across(all_of(k))) |>
    summarise(temporadas = n_distinct(season), anios = compact_years(season_year(season)), desde = min(season_year(season)), G = sum(G), P = sum(P), .groups = "drop")
  pg <- bind_rows(D$tp |> transmute(team_id, nombre = team, pG = gw, pP = gl), D$rr |> transmute(team_id, nombre = team, pG = G, pP = P)) |>
    group_by(across(all_of(k))) |> summarise(pG = sum(pG), pP = sum(pP), .groups = "drop")
  se <- D$tp |> transmute(team_id, nombre = team, gano, final = ronda_id == 4) |> group_by(across(all_of(k))) |>
    summarise(sG = sum(gano), sP = sum(1 - gano), finales = sum(final), .groups = "drop")
  ti <- D$seasons |> filter(campeon_unico) |> transmute(team_id = campeon_id, nombre = campeon) |> count(team_id, nombre, name = "titulos")
  zo <- D$zc |> count(team_id, nombre = team, name = "zonas")
  reg |> left_join(pg, by = k) |> left_join(se, by = k) |> left_join(ti, by = k) |> left_join(zo, by = k) |>
    mutate(across(c(pG, pP, sG, sP, finales, titulos, zonas), \(x) coalesce(as.numeric(x), 0)),
           pct = G / (G + P), ppct = if_else(pG + pP > 0, pG / (pG + pP), NA_real_)) |> arrange(team_id, desde)
}
