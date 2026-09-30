# ---------------------------------------------------------------------------
# Historia de la LMB 1925-2026: standings, postemporadas y equipos
# Ejecutar:  shiny::runApp()   (desde esta carpeta)
# Identidad de equipo = team_id (hoja catalogo_equipos); el nombre mostrado en cada
# temporada sale de equipos_long_format.nombre_temporada.
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(plotly)
  library(dplyr); library(tidyr); library(stringr); library(htmltools)
})
source("R/prep.R", local = TRUE)

DATA_XLSX <- "data/lmb_regular_post_seasons_historic.xlsx"
D     <- load_lmb(DATA_XLSX)
REACH <- reach_table(D)
IDENT <- build_identities(D)
SEASON_CHOICES <- with(D$seasons |> filter(hay_datos) |> arrange(desc(season)), setNames(season, paste0(etiqueta, ifelse(cancelada, " · cancelada", ""))))
CANCEL_MOTIVO <- c("2020" = "pandemia de COVID-19")
ZC_LOOKUP <- D$zc |> select(season, team_id, zn)
CANON <- setNames(D$cat$equipo, D$cat$team_id)

# ------------------------- Diseño -------------------------
C <- list(bg = "#F4F6F9", ink = "#0F172A", slate = "#1E293B", amber = "#D97706", mid = "#64748B",
          line = "#E2E8F0", soft = "#94A3B8", pale = "#CBD5E1")
theme <- bs_theme(version = 5, bg = C$bg, fg = C$ink, primary = C$slate, secondary = C$amber,
                  base_font = font_collection(font_google("Public Sans", local = FALSE), "system-ui", "-apple-system", "Segoe UI", "sans-serif"),
                  heading_font = font_collection(font_google("Chivo", local = FALSE), "system-ui", "sans-serif"),
                  code_font = font_collection(font_google("JetBrains Mono", local = FALSE), "ui-monospace", "Menlo", "monospace"), "font-size-base" = "0.95rem")
CSS <- sprintf("
:root{--slate:%1$s;--amber:%2$s;--line:%3$s;--mid:%4$s}
.navbar{box-shadow:0 1px 0 rgba(255,255,255,.08)}
.navbar-brand{font-family:'Chivo',sans-serif;font-weight:800;letter-spacing:.01em}
.nav-link.active{box-shadow:inset 0 -3px 0 var(--amber)}
h4,h5,h6{font-family:'Chivo',sans-serif;font-weight:700}
.bslib-sidebar-layout>.sidebar{background:#fff}
.hero{background:var(--slate);color:#fff;border-radius:8px;padding:1.1rem 1.4rem;margin-bottom:1.1rem}
.hero .yr{font-family:'Chivo',sans-serif;font-weight:800;font-size:2.7rem;line-height:1;letter-spacing:-.02em}
.hero .champ{margin-top:.35rem;font-size:1.15rem;font-weight:600}.hero .champ .star{color:var(--amber)}
.hero .mgr{font-weight:400;opacity:.8}.hero .fin{opacity:.85;margin:.15rem 0 .5rem}
.hero .txt{opacity:.8;max-width:80ch;font-size:.9rem;margin:.4rem 0 0}
.hero details summary{cursor:pointer;color:#fbbf24;font-size:.85rem;margin-top:.4rem}
.chip{display:inline-block;padding:.08rem .55rem;border-radius:999px;font-size:.76rem;font-weight:600;margin:.1rem .3rem .1rem 0;background:#fff;border:1px solid var(--line);color:var(--slate)}
.hero .chip{background:rgba(255,255,255,.12);border-color:rgba(255,255,255,.25);color:#fff}
.chip.count{background:#ECFDF5;border-color:#A7F3D0;color:#065F46}.chip.info{background:#F1F5F9;color:var(--mid)}
.chip.post{background:#FEF3C7;border-color:#FDE68A;color:#92400E}
.blocks{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%%,470px),1fr));gap:1.1rem}
.blk{min-width:0;background:#fff;border:1px solid var(--line);border-radius:8px;padding:.8rem .9rem;overflow-x:auto}
.blk.dim{opacity:.75}.blk h5{margin:0 0 .3rem;font-size:1.05rem}
table.stand{width:100%%;border-collapse:collapse;margin-top:.5rem;font-size:.9rem}
table.stand th{font-size:.72rem;text-transform:uppercase;letter-spacing:.05em;color:var(--mid);border-bottom:2px solid var(--slate);padding:.3rem .45rem;text-align:right;white-space:nowrap}
table.stand td{padding:.28rem .45rem;text-align:right;border-bottom:1px solid #F1F5F9}
table.stand td.n{font-family:'JetBrains Mono',monospace;font-size:.82rem}
table.stand th:nth-child(2),table.stand td:nth-child(2),table.stand th:last-child,table.stand td:last-child{text-align:left}
table.stand td:nth-child(2),table.stand td:last-child{white-space:nowrap}
table.stand th:first-child,table.stand td:first-child{text-align:center;color:var(--mid);width:1.8rem}
.alias{display:block;font-size:.72rem;color:var(--mid);font-weight:400}
tr.champ td{background:#FFFBEB;font-weight:600;box-shadow:inset 3px 0 0 var(--amber)}
tr.champ td:first-child{box-shadow:inset 3px 0 0 var(--amber)}
.bracket{display:flex;gap:1.1rem;overflow-x:auto;padding-bottom:.5rem}
.round{min-width:250px;flex:1}.round h6{font-size:.8rem;text-transform:uppercase;letter-spacing:.05em;color:var(--mid);border-bottom:2px solid var(--slate);padding-bottom:.25rem}
.series{background:#fff;border:1px solid var(--line);border-radius:6px;margin-bottom:.5rem;overflow:hidden}
.series.final{border:2px solid var(--amber)}
.series div{display:flex;justify-content:space-between;padding:.28rem .65rem}
.series .w{font-weight:700}.series .l{color:var(--mid);border-top:1px solid #F1F5F9}
.series b{font-family:'JetBrains Mono',monospace}
.stats{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:.7rem;margin-bottom:1rem}
.stat{background:#fff;border:1px solid var(--line);border-radius:8px;padding:.6rem .85rem;border-top:3px solid var(--slate)}
.stat.hl{border-top-color:var(--amber)}
.stat .k{font-size:.75rem;text-transform:uppercase;letter-spacing:.05em;color:var(--mid)}
.stat .v{font-size:1.6rem;font-weight:700;font-family:'Chivo',sans-serif}.stat .s{font-size:.82rem;color:var(--mid)}
.namecard{background:#fff;border:1px solid var(--line);border-radius:8px;padding:.7rem .9rem;margin-bottom:1rem}
.namecard .row-n{display:flex;gap:.8rem;padding:.2rem 0;border-bottom:1px solid #F1F5F9}
.namecard .yrs{font-family:'JetBrains Mono',monospace;font-size:.8rem;color:var(--mid);min-width:11rem}
.zc{display:inline-block;margin-left:.4rem;padding:0 .4rem;border-radius:999px;font-size:.7rem;font-weight:600;background:#FEF3C7;color:#92400E;border:1px solid #FDE68A;white-space:nowrap}
.hero .zc{background:rgba(217,119,6,.25);color:#FDE68A;border-color:rgba(253,230,138,.5)}
.hero.cancel{background:#475569}.hero .lineas{margin:.5rem 0 0;font-size:.92rem}
table.ident{width:100%%;border-collapse:collapse;font-size:.88rem}
table.ident th{font-size:.72rem;text-transform:uppercase;letter-spacing:.05em;color:var(--mid);border-bottom:2px solid var(--slate);padding:.3rem .5rem;text-align:right;white-space:nowrap}
table.ident td{padding:.3rem .5rem;text-align:right;border-bottom:1px solid #F1F5F9}
table.ident td.n{font-family:'JetBrains Mono',monospace;font-size:.82rem}
table.ident th:first-child,table.ident td:first-child,table.ident th:nth-child(2),table.ident td:nth-child(2){text-align:left}
.cancel-box{background:#F1F5F9;border:1px dashed var(--mid);border-radius:8px;padding:.7rem 1rem;color:var(--mid);margin-bottom:.8rem}
.dataTables_wrapper{background:#fff;border:1px solid var(--line);border-radius:8px;padding:.7rem}
table.dataTable thead th{font-size:.75rem;text-transform:uppercase;letter-spacing:.04em;color:var(--mid)}
", C$slate, C$amber, C$line, C$mid)

# ------------------------- Helpers -------------------------
fmt_jv <- function(x) ifelse(x == 0, "—", ifelse(x %% 1 == .5, paste0(ifelse(x < 1, "", floor(x)), "½"), as.character(x)))
stat_box <- function(k, v, s = NULL, hl = FALSE) div(class = paste("stat", if (hl) "hl"), div(class = "k", k), div(class = "v", v), if (!is.null(s)) div(class = "s", s))
dt_es <- function(df, ..., page = 15) datatable(df, rownames = FALSE, class = "compact stripe hover", ...,
  options = list(pageLength = page, dom = "frtip", scrollX = TRUE, language = list(search = "Buscar:", info = "_START_ a _END_ de _TOTAL_",
    infoEmpty = "Sin resultados", zeroRecords = "Sin resultados", paginate = list(previous = "Anterior", `next` = "Siguiente"))))
chip_class <- function(rol) switch(rol, regular = "count", regular_vuelta = "count", postemporada_rr = "post", "info")
names_text <- function(tid) {
  n <- D$nombres |> filter(team_id == tid)
  paste0(n$nombre, " (", n$anios, ")", collapse = " · ")
}

zc_badge <- function(season, team_id) {
  z <- ZC_LOOKUP[ZC_LOOKUP$season == season & ZC_LOOKUP$team_id == team_id, ]
  if (!nrow(z)) return(NULL)
  tags$span(class = "zc", title = "Campeón de zona", paste0("◆ Campeón de zona", if (!is.na(z$zn[1])) paste0(" ", z$zn[1]) else ""))
}
planned_table <- function(d) {
  tags$table(class = "stand", tags$thead(tags$tr(tags$th("#"), tags$th("Equipo programado"))),
             tags$tbody(lapply(seq_len(nrow(d)), function(i) tags$tr(tags$td(i), tags$td(d$team[i], if (d$team[i] != d$canon[i]) tags$span(class = "alias", d$canon[i]))))))
}
standings_table <- function(d, champ_id, season) {
  show_pts <- any(!is.na(d$puntos))
  d <- d |> left_join(REACH |> filter(season == !!season) |> select(team_id, llego), by = "team_id")
  head <- tags$tr(tags$th("#"), tags$th("Equipo"), tags$th("G"), tags$th("P"), tags$th("PCT"), tags$th("JV"),
                  if (show_pts) tags$th("Pts"), tags$th("Postemporada"))
  rows <- lapply(seq_len(nrow(d)), function(i) {
    es_c <- !is.na(champ_id) && d$team_id[i] == champ_id
    tags$tr(class = if (es_c) "champ",
      tags$td(d$pos[i]),
      tags$td(d$team[i], zc_badge(season, d$team_id[i]), if (d$team[i] != d$canon[i]) tags$span(class = "alias", d$canon[i])),
      tags$td(class = "n", d$G[i]), tags$td(class = "n", d$P[i]), tags$td(class = "n", fmt_pct(d$pct[i])), tags$td(class = "n", fmt_jv(d$jv[i])),
      if (show_pts) tags$td(class = "n", ifelse(is.na(d$puntos[i]), "", format(d$puntos[i]))), tags$td(coalesce(d$llego[i], "")))
  })
  tags$table(class = "stand", tags$thead(head), tags$tbody(rows))
}

# ------------------------- UI -------------------------
season_input <- function(id) div(class = "d-flex gap-1 align-items-end",
  div(class = "flex-grow-1", selectInput(paste0(id, "_s"), "Temporada", SEASON_CHOICES, width = "100%")),
  div(class = "mb-3", actionButton(paste0(id, "_prev"), "‹", class = "btn-sm"), actionButton(paste0(id, "_next"), "›", class = "btn-sm")))

TEAM_CHOICES <- {
  r <- D$ts |> group_by(team_id) |> summarise(a = min(season_year(season)), b = max(season_year(season)), .groups = "drop") |>
    mutate(lab = paste0(CANON[as.character(team_id)], " (", a, "–", b, ")")) |> arrange(lab)
  setNames(r$team_id, r$lab)
}
DEFAULT_TEAM <- D$cat$team_id[key_of(D$cat$equipo) == "diablos rojos del mexico"][1]

ui <- page_navbar(
  title = "Historia de la LMB", theme = theme, fillable = FALSE, bg = C$slate, inverse = TRUE,
  header = tags$head(tags$style(HTML(CSS))),
  nav_panel("Standings", layout_sidebar(sidebar = sidebar(width = 270, season_input("st"),
      checkboxInput("st_info", "Mostrar standings informativos (global, vueltas ya incluidas en el Final)", TRUE),
      helpText("Verde: cuenta en el récord histórico. Gris: solo informativo, no se suma. Bajo el nombre se muestra el equipo del catálogo cuando el nombre de la temporada es distinto.")),
    uiOutput("st_head"), uiOutput("st_blocks"))),
  nav_panel("Postemporada", navset_tab(
    nav_panel("Por temporada", layout_sidebar(sidebar = sidebar(width = 270, season_input("po")), uiOutput("po_head"), uiOutput("po_bracket"))),
    nav_panel("Todas las series", layout_sidebar(sidebar = sidebar(width = 270,
        selectizeInput("sx_team", "Equipo", choices = TEAM_CHOICES, multiple = TRUE),
        checkboxGroupInput("sx_ronda", "Ronda", setNames(seq_along(RONDA_CORTA), RONDA_CORTA), selected = 1:4),
        sliderInput("sx_years", "Años", 1925, 2026, c(1925, 2026), sep = "")), DTOutput("sx_tbl"))))),
  nav_panel("Equipos", layout_sidebar(sidebar = sidebar(width = 270, selectInput("tm", "Equipo", choices = TEAM_CHOICES, selected = DEFAULT_TEAM),
      helpText("Récord de temporada regular: sin duplicar vueltas ni global."), downloadButton("tm_dl", "Descargar CSV", class = "btn-sm")),
    uiOutput("tm_head"), uiOutput("tm_stats"), plotlyOutput("tm_plot", height = 300), h5("Temporada por temporada", class = "mt-3"), DTOutput("tm_tbl"))),
  nav_panel("Historial", helpText("Temporada regular: standing Final si existe; si solo hay dos vueltas se suman; el global nunca se suma. Postemporada: juegos de todas las series (y round robin de 1983). Cada fila es un equipo del catálogo, con todos sus nombres."),
            DTOutput("hi_tbl"), plotlyOutput("hi_plot", height = 340)),
  nav_panel("Campeones", layout_columns(col_widths = c(8, 4), DTOutput("ch_tbl"), div(h5("Managers campeones"), DTOutput("mg_tbl")))),
  nav_panel("Línea de tiempo", layout_sidebar(sidebar = sidebar(width = 270,
      selectInput("tl_orden", "Ordenar equipos por", c("Año de debut" = "debut", "Nombre del catálogo" = "nombre", "Última temporada" = "ultima")),
      selectInput("tl_filtro", "Mostrar", c("Todos los equipos" = "todos", "Activos en la última temporada" = "activos", "Con campeonato" = "titulo", "Con cambios de nombre" = "cambios")),
      sliderInput("tl_years", "Años", 1925, 2026, c(1925, 2026), sep = ""),
      checkboxInput("tl_star", "Marcar campeonatos (★)", TRUE), checkboxInput("tl_zone", "Marcar campeones de zona (◆)", TRUE),
      helpText("Fuente: catálogo de equipos y hoja long. Cada barra es un nombre; el color cambia con cada nuevo nombre del mismo equipo. Las barras separadas indican años sin participar. Las sedes vienen de la hoja long.")),
    uiOutput("tl_ui"), h5("Nombres y sedes", class = "mt-3"), DTOutput("tl_tbl"))),
  nav_panel("Liga", plotlyOutput("lg_teams", height = 300), plotlyOutput("lg_fmt", height = 260),
            helpText("Cada barra es una temporada; el color es la estructura del standing derivada del archivo.")),
  nav_panel("Método y calidad", uiOutput("qa_text"), uiOutput("qa_tables"))
)

# ------------------------- Server -------------------------
server <- function(input, output, session) {
  for (id in c("st", "po")) local({ id <- id
    step <- function(k) { v <- as.numeric(SEASON_CHOICES); i <- which(v == as.numeric(input[[paste0(id, "_s")]])) + k
      if (length(i) && i >= 1 && i <= length(v)) updateSelectInput(session, paste0(id, "_s"), selected = v[i]) }
    observeEvent(input[[paste0(id, "_prev")]], step(1)); observeEvent(input[[paste0(id, "_next")]], step(-1)) })

  H <- reactive(build_history(D))
  info <- function(s) D$seasons |> filter(season == as.numeric(s))
  header <- function(s) {
    i <- info(s); req(nrow(i) == 1)
    fin <- D$sr |> filter(season == i$season, ronda_id == 4)
    if (isTRUE(i$cancelada)) {
      mot <- CANCEL_MOTIVO[as.character(season_year(i$season))]
      return(div(class = "hero cancel", div(class = "yr", i$etiqueta),
        div(class = "champ", "Temporada cancelada", if (!is.na(mot)) span(class = "mgr", paste0("  ·  ", mot))),
        p(class = "txt", "Temporada programada que no se disputó. Se conserva en el archivo para mantener la continuidad del calendario; no cuenta para los récords ni para el número de temporadas."),
        div(tags$span(class = "chip", "Sin campeón"), tags$span(class = "chip", "Sin juegos")),
        if (!is.na(i$notas)) p(class = "txt", tags$em(i$notas))))
    }
    div(class = "hero", div(class = "yr", i$etiqueta),
      div(class = "champ", span(class = "star", "★ "), coalesce(i$campeon, "—"), if (!is.na(i$manager)) span(class = "mgr", paste0("  ·  Manager: ", i$manager))),
      if (nrow(fin)) div(class = "fin", paste0("Final: ", fin$ganador, " ", fin$w, "-", fin$l, " ", fin$perdedor)),
      div(tags$span(class = "chip", i$estructura), tags$span(class = "chip", i$campeon_fuente), if (isTRUE(i$zone_champions)) tags$span(class = "chip", "◆ Campeones de zona reconocidos")),
      if (isTRUE(i$zone_champions)) div(class = "lineas", if (!is.na(i$zc_txt)) span(tags$b("Campeones de zona: "), i$zc_txt) else span("Se reconocieron campeones de zona, pero la temporada no tiene serie final para identificarlos.")),
      if (!is.na(i$formato_campeonato)) p(class = "txt", i$formato_campeonato),
      if (!is.na(i$notas)) tags$details(tags$summary("Notas de la temporada"), p(class = "txt", tags$em(i$notas))))
  }

  # ---- Standings
  output$st_head <- renderUI(header(input$st_s))
  output$st_blocks <- renderUI({
    s <- as.numeric(input$st_s); i <- info(s); champ <- if (isTRUE(i$campeon_unico)) i$campeon_id else NA_integer_
    d <- D$st |> filter(season == s) |> mutate(ord = match(rol, c("regular", "regular_vuelta", "postemporada_rr", "cancelada", "info_vuelta", "info_global")))
    if (any(d$rol == "cancelada" & d$zona != "GLOBAL")) d <- filter(d, !(rol == "cancelada" & zona == "GLOBAL"))
    if (!isTRUE(input$st_info)) d <- filter(d, cuenta | rol == "postemporada_rr")
    bl <- d |> distinct(ord, bloque_id, bloque_txt, rol, rol_txt) |> arrange(ord, bloque_id)
    div(class = "blocks", lapply(seq_len(nrow(bl)), function(k) {
      x <- d |> filter(bloque_id == bl$bloque_id[k])
      div(class = paste("blk", if (!bl$rol[k] %in% c("regular", "regular_vuelta", "postemporada_rr")) "dim"),
          h5(bl$bloque_txt[k]), tags$span(class = paste("chip", chip_class(bl$rol[k])), bl$rol_txt[k]),
          if (bl$rol[k] == "cancelada") planned_table(x) else standings_table(x, champ, s))
    }))
  })

  # ---- Postemporada
  output$po_head <- renderUI(header(input$po_s))
  output$po_bracket <- renderUI({
    s <- as.numeric(input$po_s); x <- D$sr |> filter(season == s); rr <- D$st |> filter(season == s, rol == "postemporada_rr")
    if (isTRUE(info(s)$cancelada)) return(div(class = "cancel-box", "Temporada cancelada: no hubo postemporada."))
    if (!nrow(x) && !nrow(rr)) return(div(class = "alert alert-secondary", "No hay series registradas: el campeón se definió sin postemporada (ver encabezado)."))
    div(if (nrow(x)) div(class = "bracket", lapply(sort(unique(x$ronda_id)), function(r) {
      d <- filter(x, ronda_id == r)
      div(class = "round", h6(RONDA_CORTA[r]), lapply(seq_len(nrow(d)), function(k) div(class = paste("series", if (r == 4) "final"),
        div(class = "w", span(d$ganador[k], if (r == 4) zc_badge(s, d$id_w[k])), tags$b(d$w[k])),
        div(class = "l", span(d$perdedor[k], if (r == 4) zc_badge(s, d$id_l[k])), tags$b(d$l[k])))))})),
      if (nrow(rr)) div(class = "mt-3", h6("Round robin (contado como postemporada)"), div(class = "blocks", lapply(unique(rr$bloque_id), function(b)
        standings_table(filter(rr, bloque_id == b), NA_integer_, s)))))
  })
  output$sx_tbl <- renderDT({
    d <- D$sr |> filter(ronda_id %in% as.integer(input$sx_ronda), between(season_year(season), input$sx_years[1], input$sx_years[2]))
    if (length(input$sx_team)) { ids <- as.integer(input$sx_team); d <- filter(d, id_w %in% ids | id_l %in% ids) }
    dt_es(d |> transmute(Temporada = season_label(season), Ronda = ronda_corta, Ganador = ganador, Marcador = paste0(w, "-", l), Perdedor = perdedor), page = 20)
  })

  # ---- Equipos
  tid <- reactive({ req(input$tm); as.integer(input$tm) })
  team_seasons <- reactive({
    D$ts |> filter(team_id == tid()) |> left_join(REACH, by = c("season", "team_id")) |>
      left_join(D$seasons |> select(season, etiqueta, campeon_id, campeon_unico, manager), by = "season") |>
      mutate(es_campeon = coalesce(campeon_unico & campeon_id == team_id, FALSE), llego = coalesce(llego, "—"),
             tag = paste0(etiqueta, if_else(extra, "E", "")),
             cat = case_when(es_campeon ~ "Campeón", llego == "Subcampeón" ~ "Subcampeón", llego != "—" ~ "Postemporada", TRUE ~ "Sin postemporada")) |>
      arrange(season, extra)
  })
  output$tm_head <- renderUI({
    c1 <- D$cat |> filter(team_id == tid()); n <- IDENT |> filter(team_id == tid())
    pctf <- function(g, p) ifelse(g + p > 0, fmt_pct(g / (g + p)), "—")
    head <- tags$tr(tags$th("Nombre"), tags$th("Años"), tags$th("Temp."), tags$th("Regular"), tags$th("PCT"), tags$th("Post."), tags$th("PCT post."), tags$th("Títulos"), tags$th("Camp. zona"))
    rows <- lapply(seq_len(nrow(n)), function(i) tags$tr(tags$td(n$nombre[i]), tags$td(class = "n", n$anios[i]), tags$td(class = "n", n$temporadas[i]),
      tags$td(class = "n", paste0(n$G[i], "-", n$P[i])), tags$td(class = "n", pctf(n$G[i], n$P[i])),
      tags$td(class = "n", paste0(n$pG[i], "-", n$pP[i])), tags$td(class = "n", pctf(n$pG[i], n$pP[i])),
      tags$td(class = "n", n$titulos[i]), tags$td(class = "n", n$zonas[i])))
    div(class = "namecard", h4(class = "mb-1", c1$equipo), if (!is.na(c1$sede)) div(class = "text-muted mb-2", paste("Sede principal:", c1$sede)),
        h6(if (nrow(n) > 1) "Nombres utilizados y récord con cada identidad" else "Nombre utilizado y récord"),
        div(style = "overflow-x:auto", tags$table(class = "ident", tags$thead(head), tags$tbody(rows))),
        helpText("Récord de temporada regular sin duplicar vueltas ni global; la postemporada incluye todas las series. Campeón de zona: finalista de una temporada en la que se reconocieron campeones de zona."))
  })
  output$tm_stats <- renderUI({
    r <- filter(H()$resumen, team_id == tid()); req(nrow(r) == 1)
    div(class = "stats",
      stat_box("Temporada regular", paste0(r$G, "-", r$P), paste0("PCT ", fmt_pct(r$pct))),
      stat_box("Postemporada (juegos)", paste0(r$pG, "-", r$pP), if (!is.na(r$ppct)) paste0("PCT ", fmt_pct(r$ppct)) else "sin juegos"),
      stat_box("Series ganadas-perdidas", paste0(r$sG, "-", r$sP)),
      stat_box("Campeonatos", r$titulos, paste0(r$finales, " finales jugadas"), hl = TRUE),
      stat_box("Campeón de zona", r$zonas, "como finalista en temporadas con zonas reconocidas"),
      stat_box("Temporadas", r$temporadas, paste0(r$primera, " a ", r$ultima)))
  })
  output$tm_plot <- renderPlotly({
    d <- team_seasons(); pal <- c("Campeón" = C$amber, "Subcampeón" = C$slate, "Postemporada" = C$soft, "Sin postemporada" = C$pale)
    plot_ly(d, x = ~factor(tag, unique(tag)), y = ~pct, color = ~factor(cat, names(pal)), colors = pal, type = "bar",
            text = ~paste0(tag, ": ", team, "<br>", G, "-", P, " (", fmt_pct(pct), ")<br>", llego, if_else(es_zc, paste0("<br>◆ Campeón de zona ", coalesce(zona_camp, "")), ""), "<br>", regla), hoverinfo = "text") |>
      layout(font = list(family = "Public Sans"), xaxis = list(title = "", tickangle = -60), yaxis = list(title = "PCT", range = c(0, 1)), legend = list(orientation = "h", y = 1.15),
             shapes = list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = .5, y1 = .5, line = list(color = C$mid, dash = "dot"))))
  })
  tm_out <- reactive(team_seasons() |> transmute(Temporada = tag, `Nombre ese año` = team, Bloque = zona, `Regla aplicada` = regla, G, P, PCT = round(pct, 3),
      Lugar = if_else(is.na(pos_bloque), "—", paste0(pos_bloque, "°", if_else(is.na(pos_global), "", paste0(" (global ", pos_global, "°)")))),
      Postemporada = llego, `Campeón de zona` = if_else(es_zc, coalesce(zona_camp, "Sí"), ""), `Manager campeón` = if_else(es_campeon, coalesce(manager, ""), "")))
  output$tm_tbl <- renderDT(dt_es(tm_out(), page = 12) |> formatRound("PCT", 3))
  output$tm_dl <- downloadHandler(function() paste0("lmb_", str_replace_all(tolower(CANON[[as.character(tid())]]), "[^a-z0-9]+", "_"), ".csv"),
                                  function(file) write.csv(tm_out(), file, row.names = FALSE, fileEncoding = "UTF-8"))

  # ---- Historial
  output$hi_tbl <- renderDT({
    nm <- D$nombres |> group_by(team_id) |> summarise(Nombres = paste0(nombre, " (", anios, ")", collapse = " · "), .groups = "drop")
    H()$resumen |> left_join(nm, by = "team_id") |> arrange(desc(G + P)) |>
      transmute(Equipo = equipo, Temporadas = temporadas, Desde = primera, Hasta = ultima, JJ = G + P, G, P, PCT = pct,
                `Post G` = pG, `Post P` = pP, `Post PCT` = ppct, `Series G` = sG, `Series P` = sP, Finales = finales, Títulos = titulos, `Camp. de zona` = zonas, Nombres = coalesce(Nombres, equipo)) |>
      dt_es(page = 20) |> formatRound(c("PCT", "Post PCT"), 3)
  })
  output$hi_plot <- renderPlotly({
    r <- H()$resumen |> filter(titulos > 0) |> arrange(titulos) |> mutate(equipo = factor(equipo, equipo))
    plot_ly(r, y = ~equipo, x = ~titulos, type = "bar", orientation = "h", marker = list(color = C$amber),
            text = ~paste0(titulos, " títulos, ", finales, " finales"), hoverinfo = "text") |>
      layout(font = list(family = "Public Sans"), title = list(text = "Campeonatos por equipo", x = 0), xaxis = list(title = ""), yaxis = list(title = ""), margin = list(l = 190))
  })

  # ---- Campeones
  output$ch_tbl <- renderDT({
    fin <- D$sr |> filter(ronda_id == 4) |> group_by(season) |> slice_tail(n = 1) |> ungroup() |> transmute(season, Subcampeón = perdedor, Final = paste0(w, "-", l))
    D$seasons |> filter(hay_datos) |> left_join(fin, by = "season") |> arrange(desc(season)) |>
      transmute(Temporada = etiqueta, Campeón = coalesce(campeon, "—"), Manager = manager, Subcampeón = coalesce(Subcampeón, ""), Final = coalesce(Final, ""),
                `Campeones de zona` = if_else(zone_champions, coalesce(zc_txt, "Sí (sin serie final)"), ""), Definición = campeon_fuente) |> dt_es(page = 20)
  })
  output$mg_tbl <- renderDT(D$seasons |> filter(!is.na(manager), campeon_unico) |> group_by(Manager = manager) |>
      summarise(Títulos = n(), Años = paste(etiqueta, collapse = ", "), .groups = "drop") |> arrange(desc(Títulos)) |> dt_es(page = 12))

  # ---- Liga
  output$lg_teams <- renderPlotly({
    d <- D$ts |> filter(!extra) |> group_by(season) |> summarise(equipos = n_distinct(team_id), jpe = mean(G + P), .groups = "drop")
    plot_ly(d, x = ~season, y = ~equipos, type = "scatter", mode = "lines+markers", name = "Equipos", line = list(color = C$slate), marker = list(color = C$slate),
            text = ~paste0(season_label(season), ": ", equipos, " equipos"), hoverinfo = "text") |>
      add_lines(y = ~jpe, name = "Juegos por equipo", yaxis = "y2", line = list(color = C$amber), text = ~paste0(season_label(season), ": ", round(jpe), " JJ"), hoverinfo = "text") |>
      layout(font = list(family = "Public Sans"), xaxis = list(title = ""), yaxis = list(title = "Equipos"), yaxis2 = list(overlaying = "y", side = "right", title = "Juegos por equipo"), legend = list(orientation = "h", y = 1.15))
  })
  output$lg_fmt <- renderPlotly(plot_ly(D$seasons |> filter(hay_datos), x = ~season, y = 1, color = ~estructura, type = "bar",
      colors = c("#1E293B", "#D97706", "#64748B", "#F59E0B", "#0F766E", "#94A3B8", "#7C2D12", "#475569", "#B45309", "#334155", "#0EA5E9", "#CBD5E1"),
      text = ~paste0(etiqueta, ": ", estructura), hoverinfo = "text") |>
      layout(font = list(family = "Public Sans"), barmode = "stack", yaxis = list(visible = FALSE), xaxis = list(title = ""), legend = list(orientation = "h", y = -.2)))

  # ---- Línea de tiempo
  tl_runs <- reactive({
    r <- D$runs |> filter(hasta >= input$tl_years[1], desde <= input$tl_years[2])
    last <- max(D$runs$hasta)
    ids <- switch(input$tl_filtro,
      activos = D$runs$team_id[D$runs$hasta == last],
      titulo  = D$seasons$campeon_id[D$seasons$campeon_unico],
      cambios = (D$runs |> count(team_id) |> filter(n > 1))$team_id, unique(D$runs$team_id))
    r <- filter(r, team_id %in% ids)
    base <- D$runs |> group_by(canon) |> summarise(debut = min(desde), fin = max(hasta), .groups = "drop")
    ord <- switch(input$tl_orden, debut = arrange(base, debut, canon), nombre = arrange(base, canon), ultima = arrange(base, desc(fin), canon))$canon
    list(r = r, ord = ord[ord %in% r$canon])
  })
  output$tl_ui <- renderUI({ n <- length(tl_runs()$ord); plotlyOutput("tl_plot", height = paste0(max(n * 21 + 130, 260), "px")) })
  output$tl_plot <- renderPlotly({
    t <- tl_runs(); r <- t$r; req(nrow(r) > 0); ord <- t$ord
    pal <- c("#1E293B", "#D97706", "#0F766E", "#7C3AED", "#BE123C", "#0369A1", "#65A30D", "#A16207")
    r <- r |> mutate(w = hasta - desde + 1, col = pal[pmin(idx, length(pal))], lab = if_else(w >= 6, nombre, ""),
                     tip = paste0("<b>", nombre, "</b><br>", desde, if_else(hasta > desde, paste0("–", hasta), ""), " (", w, if_else(w == 1, " año", " años"), ")<br>Equipo: ", canon, "<br>Sede: ", coalesce(sede, "—")))
    p <- plot_ly(r, type = "bar", orientation = "h", y = ~canon, x = ~w, base = ~desde, marker = list(color = ~col, line = list(color = C$bg, width = 1.2)),
                 text = ~lab, textposition = "inside", insidetextanchor = "start", textfont = list(color = "white", size = 10, family = "Public Sans"),
                 hovertext = ~tip, hoverinfo = "text", showlegend = FALSE)
    if (isTRUE(input$tl_zone)) { z <- D$zc |> inner_join(D$cat |> select(team_id, canon = equipo), by = "team_id") |> filter(canon %in% ord, season_year(season) >= input$tl_years[1], season_year(season) <= input$tl_years[2])
      if (nrow(z)) p <- add_trace(p, data = z, inherit = FALSE, type = "scatter", mode = "markers", x = ~season_year(season) + .3, y = ~canon, name = "Campeón de zona",
                                  marker = list(symbol = "diamond", size = 7, color = "#FDE68A", line = list(color = "#92400E", width = 1)),
                                  hovertext = ~paste0(season_label(season), ": ", team, " · campeón de zona ", coalesce(zn, "")), hoverinfo = "text") }
    if (isTRUE(input$tl_star)) { ch <- D$seasons |> filter(campeon_unico) |> transmute(season, canon = CANON[as.character(campeon_id)], campeon) |> filter(canon %in% ord, season_year(season) >= input$tl_years[1], season_year(season) <= input$tl_years[2])
      if (nrow(ch)) p <- add_trace(p, data = ch, inherit = FALSE, type = "scatter", mode = "markers", x = ~season_year(season) + .7, y = ~canon, name = "Campeón",
                                   marker = list(symbol = "star", size = 10, color = "#FBBF24", line = list(color = "white", width = .8)),
                                   hovertext = ~paste0(season_label(season), ": ", campeon, " · campeón"), hoverinfo = "text") }
    canc <- D$seasons |> filter(cancelada) |> pull(season) |> season_year()
    shp <- lapply(canc, \(y) list(type = "rect", xref = "x", yref = "paper", x0 = y, x1 = y + 1, y0 = 0, y1 = 1, fillcolor = "rgba(100,116,139,0.18)", line = list(width = 0), layer = "above"))
    ann <- lapply(canc, \(y) list(x = y + .5, y = 1, xref = "x", yref = "paper", text = paste(y, "· cancelada"), showarrow = FALSE, yanchor = "bottom", yshift = 16, font = list(size = 10, color = C$mid)))
    p |> layout(font = list(family = "Public Sans", size = 11), barmode = "overlay", bargap = .25, shapes = shp, annotations = ann,
                xaxis = list(title = "", range = c(input$tl_years[1], input$tl_years[2] + 1), side = "top", dtick = 10, gridcolor = "#E2E8F0"),
                yaxis = list(title = "", autorange = "reversed", categoryorder = "array", categoryarray = ord, automargin = TRUE),
                legend = list(orientation = "h", y = -0.02), margin = list(t = 60, l = 10, r = 10))
  })
  output$tl_tbl <- renderDT({
    t <- tl_runs(); t$r |> mutate(canon = factor(canon, t$ord)) |> arrange(canon, desde) |>
      transmute(Equipo = as.character(canon), Nombre = nombre, Desde = desde, Hasta = hasta, Años = hasta - desde + 1, Sede = coalesce(sede, "")) |> dt_es(page = 15)
  })

  # ---- Método y calidad
  output$qa_text <- renderUI(div(class = "mb-3", h4("Cómo se contabiliza"), tags$ol(
    tags$li("Cada equipo es un team_id del catálogo. El nombre de cada temporada viene de equipos_long_format; el resto de nombres usados aparece en Equipos e Historial. Mariachis de Guadalajara (2021-2023) es un equipo distinto de Charros de Jalisco."),
    tags$li("Si la temporada tiene standing Final (1993-2010), solo cuenta el Final; las vueltas son informativas."),
    tags$li("Si hay dos vueltas sin Final (1926, 1949-51, 1966), se suman ambas."),
    tags$li("El standing Global (2011-2026) nunca se suma: ya está incluido en el standing por zona."),
    tags$li("El round robin de 1983 se cuenta como postemporada. La zona extraordinaria de 1980 son juegos aparte y se suman."),
    tags$li("PCT y JV se recalculan desde G y P. 2018-1 y 2018-2 son dos campeonatos."),
    tags$li("Si no hay serie final, el campeón es el equipo con mejor récord (los empates se marcan)."),
    tags$li("Temporada cancelada (2020): aparece con los equipos programados, pero sin récord, sin campeón y sin contar como temporada jugada."),
    tags$li("Campeones de zona: cuando la temporada tiene zone_champions = TRUE, los dos finalistas de la Serie del Rey son campeones de su zona (Norte o Sur, según el standing).")),
    p("Postemporada: los juegos salen de los marcadores de cada serie.")))
  output$qa_tables <- renderUI({
    q <- D$qa; nm <- c(acciones = "Correcciones aplicadas al leer", resolucion = "Nombres de standings asignados a un equipo sin coincidencia exacta en la hoja long",
      series_resolucion = "Nombres de series asignados sin coincidencia exacta", catalogo_vs_long = "Nombre del catálogo distinto al canónico de la hoja long",
      catalogo_temporadas = "Temporadas del catálogo distintas a las de la hoja long", long_sin_standing = "Hoja long con equipo activo pero sin fila en standings",
      long_sin_temporada = "Hoja long con años sin standings", pct = "PCT del archivo distinto a G/(G+P)",
      final_vs_vueltas = "Final distinto a la suma de vueltas (se usa el Final)", balance = "Temporadas donde G total no iguala P total",
      zonas = "Campeones de zona: temporadas que no se pueden identificar", series_sin_standing = "Equipos en series sin standing esa temporada", temporadas_sin_serie = "Temporadas sin series (campeón por récord)")
    tagList(lapply(names(nm), function(k) { id <- paste0("qa_", k); local({ k <- k; output[[id]] <- renderDT(dt_es(q[[k]], page = 8)) })
      div(class = "mb-4", h5(paste0(nm[[k]], " (", nrow(q[[k]]), ")")), DTOutput(id)) }))
  })
}
shinyApp(ui, server)
