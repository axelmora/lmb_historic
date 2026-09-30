# Prueba de humo del servidor Shiny (sin navegador): ejecutar desde la carpeta de la app
suppressPackageStartupMessages(library(shiny))
app <- source("app.R")$value
testServer(app, {
  for (s in c("2026", "2022", "2020", "2018.1", "2010", "2006", "1983", "1980", "1966", "1926", "1925", "1937")) {
    session$setInputs(st_s = s, st_info = TRUE, po_s = s)
    stopifnot(nchar(as.character(output$st_head$html)) > 100, nchar(as.character(output$st_blocks$html)) > 200)
    invisible(output$po_bracket)
  }
  session$setInputs(tm = as.character(D$cat$team_id[key_of(D$cat$equipo) == "diablos rojos del mexico"]), sx_team = character(0), sx_ronda = 1:4, sx_years = c(1925, 2026))
  invisible(output$tm_head); invisible(output$tm_stats); invisible(output$tm_plot); invisible(output$tm_tbl); invisible(output$hi_tbl); invisible(output$hi_plot)
  invisible(output$sx_tbl); invisible(output$ch_tbl); invisible(output$mg_tbl); invisible(output$lg_teams); invisible(output$lg_fmt)
  session$setInputs(tl_orden = "debut", tl_filtro = "todos", tl_years = c(1925, 2026), tl_star = TRUE, tl_zone = TRUE)
  invisible(output$tl_ui); invisible(output$tl_plot); invisible(output$tl_tbl)
  for (f in c("activos", "titulo", "cambios")) { session$setInputs(tl_filtro = f); invisible(output$tl_plot) }
  invisible(output$qa_tables)
  cat("OK: todas las salidas se renderizan sin error\n")
})
