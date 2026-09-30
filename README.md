# Historia de la LMB 1925-2026 (R + Shiny)

## Ejecutar
```r
# paquetes: shiny bslib DT plotly dplyr tidyr stringr purrr tibble readxl htmltools stringi
shiny::runApp()          # desde esta carpeta
```
Datos: `data/lmb_regular_post_seasons_historic.xlsx` (5 hojas: standings_1925_2026, series_result, lmb_seasons_notes, equipos_long_format, catalogo_equipos). Las fuentes de Google Fonts (Public Sans, Chivo, JetBrains Mono) se descargan al abrir la app; sin internet se usa la fuente del sistema.

## Novedades de esta versión
- **2020 (temporada cancelada):** aparece con los 16 equipos programados y la etiqueta "Temporada cancelada · pandemia de COVID-19". No cuenta para récords, campeones ni número de temporadas. Se detecta sola porque todos sus juegos están en cero.
- **Campeones de zona:** si `zone_champions` es TRUE en `lmb_seasons_notes`, los dos finalistas de la Serie del Rey son campeones de su zona (Norte o Sur, según el standing). Se muestra en el encabezado de la temporada, con una insignia ◆ en standings y en la final, y en Equipos (tarjeta, tabla por temporada y récord por nombre), Historial y Campeones.
- **Récord por identidad:** en Equipos, cada nombre usado trae años, temporadas, récord regular, récord de postemporada, títulos y campeonatos de zona.
- **Línea de tiempo:** nueva pestaña con una barra por nombre de cada equipo (catálogo + hoja long), ★ de campeonatos, ◆ de campeón de zona, banda de 2020 y filtros por orden, años y tipo de equipo.

## Identidad de equipo
Cada equipo es un `team_id` del catálogo (73). El nombre de cada temporada viene de `equipos_long_format.nombre_temporada`; en Equipos e Historial se listan todos los nombres con sus años. Mariachis de Guadalajara (id 72, 2021-2023) es un equipo aparte de Charros de Jalisco (id 47).
Standings y series se ligan al `team_id` por nombre (sin acentos ni mayúsculas) dentro de la temporada. Lo que no coincide exacto se resuelve por catálogo, por descarte o por mismo nombre en otras temporadas, y queda listado en **Método y calidad**.

## Reglas de temporada regular
1. Con standing **Final** (1993-2010) solo cuenta el Final; las dos vueltas son informativas.
2. Con dos vueltas **sin Final** (1926, 1949-51, 1966) se suman ambas.
3. El standing **Global** (2011-2026) nunca se suma.
4. El **round robin** de 1983 cuenta como postemporada. La **zona extraordinaria** de 1980 se suma aparte.
5. PCT y JV se recalculan desde G y P. 2018-1 y 2018-2 son dos campeonatos.

## Limpieza del archivo fuente
`python tools/limpiar_fuente.py entrada.xlsx salida.xlsx` quita acentos de los nombres de equipo en todas las hojas, corrige "Dorados deChihuahua", quita duplicados en `nombres_usados`, guarda `temporadas` como texto, restaura los marcadores que Excel convirtió en fecha y vacía las celdas con el texto "NA". Las notas, managers y sedes conservan acentos. La app también funciona con el archivo original (compara sin acentos).

## Pendientes en los datos (ver Método y calidad)
- 1980 está marcada con campeones de zona, pero no tiene serie final (el campeón salió del mejor récord), así que sus campeones de zona no se identifican.
- Guerreros de Oaxaca y Cafeteros de Córdoba 2000 (Centro): las dos vueltas suman menos juegos que el Final; se usa el Final. Según el usuario, es una inconsistencia de la fuente consultada.
- 15 temporadas donde G total no iguala P total (transcritas del "quién es quién" de la LMB; pendientes de cotejar).
- Campeón sin serie final: 36 temporadas definidas por mejor récord.
- En 1953 la hoja long dice "Indios Verdes de Anáhuac" para Agrario (id 32); confirmar si debe ser "Indios de Anáhuac".

## Pruebas
`Rscript tests/check_app.R` renderiza todas las salidas del servidor sin navegador.
