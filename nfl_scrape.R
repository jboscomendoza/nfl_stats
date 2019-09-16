# Datos obtenidos de https://www.pro-football-reference.com/

# Paquetes ----
library(tidyverse)
library(rvest)

# Funciones ----
extraer_tabla <- function(nodo_html, nodo = "#games") {
  tabla <- 
    nodo_html %>% 
    html_nodes(nodo) %>% 
    html_table(header = NA, trim = TRUE) %>% 
    data.frame() %>% 
    tbl_df() %>% 
    slice(-c(1))
}

get_completos <- function(lista) {
  seleccion <- 
    map_lgl(lista, function(tabla) {
      tabla[[3]] %>% 
        dim() %>% 
        sum() %>% 
        as.logical()
    })
  
  mi_df <- 
    map_df(lista[seleccion], function(tabla) {
      tabla_df <- tabla[[3]]
      tabla_df$equipo <- tabla$equipo
      tabla_df$temporada <- tabla$temporada
      tabla_df
    })  

  names(mi_df) <- columnas
  
  mi_df %>% 
    filter(Opponent != "Bye Week" & Date != "Playoffs") %>% 
    select(-BOX)
}

limpiar_df <- function(mi_df) {
  mi_df %>% 
    mutate_at(c(1, 10:24, 26), ~as.numeric(ifelse(. == "", "0", .))) %>% 
    mutate_if(is.character, ~ifelse(. == "", "No", .)) %>% 
    group_by(Equipo, Temporada) %>% 
    mutate(
      Playoffs = ifelse(is.na(Week), "Playoffs", "No"),
      Week = row_number()
      ) %>% 
    ungroup()  %>% 
    select(Equipo, Temporada, everything()) %>% 
    arrange(Equipo, Temporada, Week)
}

# Referencias ----
vinculo_base <- "https://www.pro-football-reference.com/teams/"

columnas <- 
  c(
    "Week", "Day", "Date", "Hour", "BOX", "Result", "OT", 
    "Record", "Local", "Opponent", "Scored", "Allowed", "FirstD", 
    "OffTotYd", "OffPassY", "OffRushY", "OffTO",
    "DefFirstD", "DefTotYd", "DefPassY", "DefRushY", "DefTO",
    "PtsOffense", "PtsDefense", "PtsSpTms",  "Equipo", "Temporada"
    )

referencia <- 
  expand.grid(
    equipo = c(
      "nwe","mia","buf", "njy", "rav","pit","cle", "cin",
      "htx","clt","oti", "jax", "kan","sdg","den", "rai",
      "dal","phi","was", "nyg", "chi","min","gnb", "det",
      "nor","atl","car", "tam", "ram","sea","sfo", "crd"
    ),
    temporada = 2008:2018, 
    stringsAsFactors = FALSE
  ) %>% 
  pmap_df(function(equipo, temporada) {
    vinculo <- paste0(vinculo_base, equipo, "/", temporada, ".htm")
    list(equipo = equipo, temporada = temporada, url = vinculo)
  })

# Extraer datos ----
nfl_html <- 
  map(referencia$url, function(x) {
    message(x)
    # Retraso de cortesía entre cada peticion
    Sys.sleep(1)
    read_html(x)
  })

nfl_stats <- 
  referencia %>% 
  pmap(list) %>% 
  map2(., nfl_html, function(x, y) {
    extraer_tabla(y) %>% 
      list(equipo = x[[1]], temporada = x[[2]],  stats = .)
  })

nfl_data <- 
  nfl_stats %>% 
  get_completos() %>% 
  limpiar_df()

# Escribir datos -----
write_rds(nfl_html, "nfl_html.rds")
write_rds(nfl_data, "nfl_data.rds")
write_csv(nfl_data, "nfl_data.csv")
