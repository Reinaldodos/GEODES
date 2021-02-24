pacman::p_load(rio, data.table, tidyverse,
               sf, cartography)

input_GEODES =
  "https://www.data.gouv.fr/fr/datasets/r/c2e2e844-9671-4f81-8c81-1b79f7687de3" %>%
  rio::import()

Bases_urbaines =
  "~/Téléchargements/fond_AAV2020_geo20/fond_AAV2020_geo20_metro/zMetro.shp" %>%
  sf::st_read()

Bases_urbaines %>%
  ggplot() +
  geom_sf(mapping = aes(fill = LIBCATE, colour = LIBCATE)) +
  scale_fill_viridis_d() +
  scale_colour_viridis_d()

input =
  input_GEODES %>%
  filter(clage_65 == 0) %>%
  tidyr::extract(
    col = semaine_glissante,
    into = c("day_start", "day_end"),
    regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})-([0-9]{4}-[0-9]{2}-[0-9]{2})",
    remove = FALSE
  )  %>%
  filter(day_end == max(day_end)) %>%
  mutate(across(.cols = starts_with("day"),
                .fns = lubridate::ymd)) %>%
  tidyr::separate(col = ti_classe,
                  into = c("low", "high"),
                  sep = ";") %>%
  mutate(across(
    .cols = c(low, high),
    .fns = str_remove_all,
    pattern = "[^[0-9]]"
  )) %>%
  mutate(across(.cols = c(low, high),
                .fns = as.integer)) 

input[is.na(input$high),]$high = 2000

Population =
  "~/Téléchargements/base-ccc-evol-struct-pop-2017/base-cc-evol-struct-pop-2017.CSV" %>%
  rio::import() %>%
  distinct(CODGEO, P17_POP) %>%
  drop_na()

output =
  inner_join(x = Population,
             y = input,
             by = c("CODGEO" = "com2020")) %>%
  mutate(LOW = low / 1e5 * P17_POP,
         HIGH = high / 1e5 * P17_POP)

PLM =
  anti_join(y = Bases_urbaines %>% as.data.frame(),
            x = output,
            by = "CODGEO")
output =
  bind_rows(
    PLM %>%
      mutate(
        AAV20 = case_when(
          str_detect(string = CODGEO, pattern = "^75") ~ "001",
          str_detect(string = CODGEO, pattern = "^69") ~ "002",
          str_detect(string = CODGEO, pattern = "^13") ~ "003"
        )
      ),
    inner_join(
      x = output,
      y = Bases_urbaines %>% 
        as.data.frame() %>% 
        distinct(CODGEO, AAV20),
      by = "CODGEO"
    )
  ) %>%
  drop_na() %>%
  split(f = .$AAV20 == "000")

output_urbaines = 
bind_rows(
output$`FALSE` %>% 
  group_by(AAV20) %>% 
  summarise(across(.cols = c(LOW, HIGH, P17_POP),
                   .fns = sum),
            .groups = "drop") %>% 
  mutate(across(.cols = c(LOW, HIGH), 
                .fns = ~ ./P17_POP*1e5)) %>% 
  inner_join(y = Bases_urbaines %>% as.data.frame() %>% 
               distinct(CODGEO, AAV20)),
output$`TRUE` %>% 
  select(CODGEO, LOW = low, HIGH = high)
) %>% 
  distinct(CODGEO, LOW, HIGH) %>% 
  inner_join(x = Bases_urbaines)

CartO <- function(data, selon) {
  GRAPHE = 
    data %>% 
    mutate(
      Incidence = cut(x = {{selon}}, 
                      breaks = c(-Inf, 10, 50, 150, 250, Inf),
                      labels = c("< 10 (Zero Covid)", 
                                 "[10;50[ (Zone verte)",
                                 "[50;150[ (Alerte)",
                                 "[150;250[ (Alerte renforcée)",
                                 "> 250 (Hors de contrôle)")
                      )
      ) %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Incidence, 
                          colour = Incidence)) +
    scale_fill_brewer(type = "div",
                      palette = "RdYlGn",
                      direction = -1) +
    scale_colour_brewer(type = "div",
                      palette = "RdYlGn",
                      direction = -1) +
  theme_void() +
    theme(
      plot.background = element_rect(fill = " light grey"),
      legend.background = element_rect(fill = "white"), 
      plot.title =  element_text(hjust = .5),
      plot.subtitle =  element_text(hjust = .5),
      plot.caption.position = "plot" 
    ) +
    labs(title = "Taux d'incidence par aire urbaine",
         caption = 
           "Données: Santé Publique France / INSEE
       Reinaldo Dos Santos @reinaldodos")
  return(GRAPHE)
}

jpeg(filename = "~/Images/Hypothese basse.jpeg", 
     # width = 1290, height = 960,
     quality = 100)
CartO(data = output_urbaines, selon = LOW) +
  labs(subtitle = paste("Hypothèse basse", lubridate::today()))
dev.off()

jpeg(filename = "~/Images/Hypothese haute.jpeg", 
     # width = 1290, height = 960,
     quality = 100)
CartO(data = output_urbaines, selon = HIGH) +
  labs(subtitle = paste("Hypothèse haute", lubridate::today()))
dev.off()
