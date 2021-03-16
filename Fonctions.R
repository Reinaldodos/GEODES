pacman::p_load(tidyverse, data.table, rio,
               lubridate,
               sf, cartography)

fetch_data <- function(data, Regions_dep) {
  data %>%
    left_join(y = Regions_dep %>% rio::import(),
              by = c("dep" = "num_dep")) %>%
    return()
}

split_data <- function(data, ...) {
  data %>%
    group_by(jour, ...) %>%
    summarise(across(
      .cols = c("hosp", "rea"),
      .fns = sum,
      na.rm = T
    ),
    .groups = "drop") %>%
    return()
}

merdouilles_dates <- function(input) {
  require(lubridate)
  date_ymd =
    input %>%
    mutate_at(.vars = "jour", .funs = ymd) %>%
    drop_na(jour)
  
  date_dmy =
    input %>%
    filter(str_detect(string = jour, pattern = "/")) %>%
    mutate_at(.vars = "jour", .funs = dmy) %>%
    drop_na(jour)
  
  
  bind_rows(date_ymd, date_dmy) %>%
    return()
}

Calcul_incidence <- function(input, selon) {
  require(lubridate)
  require(incidence)
  Dates = input %>% pull(jour) %>% as.character()
  REPS = input %>% pull({
    {
      selon
    }
  })
  input_list =
    pmap(.f = rep,
         .l = list(x = Dates,
                   times = REPS)) %>%
    # map(as.character) %>%
    flatten_chr() %>% ymd()
  Incidence =  incidence(input_list)
  return(Incidence)
}

Estimate <- function(incidence) {
  require(EpiEstim)
  incidence %>%
    estimate_R(method = "parametric_si",
               config = EpiEstim::make_config(list(mean_si = 3.96,
                                                   std_si = 4.75))) %>%
    return()
}

Extract_Reff <- function(estimateR) {
  estimateR$dates %>% data.frame(Date = .) %>%
    rowid_to_column() %>%
    left_join(x = estimateR$R,
              by = c("t_end" = "rowid")) %>%
    return()
}

Reff_plot <- function(Reff, selon) {
  plot =
    Reff %>%
    ggplot(mapping = aes(x = Date,
                         y = `Median(R)`,
                         colour = {
                           {
                             selon
                           }
                         })) +
    # geom_line() +
    geom_ribbon(
      mapping = aes(ymin = `Quantile.0.025(R)`,
                    ymax = `Quantile.0.975(R)`),
      alpha = .5
    ) +
    geom_hline(yintercept = 1, colour = "red") +
    geom_vline(xintercept =
                 c(dmy(13032020),
                   dmy(11052020),
                   dmy(30102020),
                   dmy(28112020)),
               linetype = "dashed")
  return(plot)
}

Situation = function(Reff) {
  Reff %>%
    filter(Date == max(Date)) %>%
    mutate(zscore = (`Median(R)` - 1) / `Std(R)`) %>%
    mutate(Proba = pnorm(q = `Median(R)` - 1, sd = `Std(R)`)) %>%
    return()
}

do_Carte <- function(Carte, Situation, N) {
  Carte %>%
    select(dep = code, geometry) %>%
    inner_join(x = Situation) %>%
    mutate(Proba = 100 * Proba) %>%
    st_as_sf() %>%
    choroLayer(
      var = "Proba",
      # method = "quantile",
      breaks = seq(0, 100, 10),
      # nclass = N,
      col = rev(RColorBrewer::brewer.pal(n = N,
                                         name = "RdYlGn"))
    )
}

filtrer_GEODES <- function(input_GEODES) {
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
  
  input[is.na(input$high), ]$high = 2000
  
  return(input)
}

Charger_INSEE = function(file) {
  rio::import(file) %>%
    distinct(CODGEO, P17_POP) %>%
    drop_na()
}

Incidence_aires_urbaines <-
  function(Population, input, Bases_urbaines) {
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
          summarise(across(
            .cols = c(LOW, HIGH, P17_POP),
            .fns = sum
          ),
          .groups = "drop") %>%
          mutate(across(
            .cols = c(LOW, HIGH),
            .fns = ~ . / P17_POP * 1e5
          )) %>%
          inner_join(y = Bases_urbaines %>% as.data.frame() %>%
                       distinct(CODGEO, AAV20)),
        output$`TRUE` %>%
          select(CODGEO, LOW = low, HIGH = high)
      ) %>%
      distinct(CODGEO, LOW, HIGH) %>%
      inner_join(x = Bases_urbaines)
    
    return(output_urbaines)
  }

CartO <- function(data, selon) {
  require(magrittr)
  Couleurs =
    RColorBrewer::brewer.pal(n = 5, name = "BrBG") %>% 
    c("#000000", .) %>% rev()
  
  GRAPHE =
    data %>%
    mutate(Incidence = cut(
      x = {
        {
          selon
        }
      },
      breaks = c(-Inf, 10, 50, 150, 250, 400, Inf),
      labels = c(
        "< 10 (Zero Covid)",
        "[10;50[ (Zone verte)",
        "[50;150[ (Alerte)",
        "[150;250[ (Alerte renforcée)",
        "> 250 (Surveillance renforcée)",
        "> 400 (Point de vigilance très fort)"
      )
    )) %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Incidence,
                          colour = Incidence)) +
    scale_fill_manual(values = Couleurs) +
    scale_colour_manual(values = Couleurs) +
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

