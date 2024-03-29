pacman::p_load(tidyverse, data.table, rio,
               lubridate,
               sf, cartography)

fetch_data <- function(data, Regions_dep) {
  data %>%
    left_join(y = Regions_dep %>% rio::import(),
              by = c("dep" = "num_dep")) %>%
    filter(sexe == 0) %>% 
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
    filter(hosp > 0 | rea > 0) %>% 
    return()
}

Calcul_incidence <- function(input, selon) {
  require(lubridate)
  require(magrittr)
  require(incidence)
  input %<>% filter({{selon}} > 0)
  Dates = input %>% dplyr::pull(jour) %>% as.character()
  REPS = input %>% dplyr::pull({{selon}})
  
  input_list =
    pmap(.f = rep,
         .l = list(x = Dates,
                   times = REPS)) %>%
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
    inner_join(x = Situation, by = "dep") %>%
    mutate(`Probabilité R>1` = 100 * Proba) %>%
    st_as_sf() %>%
    choroLayer(
      var = "Probabilité R>1",
      # method = "quantile",
      breaks = seq(0, 100, 10),
      # nclass = N,
      col = rev(RColorBrewer::brewer.pal(n = N,
                                         name = "RdYlGn"))
    )
}

filtrer_GEODES <- function(file) {
  input =
    file %>%
    rio::import(format = "csv") %>% 
    dplyr::filter(cl_age65 == 0) %>%
    tidyr::extract(
      col = sg,
      into = c("day_start", "day_end"),
      regex = "([0-9]{4}-[0-9]{2}-[0-9]{2})-([0-9]{4}-[0-9]{2}-[0-9]{2})",
      remove = FALSE
    )  %>%
    dplyr::filter(day_end == max(day_end)) %>%
    dplyr::mutate(across(.cols = starts_with("day"),
                  .fns = lubridate::ymd)) %>%
    tidyr::separate(col = Ti,
                    into = c("low", "high"),
                    sep = "-") %>%
    dplyr::mutate(across(
      .cols = c(low, high),
      .fns = str_remove_all,
      pattern = "[^[0-9]]"
    )) %>%
    dplyr::mutate(across(.cols = c(low, high),
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
                 by = c("CODGEO" = "com")) %>%
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
  ZeroCovid = colorRampPalette(colors = c("#00441b", "#f7f7f7"))
  Covid = colorRampPalette(colors = c("#f7f7f7", "red"))
  Alarma = colorRampPalette(colors = c("red", "#000000"))
  Couleurs = c(ZeroCovid(3), Covid(4), Alarma(4)) %>% unique()
  
  GRAPHE =
    data %>%
    mutate(Incidence = cut(
      x = {
        {
          selon
        }
      },
      breaks = c(-Inf, 10, 50, 150, 250, 400, 750, 1000, 2000, Inf),
      labels = c(
        "< 10 (Zero Covid)",
        "[10; 50[ (Zone verte)",
        "[50; 150[ (Alerte)",
        "[150; 250[ (Alerte renforcée)",
        "[250; 500] (Surveillance renforcée)",
        "[500; 750] (Vague)",
        "[750; 1000] (Raz-de-marée)",
        "[1000; 2000] (Tsunami)",
        "> 2000 (Armageddon)"
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

Workflow_Reff = function(input, selon){
  Calcul_incidence(input = input, selon = {{selon}}) %>% 
    Estimate() %>% 
    Extract_Reff() %>% 
    return()
}


Lisser <- function(output_nat, selon) {
  require(tidyquant)
  output_nat %>%
    tidyquant::tq_transmute(mutate_fun = rollmean,
                            k = 7) %>%
    transmute(jour,
              Nb = {{selon}}) %>% 
    return()
}

Model_ARIMA <- function(test) {
  require(lubridate)
  Debut_vague =
    test %>%
    dplyr::filter(jour > today() - dmonths(3)) %>%
    top_n(n = 1, wt = -Nb) %>% dplyr::pull(jour)
  
  Pics = 
    test %>% 
    group_by(quarter(jour), year(jour)) %>% 
    top_n(n = 1, wt = Nb) 
  
  
  Vague_data = test 
  # Vague_data = test %>% filter(jour > Debut_vague)
  
  Modele = 
    Vague_data %>%
    timetk::tk_ts() %>% 
    log() %>% forecast::auto.arima()  
  
  Modele %>% forecast::forecast(h = 30) %>% 
    sweep::sw_sweep(timetk_idx = TRUE) %>% 
    transmute(jour = index,
              key,
              Nb = exp(Nb)) %>% 
    bind_rows(test, .) %>% 
    replace_na(replace = list(key = "actual")) %>% 
    return()
}

Graphe_ARIMA = function(graphe){
  graphe %>% 
    ggplot(mapping = aes(x = jour, y = Nb, colour = key))+
    geom_line() +
    theme_minimal() +
    expand_limits(y = 0)
}

Workflow_ARIMA = function(output_nat, selon) {
  Lisser(output_nat = output_nat,
         selon = {{selon}}) %>%
    Model_ARIMA() %>%
    Graphe_ARIMA()
}

process_data <- function(data, Regions_dep) {
  fetch_data(data = data, Regions_dep = Regions_dep) %>% 
    merdouilles_dates() %>% 
    return()
}
