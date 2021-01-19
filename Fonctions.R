pacman::p_load(tidyverse, data.table, rio, 
               lubridate,
               sf, cartography)

fetch_data <- function(data, Regions_dep) {
  data %>%
    import(format = "csv") %>%
    left_join(y = Regions_dep %>% import(),
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
  Dates = input %>% pull(jour)
  REPS = input %>% pull({
    {
      selon
    }
  })
  input_list =
    pmap(.f = rep,
         .l = list(x = Dates,
                   times = REPS)) %>%
    map(as.character) %>% flatten_chr() %>% ymd()
  Incidence =  incidence(input_list)
  return(Incidence)
}

Estimate <- function(incidence) {
  require(EpiEstim)
  incidence %>%
    estimate_R(method = "parametric_si",
               config = make_config(list(mean_si = 5.71,
                                         std_si = 3.89))) %>%
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
    geom_line() +
    geom_errorbar(mapping = aes(ymin = `Quantile.0.025(R)`,
                                ymax = `Quantile.0.975(R)`)) +
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
      method = "equal", 
      nclass = N,
      col = rev(RColorBrewer::brewer.pal(n = N, 
                                         name = "RdYlGn")))
}
