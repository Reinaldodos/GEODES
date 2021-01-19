pacman::p_load(rio, tidyverse, data.table, lubridate, zoo)

PLOTT = function(output, VAR) {
  graphe =
    output %>%
    ggplot(mapping = aes(x = jour, y = {
      {
        VAR
      }
    }, colour = Libellé))
  
  return(graphe)
}
SEUILS <- function(graphe) {
  graphe =
    graphe +
    geom_hline(yintercept = .4,
               colour = "green",
               linetype = "dashed") +
    geom_hline(yintercept = .6,
               colour = "red",
               linetype = "dashed")
  return(graphe)
}
HISTO_GRAPHE <- function(graphe) {
  graphe =
    graphe +
    geom_bar(stat = "identity") +
    theme(legend.position = "none")
  return(graphe)
}



Capacites = "./Capacités hospitalières.xlsx" %>% rio::import()

input =
  "https://www.data.gouv.fr/en/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7" %>% 
  rio::import(format = "csv") %>%
  group_by(dep, jour) %>%
  summarise(Rea = sum(hosp, na.rm = T), .groups = "drop") 

output =
  inner_join(x = input,
             y = Capacites,
             by = "dep") %>%
  mutate_at(.vars = vars(dep, Libellé),
            .funs = as.factor) 

# gérer les merdouilles sur les dates
date_ymd = 
  output %>% 
  mutate_at(.vars = "jour", .funs = lubridate::ymd) %>% 
  drop_na(jour)

date_dmy = 
  output %>% 
  filter(str_detect(string = jour, pattern = "/")) %>% 
  mutate_at(.vars = "jour", .funs = lubridate::dmy) %>% 
  drop_na(jour)

output = 
  bind_rows(date_ymd, date_dmy)%>%
  mutate(Mobile = zoo::rollmean(
    x = Rea,
    k = 7,
    na.pad = T,
    align = "right"
  )) %>% 
  mutate(Occupation = Mobile/Lits)

Departements = 
  output %>%
  filter(jour > lubridate::dmy(11092020)) %>% 
  ggplot(mapping = aes(x = jour, 
                       y = Occupation)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ Libellé, scales = "free_y")



Curfew = 
  c(75, 77, 78, 91, 92, 93, 94, 95, 38, 59, 69, 13, 42, 76, 34, 31) %>%
  as.character()

Big_Wave =
  Capacites %>% top_n(n = 15, wt = Lits) %>% 
  semi_join(x = output, by = "dep")

Big_Wave %>%
  filter(jour > lubridate::dmy(11092020)) %>% 
  PLOTT(VAR = Mobile) %>%
  HISTO_GRAPHE() +
  facet_wrap( ~ Libellé)

Big_Wave %>%
  PLOTT(VAR = Mobile) +
  scale_y_log10() +
  geom_line(mapping = aes(colour = Libellé)) 

Vague =
  output %>%
  semi_join(y = Big_Wave) %>% 
  PLOTT(VAR = Mobile) +
  # scale_y_log10()+
  geom_line() 

plotly::ggplotly(dynamicTicks = T, p = Vague)


