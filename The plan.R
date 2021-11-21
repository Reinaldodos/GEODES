pacman::p_load(drake, visNetwork)
source(file = "Fonctions.R")

the_plan =
  drake::drake_plan(
    data = rio::import(file = "https://www.data.gouv.fr/en/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
                       format = "csv"),
    Regions_dep =  "departements-region.csv",
    output = process_data(data = data, Regions_dep = Regions_dep),
    
    output_nat = split_data(data = output),
    group_region = split_data(data = output, region_name),

    output_region = split(x = group_region, f = group_region$region_name),
    output_dep = split(x = output, f = output$dep),
    
    Reff_nat_hosp = Workflow_Reff(input = output_nat, selon = hosp),
    Reff_region_hosp = map(.f = Workflow_Reff, .x = output_region, selon = hosp)%>% bind_rows(.id = "Region"),
    Reff_dep_hosp = map(.f = Workflow_Reff, .x = output_dep, selon = hosp)%>% bind_rows(.id = "dep"),

    Reff_nat_rea = Workflow_Reff(input = output_nat, selon = rea),
    Reff_region_rea = map(.f = Workflow_Reff, .x = output_region, selon = rea)%>% bind_rows(.id = "Region"),
    Reff_dep_rea = map(.f = Workflow_Reff, .x = output_dep, selon = rea)%>% bind_rows(.id = "dep"),
    
    Reff_nat_plot_hosp = Reff_plot(Reff = Reff_nat_hosp, selon = "NA"),
    Reff_region_plot_hosp = Reff_plot(Reff = Reff_region_hosp, selon = Region),

    Reff_nat_plot_rea = Reff_plot(Reff = Reff_nat_rea, selon = "NA"),
    Reff_region_plot_rea = Reff_plot(Reff = Reff_region_rea, selon = Region),

    Situation_region_hosp = Situation(Reff = Reff_region_hosp),
    Situation_dep_hosp = Situation(Reff = Reff_dep_hosp),

    Situation_region_rea = Situation(Reff = Reff_region_rea),
    Situation_dep_rea = Situation(Reff = Reff_dep_rea),
    
    Plot_ARIMA_hosp = Workflow_ARIMA(output_nat = output_nat, selon = hosp),
    Plot_ARIMA_rea = Workflow_ARIMA(output_nat = output_nat, selon = rea),

    Carte = sf::st_read(dsn = "https://www.data.gouv.fr/fr/datasets/r/90b9341a-e1f7-4d75-a73c-bbc010c7feeb"),
    Bases_urbaines = sf::st_read("~/Téléchargements/fond_AAV2020_geo20/fond_AAV2020_geo20_metro/zMetro.shp"),
    
    output_GEODES = filtrer_GEODES(file = "https://www.data.gouv.fr/fr/datasets/r/c2e2e844-9671-4f81-8c81-1b79f7687de3"),
    Population = Charger_INSEE(file = "~/Téléchargements/base-ccc-evol-struct-pop-2017/base-cc-evol-struct-pop-2017.CSV"),
    output_urbaines = Incidence_aires_urbaines(input = output_GEODES, Population = Population, Bases_urbaines = Bases_urbaines),
    Dept = "~/Téléchargements/departements.geojson" %>% sf::st_read()
  )

drake::vis_drake_graph(the_plan)



