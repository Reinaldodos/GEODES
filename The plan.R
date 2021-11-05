pacman::p_load(drake, visNetwork)
source(file = "Fonctions.R")

the_plan =
  drake::drake_plan(
    data = rio::import(file = "https://www.data.gouv.fr/en/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
                       format = "csv"),
    Regions_dep =  "departements-region.csv",
    input = fetch_data(data = data, Regions_dep = Regions_dep),
    output = merdouilles_dates(input = input),
    
    output_nat = split_data(data = output),
    group_region = split_data(data = output, region_name),

    output_region = split(x = group_region, f = group_region$region_name),
    output_dep = split(x = output, f = output$dep),
    
    Incidence_nat_hosp = Calcul_incidence(input = output_nat, selon = hosp),
    Incidence_region_hosp = map(.f = Calcul_incidence, .x = output_region, selon = hosp),
    Incidence_dep_hosp = map(.f = Calcul_incidence, .x = output_dep, selon = hosp),

    Incidence_nat_rea = Calcul_incidence(input = output_nat, selon = rea),
    Incidence_region_rea = map(.f = Calcul_incidence, .x = output_region, selon = rea),
    Incidence_dep_rea = map(.f = Calcul_incidence, .x = output_dep, selon = rea),
    
    Estimation_nat_hosp = Estimate(incidence = Incidence_nat_hosp),
    Estimation_region_hosp = map(.f = Estimate, .x = Incidence_region_hosp),
    Estimation_dep_hosp = map(.f = Estimate, .x = Incidence_dep_hosp),
    
    Estimation_nat_rea = Estimate(incidence = Incidence_nat_rea),
    Estimation_region_rea = map(.f = Estimate, .x = Incidence_region_rea),
    Estimation_dep_rea = map(.f = Estimate, .x = Incidence_dep_rea),
    
    Reff_nat_hosp = Extract_Reff(estimateR = Estimation_nat_hosp),
    Reff_region_hosp = map(.f = Extract_Reff, .x = Estimation_region_hosp) %>% bind_rows(.id = "Region"),
    Reff_dep_hosp = map(.f = Extract_Reff, .x = Estimation_dep_hosp) %>% bind_rows(.id = "dep"),
    
    Reff_nat_rea = Extract_Reff(estimateR = Estimation_nat_rea),
    Reff_region_rea = map(.f = Extract_Reff, .x = Estimation_region_rea) %>% bind_rows(.id = "Region"),
    Reff_dep_rea = map(.f = Extract_Reff, .x = Estimation_dep_rea) %>% bind_rows(.id = "dep"),
    
    Reff_nat_plot_hosp = Reff_plot(Reff = Reff_nat_hosp, selon = "NA"),
    Reff_region_plot_hosp = Reff_plot(Reff = Reff_region_hosp, selon = Region),
    Reff_dep_plot_hosp = Reff_plot(Reff = Reff_dep_hosp, selon = dep),
    
    Reff_nat_plot_rea = Reff_plot(Reff = Reff_nat_rea, selon = "NA"),
    Reff_region_plot_rea = Reff_plot(Reff = Reff_region_rea, selon = Region),
    Reff_dep_plot_rea = Reff_plot(Reff = Reff_dep_rea, selon = dep),
    
    Situation_nat_hosp = Situation(Reff = Reff_nat_hosp),
    Situation_region_hosp = Situation(Reff = Reff_region_hosp),
    Situation_dep_hosp = Situation(Reff = Reff_dep_hosp),

    Situation_nat_rea = Situation(Reff = Reff_nat_rea),
    Situation_region_rea = Situation(Reff = Reff_region_rea),
    Situation_dep_rea = Situation(Reff = Reff_dep_rea),

    Carte = sf::st_read(dsn = "https://www.data.gouv.fr/fr/datasets/r/90b9341a-e1f7-4d75-a73c-bbc010c7feeb"),
    Bases_urbaines = sf::st_read("~/Téléchargements/fond_AAV2020_geo20/fond_AAV2020_geo20_metro/zMetro.shp"),
    
    input_GEODES = rio::import(file = "https://www.data.gouv.fr/fr/datasets/r/c2e2e844-9671-4f81-8c81-1b79f7687de3",
                               format = "csv"),
    output_GEODES = filtrer_GEODES(input_GEODES = input_GEODES),
    Population = Charger_INSEE(file = "~/Téléchargements/base-ccc-evol-struct-pop-2017/base-cc-evol-struct-pop-2017.CSV"),
    output_urbaines = Incidence_aires_urbaines(input = output_GEODES, Population = Population, Bases_urbaines = Bases_urbaines),
    Dept = "~/Téléchargements/departements.geojson" %>% sf::st_read()
  )

# drake::vis_drake_graph(the_plan)



