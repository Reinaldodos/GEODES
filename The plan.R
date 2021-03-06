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
    group_dep = split_data(data = output, dep),
    
    output_region = split(x = group_region, f = group_region$region_name),
    output_dep = split(x = group_dep, f = group_dep$dep),
    
    Incidence_nat = Calcul_incidence(input = output_nat, selon = rea),
    Incidence_region = map(.f = Calcul_incidence, .x = output_region, selon = rea),
    Incidence_dep = map(.f = Calcul_incidence, .x = output_dep, selon = rea),
    
    Estimation_nat = Estimate(incidence = Incidence_nat),
    Estimation_region = map(.f = Estimate, .x = Incidence_region),
    Estimation_dep = map(.f = Estimate, .x = Incidence_dep),
    
    Reff_nat = Extract_Reff(estimateR = Estimation_nat),
    Reff_region = map(.f = Extract_Reff, .x = Estimation_region) %>% bind_rows(.id = "Region"),
    Reff_dep = map(.f = Extract_Reff, .x = Estimation_dep) %>% bind_rows(.id = "dep"),
    
    Reff_nat_plot = Reff_plot(Reff = Reff_nat, selon = "NA"),
    Reff_region_plot = Reff_plot(Reff = Reff_region, selon = Region),
    Reff_dep_plot = Reff_plot(Reff = Reff_dep, selon = dep),
    
    Situation_nat = Situation(Reff = Reff_nat),
    Situation_region = Situation(Reff = Reff_region),
    Situation_dep = Situation(Reff = Reff_dep),

    Carte = sf::st_read("https://www.data.gouv.fr/fr/datasets/r/90b9341a-e1f7-4d75-a73c-bbc010c7feeb"),
    Bases_urbaines = sf::st_read("~/Téléchargements/fond_AAV2020_geo20/fond_AAV2020_geo20_metro/zMetro.shp"),
    
    input_GEODES = rio::import(file = "https://www.data.gouv.fr/fr/datasets/r/c2e2e844-9671-4f81-8c81-1b79f7687de3",
                               format = "csv"),
    output_GEODES = filtrer_GEODES(input_GEODES = input_GEODES),
    Population = Charger_INSEE(file = "~/Téléchargements/base-ccc-evol-struct-pop-2017/base-cc-evol-struct-pop-2017.CSV"),
    output_urbaines = Incidence_aires_urbaines(input = output_GEODES, Population = Population, Bases_urbaines = Bases_urbaines),
    Dept = "~/Téléchargements/departements.geojson" %>% sf::st_read()
  )

drake::clean()
drake::make(the_plan)
drake::vis_drake_graph(the_plan)



