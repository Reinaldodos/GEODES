pacman::p_load(drake, visNetwork)
source(file = "Fonctions.R")

the_plan =
  drake::drake_plan(
    data = "https://www.data.gouv.fr/en/datasets/r/63352e38-d353-4b54-bfd1-f1b3ee1cabd7",
    Regions_dep =  "~/Téléchargements/departements-region.csv",
    input = fetch_data(data = data, Regions_dep = Regions_dep),
    output = merdouilles_dates(input = input),
    
    output_nat = split_data(data = output),
    group_region = split_data(data = output, region_name),
    group_dep = split_data(data = output, dep),
    
    output_region = split(x = group_region, f = group_region$region_name),
    output_dep = split(x = group_dep, f = group_dep$dep),
    
    Incidence_nat = Calcul_incidence(input = output_nat, selon = hosp),
    Incidence_region = map(.f = Calcul_incidence, .x = output_region, selon = hosp),
    Incidence_dep = map(.f = Calcul_incidence, .x = output_dep, selon = hosp),
    
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
    # Carte_region = do_Carte(Carte = Carte, Situation = Situation_region, N = 10),
    # Carte_dep = do_Carte(Carte = Carte, Situation = Situation_dep, N = 10)
  )

drake::clean()
make(the_plan)
drake::vis_drake_graph(the_plan)



