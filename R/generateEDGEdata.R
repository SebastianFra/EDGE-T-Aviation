#' Generate EDGE-Transport Input Data for the REMIND model.
#'
#' Run this script to prepare the input data for EDGE in EDGE-friendly units and regional aggregation
#' @param input_folder folder hosting raw data
#' @param output_folder folder hosting REMIND input files
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param REMIND_scenario SSP scenario
#' @param IEAbal use mrremind generated data: in case of a REMIND preprocessing run, load population.  Product of: mrremind::calcOutput("IO", subtype = "IEA_output", aggregate = TRUE)
#' @param GDP_country use mrremind generated data: in case of a REMIND preprocessing run, load GDP.  Product of: mrremind::calcOutput("GDPppp", aggregate =F)
#' @param POP_country use mrremind generated data: in case of a REMIND preprocessing run, load IEA balances. Product of: mrremind::calcOutput("Population", aggregate =F)
#' @param saveRDS optional saving of intermediate RDS files
#'
#' @return generated EDGE-transport input data
#' @author Alois Dirnaichner, Marianna Rottoli, Sebastian Franz
#' @import data.table
#' @importFrom edgeTrpLib merge_prices calculate_logit_inconv_endog calcVint shares_intensity_and_demand calculate_capCosts prepare4REMIND calc_num_vehicles_stations
#' @importFrom rmarkdown render
#' @export


generateEDGEdata <- function(input_folder, output_folder,
                             EDGE_scenario, REMIND_scenario="SSP2", COVID_scenario=TRUE,
                             IEAbal=NULL, GDP_country=NULL, POP_country=NULL,
                             saveRDS=FALSE){

  scenario <- scenario_name <- vehicle_type <- type <- `.` <- CountryCode <- RegionCode <- NULL
  non_fuel_price <- tot_price <- fuel_price_pkm <- subsector_L1 <- loadFactor <- NULL
  Year <- value <- DP_cap <- POP_val <- GDP_cap <- region <- weight <- NULL
  levelNpath <- function(fname, N){
    path <- file.path(output_folder, REMIND_scenario,COVID_scenario, EDGE_scenario, paste0("level_", N))
    if(!dir.exists(path)){
      dir.create(path, recursive = T)
    }
    return(file.path(path, fname))
  }

  level0path <- function(fname){
    levelNpath(fname, 0)
  }

  level1path <- function(fname){
    levelNpath(fname, 1)
  }

  level2path <- function(fname){
    levelNpath(fname, 2)
  }


  years <- c(1990,
             seq(2005, 2060, by = 5),
             seq(2070, 2110, by = 10),
             2130, 2150)
  ## load mappings
  REMIND2ISO_MAPPING = fread(system.file("extdata", "regionmapping_21_EU11.csv", package = "edgeTransport"))[, .(iso = CountryCode,region = RegionCode)]
  EDGEscenarios = fread(system.file("extdata", "EDGEscenario_description.csv", package = "edgeTransport"))
  GCAM2ISO_MAPPING = fread(system.file("extdata", "iso_GCAM.csv", package = "edgeTransport"))
  EDGE2teESmap = fread(system.file("extdata", "mapping_EDGE_REMIND_transport_categories.csv", package = "edgeTransport"))
  EDGE2CESmap = fread(system.file("extdata", "mapping_CESnodes_EDGE.csv", package = "edgeTransport"))

  ## load specific transport switches
  EDGEscenarios <- EDGEscenarios[scenario_name == EDGE_scenario]

  selfmarket_taxes <- EDGEscenarios[options == "selfmarket_taxes", switch]
  print(paste0("You selected self-sustaining market, option Taxes to: ", selfmarket_taxes))
  enhancedtech <- EDGEscenarios[options== "enhancedtech", switch]
  print(paste0("You selected the option to select an optimistic trend of costs/performances of alternative technologies to: ", enhancedtech))
  rebates_febates <- EDGEscenarios[options== "rebates_febates", switch]
  print(paste0("You selected the option to include rebates and ICE costs markup to: ", rebates_febates))
  smartlifestyle <- EDGEscenarios[options== "smartlifestyle", switch]
  print(paste0("You selected the option to include lifestyle changes to: ", smartlifestyle))

  if (EDGE_scenario %in% c("ConvCase", "ConvCaseWise")) {
    techswitch <- "Liquids"
  } else if (EDGE_scenario %in% c("ElecEra", "ElecEraWise")) {
    techswitch <- "BEV"
  } else if (EDGE_scenario %in% c("HydrHype", "HydrHypeWise")) {
    techswitch <- "FCEV"
  } else {
    print("You selected a not allowed scenario. Scenarios allowed are: ConvCase, ConvCaseWise, ElecEra, ElecEraWise, HydrHype, HydrHypeWise")
    quit()
  }

  print(paste0("You selected the ", EDGE_scenario, " transport scenario."))
  print(paste0("You selected the ", REMIND_scenario, " socio-economic scenario."))
  #################################################
  ## LVL 0 scripts
  #################################################
  print("-- Start of level 0 scripts")

  ## load the GDP, POP, IEA from default input data if not provided
  mrremind_folder = file.path(input_folder, "mrremind")
  if (is.null(IEAbal)) IEAbal = readRDS(file.path(mrremind_folder, "IEAbal.RDS"))
  if (is.null(GDP_country)) GDP_country = readRDS(file.path(mrremind_folder, "GDP_country.RDS"))
  if (is.null(POP_country)) POP_country = readRDS(file.path(mrremind_folder, "POP_country.RDS"))

  ## rearrange the columns and create regional values
  GDP_country = GDP_country[,, REMIND_scenario, pmatch=TRUE]
  GDP_country <- as.data.table(GDP_country)
  GDP_country[, year := as.numeric(gsub("y", "", Year))][, Year := NULL]
  setnames(GDP_country, old = "value", new = "weight")
  GDP = merge(GDP_country, REMIND2ISO_MAPPING, by.x = "ISO3", by.y = "iso")
  GDP = GDP[,.(weight = sum(weight)), by = c("region", "year")]
  setnames(GDP_country, c("ISO3"), c("iso"))


  POP_country = POP_country[,, as.numeric(gsub("\\D", "", REMIND_scenario)),pmatch=TRUE]
  POP_country <- as.data.table(POP_country)
  POP_country[, year := as.numeric(gsub("y", "", year))]
  POP = merge(POP_country, REMIND2ISO_MAPPING, by.x = "iso2c", by.y = "iso")
  POP = POP[,.(value = sum(value)), by = c("region", "year")]
  setnames(POP_country, old = c("iso2c", "variable"), new = c("iso", "POP"),skip_absent=TRUE)

  GDP_POP=merge(GDP,POP[,.(region,year,POP_val=value)],all = TRUE,by=c("region","year"))
  GDP_POP[,GDP_cap:=weight/POP_val]


  GDP_POP_cap=merge(GDP,POP[,.(region,year,POP_val=value)],all = TRUE,by=c("region","year"))
  GDP_POP_cap[,GDP_cap:=weight/POP_val]
  ## function that loads raw data from the GCAM input files and modifies them, to make them compatible with EDGE setup
  ## demand in million pkm and tmk, EI in MJ/km
  print("-- load GCAM raw data")
  GCAM_data <- lvl0_GCAMraw(input_folder)

  ##function that loads PSI energy intensity for Europe (all LDVs) and for other regions (only alternative vehicles LDVs) and merges them with GCAM intensities. Final values: MJ/km (pkm and tkm)
  print("-- merge PSI energy intensity data")
  intensity_PSI_GCAM_data <- lvl0_mergePSIintensity(GCAM_data, input_folder, enhancedtech = enhancedtech, techswitch = techswitch)
  GCAM_data$conv_pkm_mj = intensity_PSI_GCAM_data


  ## function that calculates VOT for each level and logit exponents for each level.Final values: VOT in [1990$/pkm]
  print("-- load value-of-time and logit exponents")
  VOT_lambdas=lvl0_VOTandExponents(GCAM_data, GDP_country, POP_country = POP_country, REMIND_scenario, input_folder, GCAM2ISO_MAPPING)

  ## function that loads and prepares the non_fuel prices. It also load PSI-based purchase prices for EU. Final values: non fuel price in 1990USD/pkm (1990USD/tkm), annual mileage in vkt/veh/yr (vehicle km traveled per year),non_fuel_split in 1990USD/pkt (1990USD/tkm)
  print("-- load UCD database")
  UCD_output <- lvl0_loadUCD(GCAM_data = GCAM_data, GDP_country = GDP_country, EDGE_scenario = EDGE_scenario, REMIND_scenario = REMIND_scenario, GCAM2ISO_MAPPING = GCAM2ISO_MAPPING,
                            input_folder = input_folder, years = years, enhancedtech = enhancedtech, selfmarket_taxes = selfmarket_taxes, rebates_febates = rebates_febates, techswitch = techswitch)

  ## function that integrates GCAM data. No conversion of units happening.
  print("-- correct tech output")
  correctedOutput <- lvl0_correctTechOutput(GCAM_data,
                                            UCD_output$non_energy_cost,
                                            VOT_lambdas$logit_output)
  dem_int= list()
  costs_lf_mile = list()
  dem_int$tech_output = correctedOutput$GCAM_output$tech_output
  dem_int$conv_pkm_mj = correctedOutput$GCAM_output$conv_pkm_mj
  costs_lf_mile$non_energy_cost = correctedOutput$NEcost$non_energy_cost
  costs_lf_mile$non_energy_cost_split = correctedOutput$NEcost$non_energy_cost_split
  costs_lf_mile$load_factor = UCD_output$non_energy_cost$load_factor
  costs_lf_mile$annual_mileage = UCD_output$annual_mileage
  VOT_lambdas$logit_output = correctedOutput$logitexp



  ## produce regionalized versions, and ISO version of the tech_output and LF, as they are loaded on ISO level in TRACCS. No conversion of units happening.
  print("-- generate ISO level data")
  iso_data <- lvl0_toISO(
    input_data = dem_int,
    VOT_data = VOT_lambdas$VOT_output,
    price_nonmot = VOT_lambdas$price_nonmot,
    UCD_data = costs_lf_mile,
    GDP = GDP,
    GDP_POP = GDP_POP,
    GDP_country = GDP_country,
    POP = POP,
    GCAM2ISO_MAPPING = GCAM2ISO_MAPPING,
    REMIND2ISO_MAPPING = REMIND2ISO_MAPPING,
    EDGE_scenario = EDGE_scenario,
    REMIND_scenario = REMIND_scenario)

  ## function that loads the TRACCS data for Europe. Final units for demand: millionkm (tkm and pkm)
  print("-- load EU TRACCS data")
  EU_data <- lvl0_loadEU(input_folder)


  ## function that merges TRACCS, Eurostat databases with other input data. Final values: EI in MJ/km (pkm and tkm), demand in million km (pkm and tkm), LF in p/v
  print("-- prepare the EU related databases")
  alldata <- lvl0_prepareEU(EU_data = EU_data,
                            iso_data = iso_data,
                            intensity = intensity_PSI_GCAM_data,
                            GDP_country = GDP_country,
                            input_folder = input_folder,
                            GCAM2ISO_MAPPING = GCAM2ISO_MAPPING,
                            REMIND2ISO_MAPPING = REMIND2ISO_MAPPING)

  target_LF = if(smartlifestyle) 1.8 else 1.7
  target_year = if(smartlifestyle) 2060 else 2080

  alldata$LF[
    subsector_L1 == "trn_pass_road_LDV_4W" &
      year >= 2020 & year <= target_year,
    loadFactor := loadFactor + (year - 2020)/(target_year - 2020) * (target_LF - loadFactor)]

  ## function that calculates the inconvenience cost starting point between 1990 and 2020
  incocost <- lvl0_incocost(annual_mileage = iso_data$UCD_results$annual_mileage,
                            load_factor = alldata$LF,
                            fcr_veh = UCD_output$fcr_veh)


  #################################################
  ## LVL 1 scripts
  #################################################
  print("-- Start of level 1 scripts")
  print("-- Harmonizing energy intensities to match IEA final energy balances")
  IEAbal_comparison <- lvl1_IEAharmonization(int = iso_data$int, demKm = alldata$demkm, IEA = IEAbal)


  print("-- Merge non-fuel prices with REMIND fuel prices")
  REMIND_prices <- merge_prices(
    gdx = file.path(input_folder, "REMIND/fulldata_EU.gdx"),
    REMINDmapping = REMIND2ISO_MAPPING,
    REMINDyears = years,
    intensity_data = IEAbal_comparison$merged_intensity,
    nonfuel_costs = iso_data$UCD_results$nec_cost[type == "normal"][, type := NULL],
    module = "edge_esm")

  REMIND_prices[, non_fuel_price := ifelse(is.na(non_fuel_price), mean(non_fuel_price, na.rm = TRUE), non_fuel_price), by = c("technology", "vehicle_type", "year")]
  REMIND_prices[, tot_price := non_fuel_price+fuel_price_pkm]



  print("-- EDGE calibration")
  calibration_output <- lvl1_calibrateEDGEinconv(
    prices = REMIND_prices,
    tech_output = alldata$demkm,
    logit_exp_data = VOT_lambdas$logit_output,
    vot_data = iso_data$vot,
    price_nonmot = iso_data$price_nonmot)



  print("-- cluster regions for share weight trends")
  clusters_overview <- lvl1_SWclustering(
    input_folder = input_folder,
    POP = POP,
    GDP = GDP,
    REMIND_scenario = REMIND_scenario,
    REMIND2ISO_MAPPING)

  density=clusters_overview[[1]]
  clusters=clusters_overview[[2]]



  print("-- generating trends for inconvenience costs")
  prefs <- lvl1_preftrend(SWS = calibration_output$list_SW,
                          clusters = clusters,
                          incocost = incocost,
                          calibdem = alldata$demkm,
                          GDP = GDP,
                          GDP_POP = GDP_POP,
                          years = years,
                          REMIND_scenario = REMIND_scenario,
                          EDGE_scenario = EDGE_scenario,
                          smartlifestyle = smartlifestyle,
                          techswitch = techswitch)


  print("-- prepare international aviation specific data")
  IntAv_Prep <- IntAvPreparation(tech_output_adj =  alldata$demkm,
                           input_folder= input_folder,
                           GDP_country = GDP_country)
  
 print("-- prepare domestic aviation specific data")
   DomAv_Prep <- DomAvPreparation(tech_output_adj =  alldata$demkm,
                                input_folder= input_folder,
                                GDP_country = GDP_country)
   
  #################################################
  ## LVL 2 scripts
  #################################################
  print("-- Start of level 2 scripts")
  ## LOGIT calculation
  print("-- LOGIT calculation")
  REMIND2ISO_MAPPING_adj = fread(system.file("extdata", "regionmappingH12.csv", package = "edgeTransport"))[, .(iso = CountryCode,region = RegionCode)]
  ## filter out prices and intensities that are related to not used vehicles-technologies in a certain region
  REMIND_prices = merge(REMIND_prices, unique(prefs$FV_final_pref[, c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)
  IEAbal_comparison$merged_intensity = merge(IEAbal_comparison$merged_intensity, unique(prefs$FV_final_pref[!(vehicle_type %in% c("Cycle_tmp_vehicletype", "Walk_tmp_vehicletype")) , c("region", "vehicle_type")]), by = c("region", "vehicle_type"), all.y = TRUE)


  totveh=NULL
  ## multiple iterations of the logit calculation - set to 3
    logit_data <- calculate_logit_inconv_endog(
      prices = REMIND_prices,
      vot_data = iso_data$vot,
      pref_data = prefs,
      logit_params = VOT_lambdas$logit_output,
      intensity_data = IEAbal_comparison$merged_intensity,
      price_nonmot = iso_data$price_nonmot,
      techswitch = techswitch)


    shares <- logit_data[["share_list"]] ## shares of alternatives for each level of the logit function
    mj_km_data <- logit_data[["mj_km_data"]] ## energy intensity at a technology level
    prices <- logit_data[["prices_list"]] ## prices at each level of the logit function, 1990USD/pkm

    ## regression demand calculation
    print("-- performing demand regression")
    dem_regr = lvl2_demandReg(tech_output = alldata$demkm,
                              price_baseline = prices$S3S,
                              GDP_POP = GDP_POP,
                              REMIND_scenario = REMIND_scenario,
                              smartlifestyle = smartlifestyle,
                              ICCT_data_I = IntAv_Prep,
                              ICCT_data_D = DomAv_Prep,
                              GDP_country = GDP_country,
                              REMIND2ISO_MAPPING_adj = REMIND2ISO_MAPPING_adj,
                              REMIND2ISO_MAPPING = REMIND2ISO_MAPPING,
                              COVID_scenario= COVID_scenario)
    
    
    if(saveRDS){
      saveRDS(dem_regr[["D_star"]], file = level2path("demand_regression.RDS"))
      saveRDS(dem_regr[["elasticities"]], file = level2path("elasticities.RDS"))
    }
    
    write_xlsx(dem_regr, "C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/demand.xlsx")
}
