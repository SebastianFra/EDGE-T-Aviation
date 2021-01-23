#' Estimate future transport demand using a price trajectory, GDP, and historical demand.
#'
#'
#' @param tech_output historically calibrated demand
#' @param price_baseline baseline prices
#' @param REMIND_scenario SSP scenario
#' @param smartlifestyle switch activating sustainable lifestyles
#'
#' @return transport demand projections
#' @author Marianna Rottoli
#'
#' @importFrom rmndt magpie2dt
#' @importFrom data.table shift frank
#' L= Leisure
#' B= Business

lvl2_demandReg <- function(tech_output, tech_output_adj, price_baseline, REMIND_scenario, smartlifestyle,COVID_scenario){
  rich <- var <- eps <- GDP_cap <- iso <- eps1 <- eps2 <- GDP_val <- POP_val <- index_GDP <- income_elasticity_freight_sm <- income_elasticity_freight_lo <- index_GDPcap <- income_elasticity_pass_sm <- income_elasticity_pass_lo <- price_elasticity_pass_lo <- sector <- index_price <- tot_price <- trn_freight <- price_elasticity_freight_sm <- trn_shipping_intl <- price_elasticity_freight_lo <- trn_pass <- price_elasticity_pass_sm <- trn_aviation_intl <- `.` <- index_price_f_sm <- index_price_f_lo <- index_GDP_f_sm <- index_GDPcap_p_lo <- index_GDP_f_lo <- index_price_p_sm <- index_GDPcap_p_sm <- index_POP <- index_price_p_lo <- D_star_f_sm <- D_star_p_sm <- D_star_p_lo <- D_star_f_lo <- D_star_f_sm <- value <- variable <- NULL
  
  ## conversion rate 2005->1990 USD
  
  CONV_2005USD_1990USD = 0.67
  
  
  ## Create a dt with GDP, POP and GDP_cap with EDGE regions
  GDP_POP = getRMNDGDPcap(scenario = REMIND_scenario)
  setnames(GDP_POP, old = "weight", new = "GDP_val")
  ## create ct with the various elasticities
  price_el = GDP_POP[,-"variable"]
  tmp = CJ(iso=unique(price_el$iso), var =c("income_elasticity_pass_sm",
                                            "price_elasticity_pass_sm",
                                            "income_elasticity_pass_lo_L",
                                            "price_elasticity_pass_lo_L",
                                            "income_elasticity_pass_lo_B",
                                            "price_elasticity_pass_lo_B",
                                            "income_elasticity_pass_lo_d_L",
                                            "price_elasticity_pass_lo_d_L",
                                            "income_elasticity_pass_lo_d_B",
                                            "price_elasticity_pass_lo_d_B",
                                            "income_elasticity_freight_sm",
                                            "price_elasticity_freight_sm",
                                            "income_elasticity_freight_lo",
                                            "price_elasticity_freight_lo"))
  ## define max and min values of the elasticities
  ## pass sm
  tmp[, rich := ifelse(var == "income_elasticity_pass_sm", 0.2, NA)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_sm", -0.01, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_sm", 1, NA)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_sm", -1.25, norm)]
  ## pass lo_L
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo_L", 1.546, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo_L", -0.05, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo_L", 1.546, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo_L", -1, norm)]
  ## pass lo_B
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo_B", 1.546, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo_B", -0.05, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo_B", 1.546, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo_B", -1, norm)]
  ## freight sm
  tmp[, rich := ifelse(var == "income_elasticity_freight_sm", 0.4, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_sm", -0.3, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_sm", 0.75, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_sm", -0.65, norm)]
  ## freight lo
  tmp[, rich := ifelse(var == "income_elasticity_freight_lo", 0.2, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_lo", -0.3, rich)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_lo", 0.4, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_lo", -0.65, norm)]
  
  if (smartlifestyle) {
    tmp[grep("income", var), rich := 0.2*rich]
  }
  
  #adjust rest of the elasticities
  price_el = merge(price_el, tmp, by = "iso", allow.cartesian = TRUE)
  price_el[, eps := ifelse(GDP_cap >= floor(GDP_cap[iso=="JPN" & year == 2020]), rich, NA)]
  price_el[, eps := ifelse(GDP_cap == min(GDP_cap), norm, eps)]
  
  ## interpolate of gdpcap values
  price_el = approx_dt(dt = price_el,
                       xdata=unique(price_el$GDP_cap),
                       xcol = "GDP_cap",
                       ycol="eps",
                       idxcols="var",
                       extrapolate = TRUE)
  iso_mapping <-read_csv("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/regionmappingR10.csv")
  price_el<-merge( price_el, iso_mapping[,c(2,3)], by = c("iso"), all.x=TRUE)
  
  #adjust only the international aviation
  price_el_int_aviation_L <- price_el[var == "income_elasticity_pass_lo_L"]
  price_el_int_aviation_B <- price_el[var == "income_elasticity_pass_lo_B"]
  
  
  #define tresholds and decay rate
  #couple decay rate to RPK_cap of baseline runs
  RPK_cap_baseline_L = fread(file.path("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/RPK_CAP_baseline_leisure_master3.csv"), sep=",", header = T)
  RPK_cap_baseline_B = fread(file.path("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/RPK_CAP_baseline_business_master3.csv"), sep=",", header = T)
  RPK_cap_baseline_L <- RPK_cap_baseline_L[scenario == REMIND_scenario]
  RPK_cap_baseline_B <- RPK_cap_baseline_B[scenario == REMIND_scenario]
  price_el_int_aviation_L_RPK = merge( price_el_int_aviation_L, RPK_cap_baseline_L[,c(1,2,5)], by = c("iso","year"),all.x = TRUE)
  price_el_int_aviation_B_RPK = merge( price_el_int_aviation_B, RPK_cap_baseline_B[,c(1,2,5)], by = c("iso","year"),all.x = TRUE)
  #subset data only for available RPK Data
  #leisure
  price_el_int_aviation_L_RPK <- price_el_int_aviation_L_RPK[,decay_rate:= 1]
  price_el_int_aviation_L_RPK <- subset(price_el_int_aviation_L_RPK, price_el_int_aviation_L_RPK$year <=2060 & price_el_int_aviation_L_RPK$year >=2020)
  price_el_int_aviation_B_RPK <- price_el_int_aviation_B_RPK[,decay_rate:= 1]
  price_el_int_aviation_B_RPK <- subset(price_el_int_aviation_B_RPK, price_el_int_aviation_B_RPK$year <=2060 & price_el_int_aviation_B_RPK$year >=2020)
  #International
  #mutate the decayrate for Income elasticity Leisure
  if (REMIND_scenario == "SSP1") {
    decay_DR_L=0.8
    decay_treshold_L= 1300
  }else if (REMIND_scenario == "SSP2") {
    decay_DR_L=0.90
    decay_treshold_L= 1500
  }else if (REMIND_scenario == "SSP3") {
    decay_DR_L=0.90
    decay_treshold_L= 1500
  }else if (REMIND_scenario == "SSP4") {
    decay_DR_L=0.90
    decay_treshold_L= 1500
  }else if (REMIND_scenario == "SSP5") {
    decay_DR_L=0.99
    decay_treshold_L= 1600
  }else{}
  #mutate the decayrate for Income elasticity Business
  if (REMIND_scenario == "SSP1") {
    decay_DR_B=0.7
    decay_treshold_B= 700
  }else if (REMIND_scenario == "SSP2") {
    decay_DR_B=0.8
    decay_treshold_B= 1000
  }else if (REMIND_scenario == "SSP3") {
    decay_DR_B=0.8
    decay_treshold_B= 1000
  }else if (REMIND_scenario == "SSP4") {
    decay_DR_B=0.8
    decay_treshold_B= 1000
  }else if (REMIND_scenario == "SSP5") {
    decay_DR_B=0.90
    decay_treshold_B= 1200
  }else{}
  #Loops that adjust the decay rate of  aviation
  
  #int aviation
  #Leisure Loop
  for (j in unique(price_el_int_aviation_L_RPK$iso)) {
    for (i in unique(price_el_int_aviation_L_RPK$year[price_el_int_aviation_L_RPK$iso == j])) { 
      if (price_el_int_aviation_L_RPK$RPK_CAP_L[price_el_int_aviation_L_RPK$iso == j & price_el_int_aviation_L_RPK$year == i] > decay_treshold_L) { 
        price_el_int_aviation_L_RPK$decay_rate[price_el_int_aviation_L_RPK$iso == j & price_el_int_aviation_L_RPK$year >= i] <- price_el_int_aviation_L_RPK$decay_rate[price_el_int_aviation_L_RPK$iso == j & price_el_int_aviation_L_RPK$year >= i] * decay_DR_L
      }
    }
  }
  #Business Loop
  for (j in unique(price_el_int_aviation_B_RPK$iso)) {
    for (i in unique(price_el_int_aviation_B_RPK$year[price_el_int_aviation_B_RPK$iso == j])) { 
      if (price_el_int_aviation_B_RPK$RPK_CAP_B[price_el_int_aviation_B_RPK$iso == j & price_el_int_aviation_B_RPK$year == i] > decay_treshold_B) { 
        price_el_int_aviation_B_RPK$decay_rate[price_el_int_aviation_B_RPK$iso == j & price_el_int_aviation_B_RPK$year >= i] <- price_el_int_aviation_B_RPK$decay_rate[price_el_int_aviation_B_RPK$iso == j & price_el_int_aviation_B_RPK$year >= i] * decay_DR_B
      }
    }
  }
  #merge with RPK
  #international
  price_el_int_aviation_L_RPK<- price_el_int_aviation_L_RPK[, c(3:11):= NULL] 
  price_el_int_aviation_L = merge(price_el_int_aviation_L, price_el_int_aviation_L_RPK, by = c("iso","year"),all.x = TRUE)
  price_el_int_aviation_B_RPK<- price_el_int_aviation_B_RPK[, c(3:11):= NULL] 
  price_el_int_aviation_B = merge(price_el_int_aviation_B, price_el_int_aviation_B_RPK, by = c("iso","year"),all.x = TRUE)
  #replace NAs for Leisure and Business
  #International
  #Leisure
  for (j in unique(price_el_int_aviation_L$iso)) {
    for (i in unique(price_el_int_aviation_L$year[price_el_int_aviation_L$iso == j])) { 
      if (price_el_int_aviation_L$year[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year == i] < 2020){
        price_el_int_aviation_L$decay_rate[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year == i]<- 1
      }
    }
  }
  price_el_int_aviation_L <- price_el_int_aviation_L[,decay_rate:=na.locf(decay_rate, by=iso)]
  
  for (j in unique(price_el_int_aviation_L$iso)) {
    for (i in unique(price_el_int_aviation_L$year[price_el_int_aviation_L$iso == j])) { 
      if (price_el_int_aviation_L$year[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year == i] > 2060) { 
        price_el_int_aviation_L$decay_rate[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year >= i] <- price_el_int_aviation_L$decay_rate[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year >= i] * decay_DR_L
      }
    }
  }
  #Business
  for (j in unique(price_el_int_aviation_B$iso)) {
    for (i in unique(price_el_int_aviation_B$year[price_el_int_aviation_B$iso == j])) { 
      if (price_el_int_aviation_B$year[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year == i] < 2020){
        price_el_int_aviation_B$decay_rate[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year == i]<- 1
      }
    }
  }
  price_el_int_aviation_B <- price_el_int_aviation_B[,decay_rate:=na.locf(decay_rate, by=iso)]
  
  for (j in unique(price_el_int_aviation_B$iso)) {
    for (i in unique(price_el_int_aviation_B$year[price_el_int_aviation_B$iso == j])) { 
      if (price_el_int_aviation_B$year[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year == i] > 2060) { 
        price_el_int_aviation_B$decay_rate[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year >= i] <- price_el_int_aviation_B$decay_rate[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year >= i] * decay_DR_B
      }
    }
  }
  #Decay of Income Elasticity
  #International
  #Leisure
  if (REMIND_scenario == "SSP1") {
    GDP_treshold_L= 45000
  }else if (REMIND_scenario == "SSP2") {
    GDP_treshold_L= 50000
  }else if (REMIND_scenario == "SSP3") {
    GDP_treshold_L= 50000
  }else if (REMIND_scenario == "SSP4") {
    GDP_treshold_L= 50000
  }else if (REMIND_scenario == "SSP5") {
    GDP_treshold_L= 55000
  }else{}
  
  #Business
  if (REMIND_scenario == "SSP1") {
    GDP_treshold_B= 40000
  }else if (REMIND_scenario == "SSP2") {
    GDP_treshold_B= 45000
  }else if (REMIND_scenario == "SSP3") {
    GDP_treshold_B= 45000
  }else if (REMIND_scenario == "SSP4") {
    GDP_treshold_B= 45000
  }else if (REMIND_scenario == "SSP5") {
    GDP_treshold_B= 50000
  }else{}
  #International
  #Leisure Loop
  for (j in unique(price_el_int_aviation_L$iso)) {
    for (i in unique(price_el_int_aviation_L$year[price_el_int_aviation_L$iso == j])) { 
      if (price_el_int_aviation_L$GDP_cap[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year == i] > GDP_treshold_L) { 
        price_el_int_aviation_L$eps[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year >= i] <- price_el_int_aviation_L$eps[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year >= i] * price_el_int_aviation_L$decay_rate[price_el_int_aviation_L$iso == j & price_el_int_aviation_L$year == i]
      }
    }
  }
  #Business Loop
  for (j in unique(price_el_int_aviation_B$iso)) {
    for (i in unique(price_el_int_aviation_B$year[price_el_int_aviation_B$iso == j])) { 
      if (price_el_int_aviation_B$GDP_cap[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year == i] > GDP_treshold_B) { 
        price_el_int_aviation_B$eps[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year >= i] <- price_el_int_aviation_B$eps[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year >= i] * price_el_int_aviation_B$decay_rate[price_el_int_aviation_B$iso == j & price_el_int_aviation_B$year == i]
      }
    }
  }
  
  #adjust rest asia and africa otherwise it would be to high
  #international
  price_el_int_aviation_L[, eps := ifelse( iso_region %in% c("R10REST_ASIA", "R10AFRICA"), eps *0.75, eps )]
  price_el_int_aviation_B[, eps := ifelse( iso_region %in% c("R10REST_ASIA"), eps *0.5, eps)]
  
  #drop iso_region
  price_el_int_aviation_L[,c("iso_region", "RPK_CAP_L"):=NULL]
  price_el_int_aviation_B[,c("iso_region","RPK_CAP_B"):=NULL]
  #export income elasticity trajectories
  write_xlsx(price_el_int_aviation_L, "C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/IE_Trajectories_Leisure.xlsx")
  write_xlsx(price_el_int_aviation_B, "C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/IE_Trajectories_Business.xlsx")
  #get correct format for merge
  price_el_int_aviation_L = dcast(price_el_int_aviation_L[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps")  
  price_el_int_aviation_B = dcast(price_el_int_aviation_B[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps") 
  #adjust other elasticites
  ## correct price elasticity for IND, otherwise the demand is misleadingly low
  ## https://ideas.repec.org/p/ekd/006356/7355.html
  price_el[, eps := ifelse(var == "price_elasticity_pass_sm" & iso == "IND", -0.35, eps)]
  ## CHN demand grows too quickly in the first time step: smoothen down the elasticity increase
  price_el[iso == "CHN" & var == "price_elasticity_pass_sm" & year == 2015, eps := 0.8*eps]
  ## correct elasticity for SSA, otherwise the demand does not grow enough as compared to other regions
  price_el[, eps := ifelse(var == "income_elasticity_pass_sm" & iso %in% c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "DJI", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "MYT", "NAM", "NER", "NGA", "REU", "RWA", "SEN", "SHN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE"), eps*1.1, eps)]
  price_el = dcast(price_el[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps")
  
  #merge again
  price_el = merge(price_el, price_el_int_aviation_L, by = c("iso","year"),all.x = TRUE)
  price_el = merge(price_el, price_el_int_aviation_B, by = c("iso","year"),all.x = TRUE)
  price_el[, c("income_elasticity_pass_lo_L.x","income_elasticity_pass_lo_B.x","GDP_cap.y","GDP_cap"):= NULL]
  #drop stuff
  price_el[, c("income_elasticity_pass_lo_L.x","income_elasticity_pass_lo_B.x","GDP_cap.y","GDP_cap"):= NULL]
  setnames(price_el, "income_elasticity_pass_lo_L.y", "income_elasticity_pass_lo_L")
  setnames(price_el, "income_elasticity_pass_lo_B.y", "income_elasticity_pass_lo_B")
  setnames(price_el, "GDP_cap.x", "GDP_cap")
  
  
  #calculate growth rates
  GDP_POP[,`:=`(index_GDP=GDP_val/shift(GDP_val), index_GDPcap=GDP_cap/shift(GDP_cap), index_POP=POP_val/shift(POP_val)), by=c("iso")]
  ## merge GDP_POP and price elasticity
  GDP_POP = merge(GDP_POP, price_el[,c("iso", "year" ,"income_elasticity_pass_lo_L","income_elasticity_pass_lo_B", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("iso", "year"))
  
  #calculate the indexes raised to the corresponding elasticities
  GDP_POP[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo_L=index_GDPcap^income_elasticity_pass_lo_L,
                index_GDPcap_p_lo_B=index_GDPcap^income_elasticity_pass_lo_B)]
  GDP_POP[,c("income_elasticity_freight_sm", "income_elasticity_freight_lo", "income_elasticity_pass_sm", "income_elasticity_pass_lo_L","income_elasticity_pass_lo_B") := NULL]
  
  #order the prices according to the year, within the sector
  price_baseline=price_baseline[order(-frank(sector), year)]
  #calculate "index" which represent the growth of total price
  price_baseline[,index_price:=tot_price/shift(tot_price),by=c("iso","sector")]
  #select only the needed columns
  price_baseline=price_baseline[, c("iso","year","sector","index_price")]
  #from long to wide format, so that the df has separate columns for all transport modes
  price_baseline=dcast(price_baseline, iso + year  ~ sector, value.var = "index_price", fun.aggregate = sum, margins="sector")
  ## merge with elasticities
  price_baseline = merge(price_baseline, price_el[,c("iso", "year", "price_elasticity_pass_lo_L","price_elasticity_pass_lo_B", "price_elasticity_pass_sm", "price_elasticity_freight_sm", "price_elasticity_freight_lo")], by = c("iso", "year"))
  price_baseline <- subset(price_baseline, price_baseline$year >=2005)
  write_xlsx(price_baseline, "C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/test.xlsx")
  #calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_f_sm=trn_freight^price_elasticity_freight_sm,
                       index_price_f_lo=trn_shipping_intl^price_elasticity_freight_lo,
                       index_price_p_sm=trn_pass^price_elasticity_pass_sm,
                       index_price_p_lo_L=trn_aviation_intl^price_elasticity_pass_lo_L,
                       index_price_p_lo_B=trn_aviation_intl^price_elasticity_pass_lo_B)]
  
  
  price_baseline[,c("price_elasticity_freight_sm", "price_elasticity_freight_lo", "price_elasticity_pass_sm", "price_elasticity_pass_lo_L", "price_elasticity_pass_lo_B") := NULL]
  
  #create the D* df
  D_star=merge(price_baseline,GDP_POP,by = c("iso","year"))
  
  #calculate D* for each mode separately, and select only the useful cols
  D_star_lvl1=D_star[,.(D_star_f_sm=index_price_f_sm*index_GDP_f_sm,
                        D_star_f_lo=index_price_f_lo*index_GDP_f_lo,
                        D_star_p_sm=index_price_p_sm*index_GDPcap_p_sm*index_POP,
                        D_star_p_lo_L=index_price_p_lo_L*index_GDPcap_p_lo_L*index_POP,
                        D_star_p_lo_B=index_price_p_lo_B*index_GDPcap_p_lo_B*index_POP,
                        D_star_p_lo_d_L=index_price_p_lo_L*index_GDPcap_p_lo_L*index_POP,
                        D_star_p_lo_d_B=index_price_p_lo_B*index_GDPcap_p_lo_B*index_POP,
                        iso,
                        year)]
  #calculate demand at a sector level
  demand_tot_sector=tech_output[, .(demand_tot=sum(tech_output)), by=c("iso", "year", "sector")]
  #from long to wide format, so that the df has separate columns for all transport modes
  demand_tot_sector=dcast(demand_tot_sector, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  
  #merge D* and historical demand
  D_star=merge(D_star_lvl1,demand_tot_sector, by = c("iso","year"),all.x = TRUE)
  
  #for loop that calculates the value of the following time step of demand based on the growth of the indexes
  i=NULL
  for (i in seq(1,length(unique(D_star$year)),1)) {
    D_star[year>=2010,tmp:=shift(trn_freight)*D_star_f_sm,by=c("iso")]
    D_star[is.na(trn_freight) & !is.na(tmp),trn_freight:=tmp]
    D_star[year>=2010,tmp:=shift(trn_pass)*D_star_p_sm,by=c("iso")]
    D_star[is.na(trn_pass) & !is.na(tmp),trn_pass:=tmp]
    D_star[year>=2010,tmp:=shift(trn_shipping_intl)*D_star_f_lo,by=c("iso")]
    D_star[is.na(trn_shipping_intl) & !is.na(tmp),trn_shipping_intl:=tmp]
    
    
    i=i+1
  }
  
  #drop int aviation
  D_star_else <- D_star[,c(8):= NULL]
  #adjust for domestic and international
  tech_output_adj[, tech_output_adj := ifelse(year %in% c(2000)& sector == "trn_aviation_intl" ,tech_output_adj * 1.9, tech_output_adj)]
  tech_output_adj[, tech_output_adj := ifelse(year %in% c(2005)& sector == "trn_aviation_intl" ,tech_output_adj * 1.85, tech_output_adj) ]
  tech_output_adj[, tech_output_adj := ifelse(year %in% c(2010)& sector == "trn_aviation_intl" ,tech_output_adj * 1.85, tech_output_adj) ]
  tech_output_adj[, tech_output_adj := ifelse(year %in% c(2015)& sector == "trn_aviation_intl" ,tech_output_adj * 1.75, tech_output_adj) ]
  tech_output_adj[, tech_output_adj := ifelse(year %in% c(2020)& sector == "trn_aviation_intl" ,tech_output_adj * 1.75, tech_output_adj) ]
  #int aviation loop with forward pushing the historic data base
  demand_tot_sector_avi_L=tech_output_adj[, .(demand_tot=sum(tech_output_adj)), by=c("iso", "year", "sector")]
  demand_tot_sector_avi_B=tech_output_adj[, .(demand_tot=sum(tech_output_adj)), by=c("iso", "year", "sector")]
  demand_tot_sector_avi_d_L=tech_output_adj[, .(demand_tot=sum(tech_output_adj)), by=c("iso", "year", "sector")]
  demand_tot_sector_avi_d_B=tech_output_adj[, .(demand_tot=sum(tech_output_adj)), by=c("iso", "year", "sector")]
  #Differentiate between different Trip Purposes in techoutput file and differentiate between domestic and leisure
  #in terms trip-purposes of leisure and business we assume one split from the us survey as approximation for all countries
  #domestic: 42% business , 58% leisure///international: 37.5% business 62.5% leisure
  #inline with ICAO January page 17
  #add mapping
  iso_mapping <-read_csv("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/regionmappingR10.csv")
  demand_tot_sector_avi_L<-merge(   demand_tot_sector_avi_L, iso_mapping[,c(2,3)], by = c("iso"), all.x=TRUE)
  demand_tot_sector_avi_d_L<-merge(   demand_tot_sector_avi_d_L, iso_mapping[,c(2,3)], by = c("iso"), all.x=TRUE)
  demand_tot_sector_avi_B<-merge(   demand_tot_sector_avi_B, iso_mapping[,c(2,3)], by = c("iso"), all.x=TRUE)
  demand_tot_sector_avi_d_B<-merge(   demand_tot_sector_avi_d_B, iso_mapping[,c(2,3)], by = c("iso"), all.x=TRUE)
 #Split Leisure and Business
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"  ,demand_tot * 0.625, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" ,demand_tot * 0.58, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl" ,demand_tot * 0.375, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl",demand_tot * 0.42, demand_tot) ]
  
  #Split domestic and international regional based
  #Africa
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10AFRICA" ,demand_tot * 0.65, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10AFRICA" ,demand_tot * 0.35, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10AFRICA" ,demand_tot * 0.65, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10AFRICA" ,demand_tot * 0.35, demand_tot) ]
  #Asia-Pacific
  #China
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10CHINA+" ,demand_tot * 0.30, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10CHINA+" ,demand_tot * 0.70, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10CHINA+" ,demand_tot * 0.30, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10CHINA+" ,demand_tot * 0.70, demand_tot) ]
  #India
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10INDIA+" ,demand_tot * 0.3, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10INDIA+" ,demand_tot * 0.7, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10INDIA+" ,demand_tot * 0.3, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10INDIA+" ,demand_tot * 0.7, demand_tot) ]
  #rest asia
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10REST_ASIA" ,demand_tot * 0.3, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10REST_ASIA" ,demand_tot *  0.7, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10REST_ASIA" ,demand_tot * 0.3, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10REST_ASIA" ,demand_tot * 0.7, demand_tot) ]
  #Europe
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10EUROPE" ,demand_tot * 0.74, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10EUROPE" ,demand_tot * 0.26, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10EUROPE" ,demand_tot * 0.74, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10EUROPE" ,demand_tot * 0.26, demand_tot) ]
  #ref_econ(Armenia,Belarus etc. ---> World share)
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10REF_ECON" ,demand_tot *  0.58, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10REF_ECON" ,demand_tot * 0.42, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10REF_ECON" ,demand_tot * 0.58, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10REF_ECON" ,demand_tot *  0.42, demand_tot) ]
  #Latin
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10LATIN_AM" ,demand_tot * 0.35, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10LATIN_AM" ,demand_tot * 0.65, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10LATIN_AM" ,demand_tot * 0.35, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10LATIN_AM" ,demand_tot * 0.65, demand_tot) ]
  #Middle East
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10MIDDLE_EAST" ,demand_tot * 0.79, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10MIDDLE_EAST" ,demand_tot * 0.21, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10MIDDLE_EAST" ,demand_tot * 0.79, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10MIDDLE_EAST" ,demand_tot * 0.21, demand_tot) ]
  #North-America
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10NORTH_AM" ,demand_tot * 0.16, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10NORTH_AM" ,demand_tot * 0.84, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10NORTH_AM" ,demand_tot * 0.16, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10NORTH_AM" ,demand_tot * 0.84, demand_tot) ]
  #pacific oecd (Canada, Japan,australia --> Use World share)
  demand_tot_sector_avi_L[,   demand_tot := ifelse(sector == "trn_aviation_intl" & iso_region == "R10PAC_OECD" ,demand_tot * 0.42, demand_tot) ]
  demand_tot_sector_avi_d_L[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10PAC_OECD" ,demand_tot * 0.58, demand_tot) ]
  demand_tot_sector_avi_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10PAC_OECD" ,demand_tot * 0.42, demand_tot) ]
  demand_tot_sector_avi_d_B[,   demand_tot := ifelse(sector == "trn_aviation_intl"& iso_region == "R10PAC_OECD" ,demand_tot * 0.58, demand_tot) ]
  #drop region
  demand_tot_sector_avi_L[,c("iso_region"):=NULL]
  demand_tot_sector_avi_d_L[,c("iso_region"):=NULL]
  demand_tot_sector_avi_B[,c("iso_region"):=NULL]
  demand_tot_sector_avi_d_B[,c("iso_region"):=NULL]
  #from long to wide format, so that the df has separate columns for all transport modes
  demand_tot_sector_avi_L=dcast(demand_tot_sector_avi_L, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  demand_tot_sector_avi_B=dcast(demand_tot_sector_avi_B, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  demand_tot_sector_avi_d_L=dcast(demand_tot_sector_avi_d_L, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  demand_tot_sector_avi_d_B=dcast(demand_tot_sector_avi_d_B, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  #merge D* and historical demand
  D_star_avi_L=merge(D_star_lvl1,demand_tot_sector_avi_L, by = c("iso","year"),all.x = TRUE)
  D_star_avi_B=merge(D_star_lvl1,demand_tot_sector_avi_B, by = c("iso","year"),all.x = TRUE)
  D_star_avi_d_L=merge(D_star_lvl1,demand_tot_sector_avi_d_L, by = c("iso","year"),all.x = TRUE)
  D_star_avi_d_B=merge(D_star_lvl1,demand_tot_sector_avi_d_B, by = c("iso","year"),all.x = TRUE)
  
  #Loop to calculate future demand
  #international
  i=NULL
  for (i in seq(1,length(unique(D_star_avi_L$year)),1)) {
    D_star_avi_L[year>=2020,tmp:=shift(trn_aviation_intl)*D_star_p_lo_L,by=c("iso")]
    D_star_avi_L[is.na(trn_aviation_intl) & !is.na(tmp),trn_aviation_intl:=tmp]
    i=i+1
  }
  for (i in seq(1,length(unique(D_star_avi_B$year)),1)) {
    D_star_avi_B[year>=2020,tmp:=shift(trn_aviation_intl)*D_star_p_lo_B,by=c("iso")]
    D_star_avi_B[is.na(trn_aviation_intl) & !is.na(tmp),trn_aviation_intl:=tmp]
    i=i+1
  }
  #domestic
  i=NULL
  for (i in seq(1,length(unique(D_star_avi_d_L$year)),1)) {
    D_star_avi_d_L[year>=2020,tmp:=shift(trn_aviation_intl)*D_star_p_lo_d_L,by=c("iso")]
    D_star_avi_d_L[is.na(trn_aviation_intl) & !is.na(tmp),trn_aviation_intl:=tmp]
    i=i+1
  }
  for (i in seq(1,length(unique(D_star_avi_d_B$year)),1)) {
    D_star_avi_d_B[year>=2020,tmp:=shift(trn_aviation_intl)*D_star_p_lo_d_B,by=c("iso")]
    D_star_avi_d_B[is.na(trn_aviation_intl) & !is.na(tmp),trn_aviation_intl:=tmp]
    i=i+1
  }
  #merge files
  setnames(D_star_avi_L, "trn_aviation_intl", "trn_aviation_intl_L")
  setnames(D_star_avi_B, "trn_aviation_intl", "trn_aviation_intl_B")
  setnames(D_star_avi_d_L, "trn_aviation_intl", "trn_aviation_intl_d_L")
  setnames(D_star_avi_d_B, "trn_aviation_intl", "trn_aviation_intl_d_B")
  D_star_avi=merge(D_star_avi_L, D_star_avi_B,by = c("iso","year"),all.x = TRUE)
  D_star_avi_d=merge(D_star_avi_d_L, D_star_avi_d_B,by = c("iso","year"),all.x = TRUE)
  D_star_avi=merge(D_star_avi_d, D_star_avi,by = c("iso","year"),all.x = TRUE)
  #merge two files
  D_star_final=merge(D_star_else, D_star_avi,by = c("iso","year"),all.x = TRUE)
  D_star_final <- D_star_final[, c(9,13:21,23:34,36:47):= NULL]
  D_star_final <- D_star_final[, c(15:26,28:31):= NULL]
  ## select only the columns that contains the demand
  D_star_final[, c(3:8) := NULL]
  #linear interpolation of 5 year data to yearly data
  #create sequence of dates
  ## long format
  D_star_final=melt(D_star_final,id.vars = c("iso","year"))
  ## interpolate
  D_star_final=approx_dt(dt = D_star_final, ## database to interpolate
                         xdata = seq(2005,2100), ## time steps on which to interpolate
                         ycol = "value", ## column containing the data to interpolate
                         xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                         idxcols=c("iso","variable"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                         extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  
  ## back to previous format
  D_star_final=dcast(D_star_final,iso+year~variable,value.var="value")
  #manually lower domestic demand 
  #domestic leisure
  for (j in unique(D_star_final$iso)) {
    for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
      if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2020) { 
        D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year >= i] <- D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year >= i] * 0.98
      }else{}
    }
  }
  #domestic business
  for (j in unique(D_star_final$iso)) {
    for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
      if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2020) { 
        D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year >= i] <- D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year >= i] * 0.97
      }else{}
    }
  }
  #COVID-ADJUSTMENT: Exogenous Shock
  COVID_shock = fread(file.path("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/COVID_shock.csv"), sep=";", header = T)
  D_star_final=merge(D_star_final, COVID_shock,by = c("iso","year"),all.x = TRUE)
  
  #International Leisure
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_L_1[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_L_2[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_L_3[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_L_4[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_L_5[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else{}
  #International Business
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_B_1[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_B_2[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_B_3[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_B_4[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$I_B_5[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else{}
  #Domestic Leisure
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_L_1[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_L_2[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_L_3[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_L_4[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_L[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_L[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_L_5[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else{}
  #Domestic Business
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_B_1[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_B_2[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_B_3[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_B_4[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star_final$iso)) {
        for (i in unique(D_star_final$year[D_star_final$iso == j])) { 
          if (D_star_final$year[D_star_final$iso == j & D_star_final$year == i] > 2019) { 
            D_star_final$trn_aviation_intl_d_B[D_star_final$iso == j & D_star_final$year == i] <- D_star_final$trn_aviation_intl_d_B[ D_star_final$iso == j & D_star_final$year == i] * D_star_final$D_B_5[ D_star_final$iso == j & D_star_final$year == i]
          }
        }
      }
    }else{}
  }else{}
  #Calculate total int. demand also
  D_star_final[,trn_aviation_intl:= trn_aviation_intl_L + trn_aviation_intl_B+ trn_aviation_intl_d_L + trn_aviation_intl_d_B, by = c("iso", "year")]
  D_star_final = melt(D_star_final, id.vars = c("iso", "year"),
                      measure.vars = c("trn_aviation_intl", "trn_freight", "trn_pass", "trn_shipping_intl", "trn_aviation_intl_L","trn_aviation_intl_B","trn_aviation_intl_d_L","trn_aviation_intl_d_B"))
  D_star_final = D_star_final[,.(iso, year, demand = value, sector = variable)]
  return(D_star_final)
  
}
