#' Estimate future transport demand using a price trajectory, GDP, and historical demand.
#'
#'
#' @param tech_output historically calibrated demand
#' @param price_baseline baseline prices
#' @param GDP_POP GDP per capita
#' @param ICCT_data Historical aviation demand
#' @param REMIND_scenario SSP scenario
#' @param smartlifestyle switch activating sustainable lifestyles
#' @importFrom rmndt approx_dt
#' @return transport demand projections
#' @author Marianna Rottoli, Sebastian Franz
#'
#' @importFrom data.table shift frank
#' @importFrom stats na.omit



lvl2_demandReg <- function(tech_output, price_baseline, GDP_POP, ICCT_data, RPK_cap_baseline_L, RPK_cap_baseline_B, input_folder, COVID_dir="COVID", REMIND_scenario, smartlifestyle, Baseline_Run){

  rich <- var <- eps <- GDP_cap <- region <- eps1 <- eps2 <- GDP_val <- POP_val <- NULL
  index_GDP <- income_elasticity_freight_sm <- income_elasticity_freight_lo <- index_GDPcap <- NULL
  income_elasticity_pass_sm <- income_elasticity_pass_lo <- price_elasticity_pass_lo <- sector <- NULL
  index_price <- tot_price <- trn_freight <- price_elasticity_freight_sm <- trn_shipping_intl <- NULL
  price_elasticity_freight_lo <- trn_pass <- price_elasticity_pass_sm <- trn_aviation_intl <- `.` <- NULL
  index_price_f_sm <- index_price_f_lo <- index_GDP_f_sm <- index_GDPcap_p_lo <- index_GDP_f_lo <- NULL
  index_price_p_sm <- index_GDPcap_p_sm <- index_POP <- index_price_p_lo <- D_star_f_sm <- D_star_p_sm <- NULL
  D_star_p_lo <- D_star_f_lo <- D_star_f_sm <- value <- variable <- vrich <- vpoor <-NULL
  income_elasticity_pass_lo_L <- income_elasticity_pass_lo_B <- price_elasticity_pass_lo_L <-price_elasticity_pass_lo_B <- NULL
  index_price_p_lo_L <- index_price_p_lo_B <- index_GDPcap_p_lo_L <- index_GDPcap_p_lo_B <- NULL
  trn_aviation_intl_L <- trn_aviation_intl_B <- D_star_p_lo_L <- D_star_p_lo_B <- value <- NULL

  ## conversion rate 2005->1990 USD
  CONV_2005USD_1990USD = 0.67

  ## PARAMETERS FOR ELASTICITY. These Parameters can be adjusted in order to reflect the specific SSP narrative correctly
  ## RPK Treshold & Decayand GDP Treshold, for business and leisure
  if (REMIND_scenario == "SSP1") {
    decay_DR_L=0.7
    decay_treshold_L= 1300
    decay_DR_B=0.65
    decay_treshold_B= 800
    GDP_treshold_L= 50000
    GDP_treshold_B= 45000
  }else if (REMIND_scenario == "SSP2") {
    decay_DR_L=0.85
    decay_treshold_L= 1350
    decay_DR_B=0.7
    decay_treshold_B= 850
    GDP_treshold_L= 55000
    GDP_treshold_B= 50000
  }else if (REMIND_scenario == "SSP3") {
    decay_DR_L=0.85
    decay_treshold_L= 1350
    decay_DR_B=0.7
    decay_treshold_B= 850
    GDP_treshold_L= 55000
    GDP_treshold_B= 50000
  }else if (REMIND_scenario == "SSP4") {
    decay_DR_L=0.85
    decay_treshold_L= 1350
    decay_DR_B=0.7
    decay_treshold_B= 850
    GDP_treshold_L= 55000
    GDP_treshold_B= 50000
  }else if (REMIND_scenario == "SSP5") {
    decay_DR_L=0.95
    decay_treshold_L= 1600
    decay_DR_B=0.85
    decay_treshold_B= 1200
    GDP_treshold_L= 60000
    GDP_treshold_B= 55000
  }

  ## Create a dt with GDP, POP and GDP_cap with EDGE regions
  gdp_pop = copy(GDP_POP)
  setnames(gdp_pop, old = "weight", new = "GDP_val")
  ## create ct with the various elasticities
  price_el = gdp_pop[,-"variable"]
  tmp = CJ(region=unique(price_el$region), var =c("income_elasticity_pass_sm",
                                                  "price_elasticity_pass_sm",
                                                  "income_elasticity_pass_lo_L",
                                                  "price_elasticity_pass_lo_L",
                                                  "income_elasticity_pass_lo_B",
                                                  "price_elasticity_pass_lo_B",
                                                  "income_elasticity_freight_sm",
                                                  "price_elasticity_freight_sm",
                                                  "income_elasticity_freight_lo",
                                                  "price_elasticity_freight_lo"))
  ## define max and min values of the elasticities
  ## pass sm
  tmp[, vrich := ifelse(var == "income_elasticity_pass_sm", 0.25, NA)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_sm", -0.65, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_sm", 0.5, NA)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_sm", -1.25, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_sm", 0.8, NA)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_sm", -1, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_sm", 1, NA)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_sm", -0.625, norm)]
  ## pass lo (see The income elasticity of air travel a meta analysis, Gallet et al 2014
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo_L", -1, norm)]
  tmp[, vrich := ifelse(var == "income_elasticity_pass_lo_L", 1.5, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_lo_L", -0.25, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo_L", 1.5, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo_L", -0.5, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_lo_L", 5.5, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_lo_L", -0.7, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo_L", 1.5, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo_L", -1, norm)]
  tmp[, vrich := ifelse(var == "income_elasticity_pass_lo_B", 1.5, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_lo_B", -0.25, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo_B", 1.5, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo_B", -0.5, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_lo_B", 1.5, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_lo_B", -0.7, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo_B", 1.5, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo_B", -1, norm)]

  ## freight sm
  tmp[, vrich := ifelse(var == "income_elasticity_freight_sm", 0.1875, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_freight_sm", -0.1875, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_freight_sm", 0.375, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_sm", -0.325, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_freight_sm", 0.6, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_freight_sm", -0.4, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_sm", 0.75, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_sm", -0.65, norm)]
  ## freight lo
  tmp[, vrich := ifelse(var == "income_elasticity_freight_lo", 0.1, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_freight_lo", -0.1625, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_freight_lo", 0.2, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_freight_lo", -0.325, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_freight_lo", 0.3, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_freight_lo", -0.5, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_freight_lo", 0.4, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_freight_lo", -0.65, norm)]

  price_el = merge(price_el, tmp, by = "region", allow.cartesian = TRUE)
  price_el[, eps := ifelse(GDP_cap < 15000, vpoor, NA)]
  price_el[, eps := ifelse(GDP_cap >= 25000, rich, eps)]
  price_el[, eps := ifelse(GDP_cap > 25000 & GDP_cap < 30000, vrich, eps)]
  price_el[, eps := ifelse(GDP_cap < 25000 & GDP_cap >= 15000, norm, eps)]

  ## interpolate of gdpcap values
  price_el = approx_dt(dt = price_el,
                       xdata=unique(price_el$GDP_cap),
                       xcol = "GDP_cap",
                       ycol="eps",
                       idxcols="var",
                       extrapolate = TRUE)

  price_el[region %in% c("REF", "CHA", "IND") & var %in% c("income_elasticity_pass_sm", "income_elasticity_freight_sm"), eps := 0]
  price_el[region %in% c("OAS", "MEA", "LAM") & var %in% c("income_elasticity_pass_sm"), eps := 0.1]
  price_el[region %in% c("IND") & var %in% c("income_elasticity_pass_sm"), eps := 0.5]
  price_el[region %in% c("SSA", "CHA") & var %in% c("income_elasticity_pass_sm"), eps := 0.3]
  price_el[region %in% c("EUR", "NEU", "USA", "CAZ", "JPN", "DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI", "NEN", "NES") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.1]

  if (smartlifestyle) {
    price_el[region =="REF" & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0]
    price_el[region %in% c("OAS", "MEA", "LAM") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.1]
    price_el[region %in% c("IND") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.4]
    price_el[region %in% c("CHA") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.2]
    price_el[region %in% c("SSA") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0.5]
    price_el[region %in% c("EUR", "NEU", "USA", "CAZ", "JPN", "DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI", "NEN", "NES") & var %in% c("income_elasticity_pass_lo", "income_elasticity_pass_sm"), eps := 0]
  }

  price_el[region %in% c("IND", "OAS", "SSA", "MEA") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps := 0.25]
  price_el_int_aviation_L <- price_el[var == "income_elasticity_pass_lo_L"]
  price_el_int_aviation_B <- price_el[var == "income_elasticity_pass_lo_B"]

  if (Baseline_Run == TRUE){
    ## Leisure Loop to adjust the Income Elasticity based on GDP/Capita treshold and the previous calculated decay rate based on RPK/Capita treshold
    price_el_int_aviation_L[GDP_cap > GDP_treshold_L, eps := eps*decay_DR_L, by = c("region", "year")]
    ## Business Loop to adjust the Income Elasticity based on GDP/Capita treshold and the previous calculated decay rate based on RPK/Capita treshold
    price_el_int_aviation_B[GDP_cap > GDP_treshold_B, eps := eps*decay_DR_B, by = c("region", "year")]

    price_el_int_aviation_L = dcast(price_el_int_aviation_L[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")
    price_el_int_aviation_B = dcast(price_el_int_aviation_B[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")
    ## adjust specific regions otherwise their demand grows too fast
    price_el[region %in% c("OAS", "LAM", "UKI") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=eps*0.5]
    price_el[region %in% c("SSA", "MEA") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=eps*0.75]
    price_el[region %in% c("NES", "ESC", "CAZ", "NEN") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=eps*0.05]
    price_el[region %in% c("NES", "NEN") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps := 0]
    price_el[var %in% c("price_elasticity_freight_lo", "price_elasticity_freight_sm", "price_elasticity_pass_sm"), eps := 0]
    price_el = dcast(price_el[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")
    price_el = merge(price_el, price_el_int_aviation_L[,c(1,2,4)], by = c("region","year"),all.x = TRUE)
    price_el = merge(price_el, price_el_int_aviation_B[,c(1,2,4)], by = c("region","year"),all.x = TRUE)
    ## drop lines which are not needed anymore
    price_el[, c("income_elasticity_pass_lo_L.x","income_elasticity_pass_lo_B.x"):= NULL]
    setnames(price_el, "income_elasticity_pass_lo_L.y", "income_elasticity_pass_lo_L")
    setnames(price_el, "income_elasticity_pass_lo_B.y", "income_elasticity_pass_lo_B")

  } else if (Baseline_Run == FALSE){

    ## get RPK/CAP Data from a previous baseline run
    price_el_int_aviation_L_RPK = merge( price_el_int_aviation_L, RPK_cap_baseline_L, by = c("region","year"),all.x = TRUE)
    price_el_int_aviation_B_RPK = merge( price_el_int_aviation_B, RPK_cap_baseline_B, by = c("region","year"),all.x = TRUE)
    price_el_int_aviation_B_RPK<-transform(price_el_int_aviation_B_RPK, decay_rate= 1)
    price_el_int_aviation_L_RPK<-transform(price_el_int_aviation_L_RPK, decay_rate= 1)
    price_el_int_aviation_L_RPK_adj<-na.omit(price_el_int_aviation_L_RPK)
    price_el_int_aviation_L_RPK_adj<- price_el_int_aviation_L_RPK_adj[,c(1,2,12)]
    price_el_int_aviation_L_RPK_adj=approx_dt(dt = price_el_int_aviation_L_RPK_adj, ## database to interpolate
                                              xdata = seq(1965,2150,5), ## time steps on which to interpolate
                                              ycol = "RPKCAP", ## column containing the data to interpolate
                                              xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                              idxcols=c("region"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                              extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
    price_el_int_aviation_B_RPK_adj<-na.omit(price_el_int_aviation_B_RPK)
    price_el_int_aviation_B_RPK_adj<- price_el_int_aviation_B_RPK_adj[,c(1,2,12)]
    price_el_int_aviation_B_RPK_adj=approx_dt(dt = price_el_int_aviation_B_RPK_adj, ## database to interpolate
                                              xdata = seq(1965,2150,5), ## time steps on which to interpolate
                                              ycol = "RPKCAP", ## column containing the data to interpolate
                                              xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                              idxcols=c("region"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                              extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
    price_el_int_aviation_L_RPK<-merge(price_el_int_aviation_L_RPK[,c(1:11,13)], price_el_int_aviation_L_RPK_adj, by = c("region","year"),all.x = TRUE)
    price_el_int_aviation_B_RPK<-merge(price_el_int_aviation_B_RPK[,c(1:11,13)], price_el_int_aviation_B_RPK_adj, by = c("region","year"),all.x = TRUE)
    ## Leisure Loop to adjust the decay_rate based on RPK/Capita data
    price_el_int_aviation_L_RPK[RPKCAP > decay_treshold_L, decay_rate := decay_rate*decay_DR_L, by = c("region")]
    ## Business Loop to adjust the decay_rate based on RPK/Capita data
    price_el_int_aviation_B_RPK[RPKCAP > decay_treshold_B, decay_rate := decay_rate*decay_DR_B, by = c("region")]


    price_el_int_aviation_L_RPK<- price_el_int_aviation_L_RPK[, c(3:11,13):= NULL]
    price_el_int_aviation_L = merge(price_el_int_aviation_L, price_el_int_aviation_L_RPK, by = c("region","year"),all.x = TRUE)
    price_el_int_aviation_B_RPK<- price_el_int_aviation_B_RPK[, c(3:11,13):= NULL]
    price_el_int_aviation_B = merge(price_el_int_aviation_B, price_el_int_aviation_B_RPK, by = c("region","year"),all.x = TRUE)
    ## Leisure Loop to adjust the Income Elasticity based on GDP/Capita treshold and the previous calculated decay rate based on RPK/Capita treshold
    price_el_int_aviation_L[GDP_cap > GDP_treshold_L, eps := eps*decay_rate, by = c("region")]
    ## Business Loop to adjust the Income Elasticity based on GDP/Capita treshold and the previous calculated decay rate based on RPK/Capita treshold
    price_el_int_aviation_B[GDP_cap > GDP_treshold_B, eps := eps*decay_rate, by = c("region")]


    price_el_int_aviation_L = dcast(price_el_int_aviation_L[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")
    price_el_int_aviation_B = dcast(price_el_int_aviation_B[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")
    ## adjust specific regions otherwise their demand grows too fast
    price_el[region %in% c("OAS", "LAM", "UKI") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=eps*0.5]
    price_el[region %in% c("SSA", "MEA") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=eps*0.75]
    price_el[region %in% c("NES", "ESC", "CAZ", "NEN") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=eps*0.05]
    price_el[region %in% c("NES", "NEN") & var %in% c("income_elasticity_pass_lo_L","income_elasticity_pass_lo_B"), eps :=0]
    price_el[var %in% c("price_elasticity_freight_lo", "price_elasticity_freight_sm", "price_elasticity_pass_sm"), eps := 0]
    price_el = dcast(price_el[,c("region","year","var","eps", "GDP_cap")], region + year + GDP_cap ~var, value.var = "eps")
    price_el = merge(price_el, price_el_int_aviation_L[,c(1,2,4)], by = c("region","year"),all.x = TRUE)
    price_el = merge(price_el, price_el_int_aviation_B[,c(1,2,4)], by = c("region","year"),all.x = TRUE)
    ## drop lines which are not needed anymore
    price_el[, c("income_elasticity_pass_lo_L.x","income_elasticity_pass_lo_B.x"):= NULL]
    setnames(price_el, "income_elasticity_pass_lo_L.y", "income_elasticity_pass_lo_L")
    setnames(price_el, "income_elasticity_pass_lo_B.y", "income_elasticity_pass_lo_B")
  }

  ## calculate growth rates
  gdp_pop[,`:=`(index_GDP=GDP_val/shift(GDP_val), index_GDPcap=GDP_cap/shift(GDP_cap), index_POP=POP_val/shift(POP_val)), by=c("region")]
  ## merge GDP_POP and price elasticity
  gdp_pop = merge(gdp_pop, price_el[,c("region", "year", "income_elasticity_pass_lo_L", "income_elasticity_pass_lo_B", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("region", "year"))

  #calculate the indexes raised to the corresponding elasticities
  gdp_pop[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo_L=index_GDPcap^income_elasticity_pass_lo_L,
                index_GDPcap_p_lo_B=index_GDPcap^income_elasticity_pass_lo_B)]
  gdp_pop[,c("income_elasticity_freight_sm", "income_elasticity_freight_lo", "income_elasticity_pass_sm", "income_elasticity_pass_lo_L","income_elasticity_pass_lo_B") := NULL]


  ## order the prices according to the year, within the sector
  price_baseline=price_baseline[order(-frank(sector), year)]
  ## calculate "index" which represent the growth of total price
  price_baseline[,index_price:=tot_price/shift(tot_price),by=c("region","sector")]
  #select only the needed columns
  price_baseline=price_baseline[, c("region","year","sector","index_price")]
  ## from long to wide format, so that the df has separate columns for all transport modes
  price_baseline=dcast(price_baseline, region + year  ~ sector, value.var = "index_price", fun.aggregate = sum, margins="sector")
  ## merge with elasticities
  price_baseline = merge(price_baseline, price_el[,c("region", "year", "price_elasticity_pass_lo_L","price_elasticity_pass_lo_B", "price_elasticity_pass_sm", "price_elasticity_freight_sm", "price_elasticity_freight_lo")], by = c("region", "year"))
  ## calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_f_sm=trn_freight^price_elasticity_freight_sm,
                       index_price_f_lo=trn_shipping_intl^price_elasticity_freight_lo,
                       index_price_p_sm=trn_pass^price_elasticity_pass_sm,
                       index_price_p_lo_L=trn_aviation_intl^price_elasticity_pass_lo_L,
                       index_price_p_lo_B=trn_aviation_intl^price_elasticity_pass_lo_B)]

  price_baseline[,c("price_elasticity_freight_sm", "price_elasticity_freight_lo", "price_elasticity_pass_sm", "price_elasticity_pass_lo_L", "price_elasticity_pass_lo_B") := NULL]

  ## create the D* df
  D_star=merge(price_baseline,gdp_pop,by = c("region","year"))
  ## calculate D* for each mode separately, and select only the useful cols
  D_star=D_star[,.(D_star_f_sm=index_price_f_sm*index_GDP_f_sm,
                   D_star_f_lo=index_price_f_lo*index_GDP_f_lo,
                   D_star_p_sm=index_price_p_sm*index_GDPcap_p_sm*index_POP,
                   D_star_p_lo_L=index_price_p_lo_L*index_GDPcap_p_lo_L*index_POP,
                   D_star_p_lo_B=index_price_p_lo_B*index_GDPcap_p_lo_B*index_POP,
                   region,
                   year)]
  ## calculate demand at a sector level
  demand_tot_sector=tech_output[, .(demand_tot=sum(tech_output)), by=c("region", "year", "sector")]

  ## calculate 2020 demand for aviation industry based on ICCT Data
  demand_tot_sector_avi<- demand_tot_sector[sector == "trn_aviation_intl"]
  demand_tot_sector_avi <- rbind(demand_tot_sector_avi,ICCT_data)
  ## interpolate
  demand_tot_sector_avi=approx_dt(dt = demand_tot_sector_avi, ## database to interpolate
                                  xdata = seq(1990,2020,5), ## time steps on which to interpolate
                                  ycol = "demand_tot", ## column containing the data to interpolate
                                  xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                  idxcols=c("region", "sector"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                  extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  ## from long to wide format, so that the df has separate columns for all transport modes
  demand_tot_sector=dcast(demand_tot_sector, region + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  demand_tot_sector_avi=dcast(demand_tot_sector_avi, region + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")

  ## merge D* and historical demand
  D_star_avi=merge(D_star,demand_tot_sector_avi, by = c("region","year"),all.x = TRUE)
  D_star=merge(D_star,demand_tot_sector, by = c("region","year"),all.x = TRUE)


  ## Split international aviation in business and leisure based on a survey IPSOS, 2017
  D_star_avi <- transform( D_star_avi, trn_aviation_intl_L = trn_aviation_intl * 0.625)
  D_star_avi <- transform( D_star_avi, trn_aviation_intl_B = trn_aviation_intl * 0.375)


  ## for loop that calculates the value of the following time step of demand based on the growth of the indexes
  i=NULL
  for (i in seq(1,length(unique(D_star$year)),1)) {
    D_star[year>=2010,tmp:=shift(trn_freight)*D_star_f_sm,by=c("region")]
    D_star[is.na(trn_freight) & !is.na(tmp),trn_freight:=tmp]
    D_star[year>=2010,tmp:=shift(trn_pass)*D_star_p_sm,by=c("region")]
    D_star[is.na(trn_pass) & !is.na(tmp),trn_pass:=tmp]
    D_star[year>=2010,tmp:=shift(trn_shipping_intl)*D_star_f_lo,by=c("region")]
    D_star[is.na(trn_shipping_intl) & !is.na(tmp),trn_shipping_intl:=tmp]
    i=i+1
  }
  ## international aviation  specific loop
  ## interpolate  the 10 years to 5 years time step to be consistent for the demand regression & effect of income elasticity
  D_star_avi=approx_dt(dt = D_star_avi, ## database to interpolate
                       xdata = seq(1990,2100,5), ## time steps on which to interpolate
                       ycol = "D_star_p_lo_L", ## column containing the data to interpolate
                       xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                       idxcols=c("region"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                       extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  D_star_avi=approx_dt(dt = D_star_avi, ## database to interpolate
                       xdata = seq(1990,2100,5), ## time steps on which to interpolate
                       ycol = "D_star_p_lo_B", ## column containing the data to interpolate
                       xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                       idxcols=c("region"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                       extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  i=NULL
  for (i in seq(1,length(unique(D_star$year)),1)) {
    D_star_avi[year>=2020,tmp:=shift(trn_aviation_intl_L)*D_star_p_lo_L,by=c("region")]
    D_star_avi[is.na(trn_aviation_intl_L) & !is.na(tmp),trn_aviation_intl_L:=tmp]
    D_star_avi[year>=2020,tmp:=shift(trn_aviation_intl_B)*D_star_p_lo_B,by=c("region")]
    D_star_avi[is.na(trn_aviation_intl_B) & !is.na(tmp),trn_aviation_intl_B:=tmp]
    i=i+1
  }
  ## select only the columns that contains the demand
  D_star[, c("tmp", "D_star_f_sm", "D_star_p_sm", "D_star_f_lo", "D_star_p_lo_L","D_star_p_lo_B","trn_aviation_intl_B","trn_aviation_intl_L","trn_aviation_intl") := NULL]
  D_star_avi[, c( "tmp", "D_star_f_sm", "D_star_p_sm", "D_star_f_lo", "D_star_p_lo_L","D_star_p_lo_B","trn_freight","trn_pass","trn_shipping_intl","trn_aviation_intl") := NULL]
  D_star<-merge(D_star,D_star_avi, by=c("region","year"), all.x = TRUE)
  ## COVID ADJUSTMENT - Totally exogenous COVID shock based on certain assumptions and actual COVID-impact data from the year 2020
  COVID_shock = fread(file.path(input_folder, COVID_dir, "COVID.csv"))
  D_star=merge(D_star, COVID_shock,by = c("region","year"), all.x = TRUE)
  ## find values to be used depending on the SSP number
  coeff_touse_L = paste0("I_L_", gsub("[^\\d]+", "", REMIND_scenario, perl=TRUE))
  coeff_touse_B = paste0("I_B_", gsub("[^\\d]+", "", REMIND_scenario, perl=TRUE))

  D_star[year > 2019, trn_aviation_intl_L :=  trn_aviation_intl_L*get(coeff_touse_L), by = c("region", "year")]
  D_star[year > 2019, trn_aviation_intl_B :=  trn_aviation_intl_B*get(coeff_touse_B), by = c("region", "year")]

 if (Baseline_Run == TRUE){
   ## prepare the RPK/Capita Baseline Demands for Demand Regression
    RPK_cap_baseline_L = D_star[,c("region", "year", "trn_aviation_intl_L")]
    RPK_cap_baseline_B = D_star[,c("region", "year", "trn_aviation_intl_B")]
    RPK_cap_baseline_L<-merge (RPK_cap_baseline_L, GDP_POP[,c(1,2,4)],by = c("region","year"),all.x = TRUE)
    RPK_cap_baseline_L<-transform(RPK_cap_baseline_L, RPKCAP= trn_aviation_intl_L /POP_val)
    RPK_cap_baseline_L<-RPK_cap_baseline_L[,c(1,2,5)]
    RPK_cap_baseline_B = D_star[,c("region", "year", "trn_aviation_intl_B")]
    RPK_cap_baseline_B<-merge (RPK_cap_baseline_B, GDP_POP[,c(1,2,4)],by = c("region","year"),all.x = TRUE)
    RPK_cap_baseline_B<-transform(RPK_cap_baseline_B, RPKCAP= trn_aviation_intl_B /POP_val)
    RPK_cap_baseline_B<-RPK_cap_baseline_B[,c(1,2,5)]
    Baseline_data = list(RPK_cap_baseline_L = RPK_cap_baseline_L,
                         RPK_cap_baseline_B = RPK_cap_baseline_B)

    return(Baseline_data)

 } else if (Baseline_Run == FALSE){
    D_star[,trn_aviation_intl:= trn_aviation_intl_L + trn_aviation_intl_B, by = c("region", "year")]
    D_star_av = D_star[,c("region", "year", "trn_aviation_intl_L", "trn_aviation_intl_B")]
    D_star<- D_star[, c("trn_aviation_intl_L", "trn_aviation_intl_B"):= NULL]
    D_star<- D_star[, trn_aviation_intl := ifelse(region == "NES", trn_aviation_intl*0.25, trn_aviation_intl)]
    D_star<- D_star[, trn_aviation_intl := ifelse(region == "NEN", trn_aviation_intl*0.25, trn_aviation_intl)]
    D_star<- D_star[, trn_aviation_intl := ifelse(region == "CAZ", trn_aviation_intl*0.35, trn_aviation_intl)]
    D_star<- D_star[, trn_aviation_intl := ifelse(region == "ECS", trn_aviation_intl*0.45, trn_aviation_intl)]
    D_star<- D_star[, trn_aviation_intl := ifelse(region == "ECE", trn_aviation_intl*0.55, trn_aviation_intl)]
    D_star = melt(D_star, id.vars = c("region", "year"),
                  measure.vars = c("trn_freight", "trn_pass", "trn_shipping_intl", "trn_aviation_intl"))
    D_star = D_star[,.(region, year, demand = value, sector = variable)]

    return(D_star)
  }

}
