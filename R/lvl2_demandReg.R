#' Estimate future transport demand using a price trajectory, GDP, and historical demand.
#'
#'
#' @param tech_output historically calibrated demand
#' @param price_baseline baseline prices
#' @param GDP_POP GDP per capita
#' @param REMIND_scenario SSP scenario
#' @param smartlifestyle switch activating sustainable lifestyles
#' @importFrom rmndt approx_dt
#' @return transport demand projections
#' @author Marianna Rottoli
#'
#' @importFrom data.table shift frank


lvl2_demandReg <- function(tech_output, price_baseline, GDP_POP, REMIND_scenario, smartlifestyle,ICCT_data_I,ICCT_data_D,GDP_country,REMIND2ISO_MAPPING_adj,REMIND2ISO_MAPPING,COVID_scenario){
  rich <- var <- eps <- GDP_cap <- iso <- eps1 <- eps2 <- GDP_val <- POP_val <- NULL
  index_GDP <- income_elasticity_freight_sm <- income_elasticity_freight_lo <- index_GDPcap <- NULL
  income_elasticity_pass_sm <- income_elasticity_pass_lo <- price_elasticity_pass_lo <- sector <- NULL
  index_price <- tot_price <- trn_freight <- price_elasticity_freight_sm <- trn_shipping_intl <- NULL
  price_elasticity_freight_lo <- trn_pass <- price_elasticity_pass_sm <- trn_aviation_intl <- `.` <- NULL
  index_price_f_sm <- index_price_f_lo <- index_GDP_f_sm <- index_GDPcap_p_lo <- index_GDP_f_lo <- NULL
  index_price_p_sm <- index_GDPcap_p_sm <- index_POP <- index_price_p_lo <- D_star_f_sm <- D_star_p_sm <- NULL
  D_star_p_lo <- D_star_f_lo <- D_star_f_sm <- value <- variable <- vrich <- vpoor <-NULL
  ## conversion rate 2005->1990 USD
  CONV_2005USD_1990USD = 0.67
  #PARAMETERS FOR ELASTICITY
  #RPK Treshold & Decay
  if (REMIND_scenario == "SSP1") {
    decay_DR_L=0.3
    decay_treshold_L= 700
  }else if (REMIND_scenario == "SSP2") {
    decay_DR_L=0.5
    decay_treshold_L= 900
  }else if (REMIND_scenario == "SSP3") {
    decay_DR_L=0.6
    decay_treshold_L= 1350
  }else if (REMIND_scenario == "SSP4") {
    decay_DR_L=0.6
    decay_treshold_L= 1350
  }else if (REMIND_scenario == "SSP5") {
    decay_DR_L=0.8
    decay_treshold_L= 1500
  }else{}
  #mutate the decayrate for Income elasticity Business
  if (REMIND_scenario == "SSP1") {
    decay_DR_B=0.25
    decay_treshold_B= 400
  }else if (REMIND_scenario == "SSP2") {
    decay_DR_B=0.4
    decay_treshold_B= 600
  }else if (REMIND_scenario == "SSP3") {
    decay_DR_B=0.7
    decay_treshold_B= 850
  }else if (REMIND_scenario == "SSP4") {
    decay_DR_B=0.7
    decay_treshold_B= 850
  }else if (REMIND_scenario == "SSP5") {
    decay_DR_B=0.7
    decay_treshold_B= 1200
  }else{}
  #GDP Treshold
  #Decay of Income Elasticity
  #International
  #Leisure
  if (REMIND_scenario == "SSP1") {
    GDP_treshold_L= 30000
  }else if (REMIND_scenario == "SSP2") {
    GDP_treshold_L= 35000
  }else if (REMIND_scenario == "SSP3") {
    GDP_treshold_L= 55000
  }else if (REMIND_scenario == "SSP4") {
    GDP_treshold_L= 55000
  }else if (REMIND_scenario == "SSP5") {
    GDP_treshold_L= 60000
  }else{}
  
  #Business
  if (REMIND_scenario == "SSP1") {
    GDP_treshold_B= 20000
  }else if (REMIND_scenario == "SSP2") {
    GDP_treshold_B= 25000
  }else if (REMIND_scenario == "SSP3") {
    GDP_treshold_B= 50000
  }else if (REMIND_scenario == "SSP4") {
    GDP_treshold_B= 50000
  }else if (REMIND_scenario == "SSP5") {
    GDP_treshold_B= 55000
  }else{}
  #Domestic Derivation
  if (REMIND_scenario == "SSP1") {
    floor_domestic_leisure = 0.2
    floor_domestic_business = 0.1
    top_domestic_leisure = 1.3
    top_domestic_business = 1.4
    special_country_floor_leisure = 0.2
    special_country_floor_business = 0.15
  }else if (REMIND_scenario == "SSP2") {
    floor_domestic_leisure = 0.3
    floor_domestic_business = 0.15
    top_domestic_leisure = 1.3
    top_domestic_business = 1.3
    special_country_floor_leisure = 0.25
    special_country_floor_business = 0.2
  }else if (REMIND_scenario == "SSP3") {
    floor_domestic_leisure = 0.20
    floor_domestic_business = 0.10
    top_domestic_leisure = 1.3
    top_domestic_business = 1.3
    special_country_floor_leisure = 0.8
    special_country_floor_business = 0.8
  }else if (REMIND_scenario == "SSP4") {
    floor_domestic_leisure = 0.20
    floor_domestic_business = 0.10
    top_domestic_leisure = 1.3
    top_domestic_business = 1.3
    special_country_floor_leisure = 0.8
    special_country_floor_business = 0.8
  }else if (REMIND_scenario == "SSP5") {
    floor_domestic_leisure = 0.4
    floor_domestic_business = 0.2
    top_domestic_leisure = 1.3
    top_domestic_business = 1.3
    special_country_floor_leisure = 0.3
    special_country_floor_business = 0.25
  }else{}

  ## Create a dt with GDP, POP and GDP_cap with EDGE isos
  gdp_pop = copy(GDP_POP)
  setnames(gdp_pop, old = "weight", new = "GDP_val")
  ## create ct with the various elasticities
  price_el = gdp_pop[,-"variable"]
  price_el_a <- disaggregate_dt(price_el, REMIND2ISO_MAPPING,
                               valuecol="GDP_cap",
                               datacols = "year",
                               weights=GDP_country)
  price_el_b <- disaggregate_dt(price_el, REMIND2ISO_MAPPING,
                               valuecol="GDP_val",
                               datacols = "year",
                               weights=GDP_country)
  price_el_c <- disaggregate_dt(price_el, REMIND2ISO_MAPPING,
                               valuecol="POP_val",
                               datacols = "year",
                               weights=GDP_country)
  price_el<-merge(price_el_a[,c(1,2,5)], price_el_b[,c(1,2,3)])
  price_el<-merge(price_el, price_el_c[,c(1,2,4)])
  tmp = CJ(iso=unique(price_el$iso), var =c("income_elasticity_pass_sm",
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
  tmp[, vrich := ifelse(var == "income_elasticity_pass_lo_L", 1.6, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_lo_L", -0.25, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo_L", 1.7, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo_L", -0.5, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_lo_L", 1.8, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_lo_L", -0.7, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo_L", 1.5, norm)]
  tmp[, norm := ifelse(var == "price_elasticity_pass_lo_L", -1, norm)]
  tmp[, vrich := ifelse(var == "income_elasticity_pass_lo_B", 1.7, vrich)]
  tmp[, vrich := ifelse(var == "price_elasticity_pass_lo_B", -0.25, vrich)]
  tmp[, rich := ifelse(var == "income_elasticity_pass_lo_B", 1.9, rich)]
  tmp[, rich := ifelse(var == "price_elasticity_pass_lo_B", -0.5, rich)]
  tmp[, vpoor := ifelse(var == "income_elasticity_pass_lo_B", 1.5, vpoor)]
  tmp[, vpoor := ifelse(var == "price_elasticity_pass_lo_B", -0.7, vpoor)]
  tmp[, norm := ifelse(var == "income_elasticity_pass_lo_B", 2, norm)]
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

  price_el = merge(price_el, tmp, by = "iso", allow.cartesian = TRUE)
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

  price_el<-merge(price_el,REMIND2ISO_MAPPING, by =c("iso"))
  price_el_int_aviation_L <- price_el[var == "income_elasticity_pass_lo_L"]
  price_el_int_aviation_B <- price_el[var == "income_elasticity_pass_lo_B"]
  #get RPK/CAP
  RPK_cap_baseline_L=fread(system.file("extdata", "RPK-CAP-L.csv", package = "edgeTransport"))
  RPK_cap_baseline_B=fread(system.file("extdata", "RPK-CAP-B.csv", package = "edgeTransport"))
  RPK_cap_baseline_L <- RPK_cap_baseline_L[scenario == REMIND_scenario]
  RPK_cap_baseline_B <- RPK_cap_baseline_B[scenario == REMIND_scenario]
  RPK_cap_baseline_B <-na.omit(RPK_cap_baseline_B)
  RPK_cap_baseline_L <-na.omit(RPK_cap_baseline_L)
  price_el_int_aviation_L_RPK = merge( price_el_int_aviation_L, RPK_cap_baseline_L[,c(1,2,5)], by = c("iso","year"),all.x = TRUE)
  price_el_int_aviation_B_RPK = merge( price_el_int_aviation_B, RPK_cap_baseline_B[,c(1,2,5)], by = c("iso","year"),all.x = TRUE)
  price_el_int_aviation_B_RPK<-transform(price_el_int_aviation_B_RPK, decay_rate= 1)
  price_el_int_aviation_L_RPK<-transform(price_el_int_aviation_L_RPK, decay_rate= 1)
  price_el_int_aviation_L_RPK_adj<-na.omit(price_el_int_aviation_L_RPK)
  price_el_int_aviation_L_RPK_adj<- price_el_int_aviation_L_RPK_adj[,c(1,2,13)]
  price_el_int_aviation_L_RPK_adj=approx_dt(dt = price_el_int_aviation_L_RPK_adj, ## database to interpolate
                     xdata = seq(2000,2150,1), ## time steps on which to interpolate
                     ycol = "RPKCAP", ## column containing the data to interpolate
                     xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                     idxcols=c("iso"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                     extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  price_el_int_aviation_B_RPK_adj<-na.omit(price_el_int_aviation_B_RPK)
  price_el_int_aviation_B_RPK_adj<- price_el_int_aviation_B_RPK_adj[,c(1,2,13)]
  price_el_int_aviation_B_RPK_adj=approx_dt(dt = price_el_int_aviation_B_RPK_adj, ## database to interpolate
                                            xdata = seq(2000,2150,1), ## time steps on which to interpolate
                                            ycol = "RPKCAP", ## column containing the data to interpolate
                                            xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                            idxcols=c("iso"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                            extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  price_el_int_aviation_L_RPK<-na.omit(price_el_int_aviation_L_RPK)
  price_el_int_aviation_B_RPK<-na.omit(price_el_int_aviation_B_RPK)
  price_el_int_aviation_L_RPK<-merge(price_el_int_aviation_L_RPK[,c(1:11,14)], price_el_int_aviation_L_RPK_adj, by = c("iso","year"),all.x = TRUE)
  price_el_int_aviation_B_RPK<-merge(price_el_int_aviation_B_RPK[,c(1:11,14)], price_el_int_aviation_B_RPK_adj, by = c("iso","year"),all.x = TRUE)
  #Leisure Loop
  for (j in unique(price_el_int_aviation_L_RPK$iso)) {
    for (i in unique(price_el_int_aviation_L_RPK$year[price_el_int_aviation_L_RPK$iso == j])) { 
      if (price_el_int_aviation_L_RPK$RPKCAP[price_el_int_aviation_L_RPK$iso == j & price_el_int_aviation_L_RPK$year == i] > decay_treshold_L) { 
        price_el_int_aviation_L_RPK$decay_rate[price_el_int_aviation_L_RPK$iso == j & price_el_int_aviation_L_RPK$year >= i] <- price_el_int_aviation_L_RPK$decay_rate[price_el_int_aviation_L_RPK$iso == j & price_el_int_aviation_L_RPK$year >= i] * decay_DR_L
      }
    }
  }
  #Business Loop
  for (j in unique(price_el_int_aviation_B_RPK$iso)) {
    for (i in unique(price_el_int_aviation_B_RPK$year[price_el_int_aviation_B_RPK$iso == j])) { 
      if (price_el_int_aviation_B_RPK$RPKCAP[price_el_int_aviation_B_RPK$iso == j & price_el_int_aviation_B_RPK$year == i] > decay_treshold_B) { 
        price_el_int_aviation_B_RPK$decay_rate[price_el_int_aviation_B_RPK$iso == j & price_el_int_aviation_B_RPK$year >= i] <- price_el_int_aviation_B_RPK$decay_rate[price_el_int_aviation_B_RPK$iso == j & price_el_int_aviation_B_RPK$year >= i] * decay_DR_B
      }
    }
  }

  price_el_int_aviation_L_RPK<- price_el_int_aviation_L_RPK[, c(3:11,13):= NULL] 
  price_el_int_aviation_L = merge(price_el_int_aviation_L, price_el_int_aviation_L_RPK, by = c("iso","year"),all.x = TRUE)
  price_el_int_aviation_B_RPK<- price_el_int_aviation_B_RPK[, c(3:11,13):= NULL] 
  price_el_int_aviation_B = merge(price_el_int_aviation_B, price_el_int_aviation_B_RPK, by = c("iso","year"),all.x = TRUE)
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

  price_el_int_aviation_L = dcast(price_el_int_aviation_L[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps")  
  price_el_int_aviation_B = dcast(price_el_int_aviation_B[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps") 
  price_el = dcast(price_el[,c("iso","year","var","eps", "GDP_cap")], iso + year + GDP_cap ~var, value.var = "eps")
  price_el = merge(price_el, price_el_int_aviation_L[,c(1,2,4)], by = c("iso","year"),all.x = TRUE)
  price_el = merge(price_el, price_el_int_aviation_B[,c(1,2,4)], by = c("iso","year"),all.x = TRUE)
  #drop stuff
  price_el[, c("income_elasticity_pass_lo_L.x","income_elasticity_pass_lo_B.x"):= NULL]
  setnames(price_el, "income_elasticity_pass_lo_L.y", "income_elasticity_pass_lo_L")
  setnames(price_el, "income_elasticity_pass_lo_B.y", "income_elasticity_pass_lo_B")
  #adjust for region specifications
  price_el<-merge(price_el, REMIND2ISO_MAPPING_adj, by=c("iso"))
  price_el[, income_elasticity_pass_lo_L := ifelse( region %in% c("OAS","CAZ","LAM", "NEU"), income_elasticity_pass_lo_L *0.5, income_elasticity_pass_lo_L )]
  price_el[, income_elasticity_pass_lo_B := ifelse( region %in% c("OAS","CAZ","LAM", "NEU"), income_elasticity_pass_lo_B *0.5, income_elasticity_pass_lo_B )]
  price_el[, income_elasticity_pass_lo_L := ifelse( region %in% c("SSA","MEA"), income_elasticity_pass_lo_L *0.75, income_elasticity_pass_lo_L )]
  price_el[, income_elasticity_pass_lo_B := ifelse( region %in% c("SSA", "MEA"), income_elasticity_pass_lo_B *0.75, income_elasticity_pass_lo_B )]
  price_el[, income_elasticity_pass_lo_L := ifelse( region %in% c("REF"), income_elasticity_pass_lo_L *0.85, income_elasticity_pass_lo_L )]
  price_el[, income_elasticity_pass_lo_B := ifelse( region %in% c("REF"), income_elasticity_pass_lo_B *0.85, income_elasticity_pass_lo_B )]

  price_el<-price_el[,c("region"):= NULL]
  price_el=melt( price_el,id.vars = c("iso","year"))
  price_el=approx_dt(dt = price_el, ## database to interpolate
                                            xdata = seq(1965,2150,1), ## time steps on which to interpolate
                                            ycol = "value", ## column containing the data to interpolate
                                            xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                            idxcols=c("iso", "variable"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                            extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  price_el=dcast( price_el,iso+year~variable,value.var="value")
  #disaggreagte  data
  gdp_pop_a <- disaggregate_dt(gdp_pop, REMIND2ISO_MAPPING,
                               valuecol="GDP_cap",
                               datacols = "year",
                               weights=GDP_country)
  gdp_pop_b <- disaggregate_dt(gdp_pop, REMIND2ISO_MAPPING,
                             valuecol="GDP_val",
                             datacols = "year",
                             weights=GDP_country)
  gdp_pop_c <- disaggregate_dt(gdp_pop, REMIND2ISO_MAPPING,
                             valuecol="POP_val",
                             datacols = "year",
                             weights=GDP_country)
  gdp_pop<-merge(gdp_pop_a[,c(1,2,5)], gdp_pop_b[,c(1,2,3)])
  gdp_pop<-merge(gdp_pop, gdp_pop_c[,c(1,2,4)])
  gdp_pop=melt(gdp_pop,id.vars = c("iso","year"))
  gdp_pop=approx_dt(dt = gdp_pop, ## database to interpolate
                    xdata = seq(1965,2150,1), ## time steps on which to interpolate
                    ycol = "value", ## column containing the data to interpolate
                    xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                    idxcols=c("iso", "variable"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                    extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  
  ## back to previous format
  gdp_pop=dcast(gdp_pop,iso+year~variable,value.var="value")
  #calculate growth rates
  gdp_pop[,`:=`(index_GDP=GDP_val/shift(GDP_val), index_GDPcap=GDP_cap/shift(GDP_cap), index_POP=POP_val/shift(POP_val)), by=c("iso")]
  ## merge GDP_POP and price elasticity
  gdp_pop = merge(gdp_pop, price_el[,c("iso", "year", "income_elasticity_pass_lo_L", "income_elasticity_pass_lo_B", "income_elasticity_pass_sm", "income_elasticity_freight_sm", "income_elasticity_freight_lo")], by = c("iso", "year"))

  #calculate the indexes raised to the corresponding elasticities
  gdp_pop[,`:=`(index_GDP_f_sm=index_GDP^income_elasticity_freight_sm,
                index_GDP_f_lo=index_GDP^income_elasticity_freight_lo,
                index_GDPcap_p_sm=index_GDPcap^income_elasticity_pass_sm,
                index_GDPcap_p_lo_L=index_GDPcap^income_elasticity_pass_lo_L,
                index_GDPcap_p_lo_B=index_GDPcap^income_elasticity_pass_lo_B)]
  gdp_pop[,c("income_elasticity_freight_sm", "income_elasticity_freight_lo", "income_elasticity_pass_sm", "income_elasticity_pass_lo_L","income_elasticity_pass_lo_B") := NULL]

  #order the prices according to the year, within the sector
  price_baseline=price_baseline[order(-frank(sector), year)]
  #get iso and yearly data
  price_baseline <- disaggregate_dt(price_baseline, REMIND2ISO_MAPPING,
                               valuecol="tot_price",
                               datacols = "year",
                               weights=GDP_country)
  
  price_baseline=melt(price_baseline,id.vars = c("iso","year","sector"))
  price_baseline=approx_dt(dt = price_baseline, ## database to interpolate
                    xdata = seq(1965,2150,1), ## time steps on which to interpolate
                    ycol = "value", ## column containing the data to interpolate
                    xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                    idxcols=c("iso", "variable","sector"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                    extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  
  ## back to previous format
  price_baseline=dcast(price_baseline,iso+year+sector~variable,value.var="value")
  #calculate "index" which represent the growth of total price
  price_baseline[,index_price:=tot_price/shift(tot_price),by=c("iso","sector")]
  #select only the needed columns
  price_baseline=price_baseline[, c("iso","year","sector","index_price")]
  #from long to wide format, so that the df has separate columns for all transport modes
  price_baseline=dcast(price_baseline, iso + year  ~ sector, value.var = "index_price", fun.aggregate = sum, margins="sector")
  ## merge with elasticities
  price_baseline = merge(price_baseline, price_el[,c("iso", "year", "price_elasticity_pass_lo_L","price_elasticity_pass_lo_B", "price_elasticity_pass_sm", "price_elasticity_freight_sm", "price_elasticity_freight_lo")], by = c("iso", "year"))
  #calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_f_sm=trn_freight^price_elasticity_freight_sm,
                             index_price_f_lo=trn_shipping_intl^price_elasticity_freight_lo,
                             index_price_p_sm=trn_pass^price_elasticity_pass_sm,
                             index_price_p_lo_L=trn_aviation_intl^price_elasticity_pass_lo_L,
                             index_price_p_lo_B=trn_aviation_intl^price_elasticity_pass_lo_B)]

  price_baseline[,c("price_elasticity_freight_sm", "price_elasticity_freight_lo", "price_elasticity_pass_sm", "price_elasticity_pass_lo_L", "price_elasticity_pass_lo_B") := NULL]

  #create the D* df
  D_star=merge(price_baseline,gdp_pop,by = c("iso","year"))
  #calculate D* for each mode separately, and select only the useful cols
  D_star=D_star[,.(D_star_f_sm=index_price_f_sm*index_GDP_f_sm,
               D_star_f_lo=index_price_f_lo*index_GDP_f_lo,
               D_star_p_sm=index_price_p_sm*index_GDPcap_p_sm*index_POP,
               D_star_p_lo_L=index_price_p_lo_L*index_GDPcap_p_lo_L*index_POP,
               D_star_p_lo_B=index_price_p_lo_B*index_GDPcap_p_lo_B*index_POP,
               iso,
               year)]
  #calculate demand at a sector level
  demand_tot_sector=tech_output[, .(demand_tot=sum(tech_output)), by=c("region", "year", "sector")]
  demand_tot_sector <- disaggregate_dt(demand_tot_sector, REMIND2ISO_MAPPING,
                                    valuecol="demand_tot",
                                    datacols = "year",
                                    weights=GDP_country)
  #calculate 2020 demand for aviation industry based on ICCT Data
  demand_tot_sector_avi= demand_tot_sector
  demand_tot_sector_avi<- demand_tot_sector_avi[demand_tot_sector_avi$sector %like% "trn_aviation_intl"]
  setnames(ICCT_data_I,"value", "demand_tot")
  demand_tot_sector_avi <- rbind(demand_tot_sector_avi,ICCT_data_I)
  ## interpolate
  demand_tot_sector_avi=approx_dt(dt = demand_tot_sector_avi, ## database to interpolate
                                    xdata = seq(1990,2020,1), ## time steps on which to interpolate
                                    ycol = "demand_tot", ## column containing the data to interpolate
                                    xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                    idxcols=c("iso", "sector"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                    extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  #from long to wide format, so that the df has separate columns for all transport modes
  demand_tot_sector=approx_dt(dt = demand_tot_sector, ## database to interpolate
                                  xdata = seq(1990,2020,1), ## time steps on which to interpolate
                                  ycol = "demand_tot", ## column containing the data to interpolate
                                  xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                  idxcols=c("iso", "sector"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                  extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  demand_tot_sector=dcast(demand_tot_sector, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  demand_tot_sector_avi=dcast(demand_tot_sector_avi, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")

  #merge D* and historical demand
  D_star_avi=merge(D_star,demand_tot_sector_avi, by = c("iso","year"),all.x = TRUE)
  D_star=merge(D_star,demand_tot_sector, by = c("iso","year"),all.x = TRUE)

  
  #Split international aviation in business and leisure based on a survey IPSOS, 2017
  D_star_avi <- transform( D_star_avi, trn_aviation_intl_L = trn_aviation_intl * 0.625)
  D_star_avi <- transform( D_star_avi, trn_aviation_intl_B = trn_aviation_intl * 0.375)

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
  # international aviation loop
  i=NULL
  for (i in seq(1,length(unique(D_star$year)),1)) {
    D_star_avi[year>=2020,tmp:=shift(trn_aviation_intl_L)*D_star_p_lo_L,by=c("iso")]
    D_star_avi[is.na(trn_aviation_intl_L) & !is.na(tmp),trn_aviation_intl_L:=tmp]
    D_star_avi[year>=2020,tmp:=shift(trn_aviation_intl_B)*D_star_p_lo_B,by=c("iso")]
    D_star_avi[is.na(trn_aviation_intl_B) & !is.na(tmp),trn_aviation_intl_B:=tmp]
  i=i+1
  }

  ## select only the columns that contains the demand
  D_star[, c("tmp", "D_star_f_sm", "D_star_p_sm", "D_star_f_lo", "D_star_p_lo_L","D_star_p_lo_B","trn_aviation_intl_B","trn_aviation_intl_L","trn_aviation_intl") := NULL]
  D_star_avi[, c( "tmp", "D_star_f_sm", "D_star_p_sm", "D_star_f_lo", "D_star_p_lo_L","D_star_p_lo_B","trn_freight","trn_pass","trn_shipping_intl","trn_aviation_intl") := NULL]
  D_star<-merge(D_star,D_star_avi, by=c("iso","year"), all.x = TRUE)
  D_star<-na.omit(D_star)
  #domestic Derivation
  ############################
  D_star <- transform( D_star, trn_aviation_intl_d_B = trn_aviation_intl_B)
  D_star <- transform( D_star, trn_aviation_intl_d_L = trn_aviation_intl_L)
  ICCT_data_D=ICCT_data_D
  ICCT_data_D = merge(ICCT_data_D,ICCT_data_I, by=c("iso"),all.x = TRUE)
  ICCT_data_D <- transform( ICCT_data_D, share =value/ demand_tot)
  D_star<-merge(D_star,ICCT_data_D[,c(1,8)] ,by=c("iso"), all.x = TRUE)
  #loop to replace international values with ICCT domestic data
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] >= 2018) { 
        D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i]* D_star$share[D_star$iso == j & D_star$year == i]
      }
    }
  }
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] >= 2018) { 
        D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i]* D_star$share[D_star$iso == j & D_star$year == i]
      }
    }
  }
  D_star<-D_star[,c(1:9)]
  ## interpolate
  Domestic_shares =fread(system.file("extdata", "Domestic_shares.csv", package = "edgeTransport"))
  Domestic_shares[, shares_d_L := ifelse( year == 2100 ,floor_domestic_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 , floor_domestic_business, shares_d_B) ]
  Domestic_shares[, shares_d_L := ifelse( year == 2005 ,top_domestic_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2005 , top_domestic_business, shares_d_B) ]
  #adjust for very larger countries
  Domestic_shares[, shares_d_L := ifelse( year == 2100 & iso == "RUS" , special_country_floor_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 & iso == "RUS"  , special_country_floor_business , shares_d_B) ]
  Domestic_shares[, shares_d_L := ifelse( year == 2100 & iso == "USA" , special_country_floor_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 & iso == "USA"  , special_country_floor_business , shares_d_B) ]
  Domestic_shares[, shares_d_L := ifelse( year == 2100 & iso == "CAN" , special_country_floor_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 & iso == "CAN"  , special_country_floor_business , shares_d_B) ]
  Domestic_shares[, shares_d_L := ifelse( year == 2100 & iso == "AUS" , special_country_floor_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 & iso == "AUS"  , special_country_floor_business , shares_d_B) ]
  Domestic_shares[, shares_d_L := ifelse( year == 2100 & iso == "ARE" , 0.1 , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 & iso == "BHR"  , 0.1 , shares_d_B) ]
  Domestic_shares=melt(Domestic_shares,id.vars = c("iso","year"))
  Domestic_shares=approx_dt(dt = Domestic_shares, ## database to interpolate
                            xdata = seq(1990,2100), ## time steps on which to interpolate
                            ycol = "value", ## column containing the data to interpolate
                            xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                            idxcols=c("iso","variable"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                            extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  Domestic_shares=dcast(Domestic_shares,iso+year~variable,value.var="value")
  #merge again
  D_star<-merge(D_star, Domestic_shares, by = c("iso", "year"), all.x=TRUE)
  D_star<-na.omit(D_star)
  #domestic leisure
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] > 2000) { 
        D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$shares_d_L[ D_star$iso == j & D_star$year == i]
      }
    }
  }
  #domestic business
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] > 2000) { 
        D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$shares_d_B[ D_star$iso == j & D_star$year == i]
      }
    }
  }
  #COVID ADJUSTMENT
  COVID_shock =fread(system.file("extdata", "COVID.csv", package = "edgeTransport"))
  D_star=merge(D_star, COVID_shock,by = c("iso","year"),all.x = TRUE)
  #International Leisure
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_1[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_2[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_3[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_4[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_5[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else{}
  #International Business
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_1[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_2[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_3[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_4[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_5[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else{}
  #Domestic Leisure
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_1[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_2[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_3[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_4[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$I_L_5[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else{}
  #Domestic Business
  if (REMIND_scenario == "SSP1"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_1[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP2"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_2[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP3"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_3[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP4"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_4[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else if (REMIND_scenario == "SSP5"){
    if (COVID_scenario == TRUE){
      for (j in unique(D_star$iso)) {
        for (i in unique(D_star$year[D_star$iso == j])) { 
          if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
            D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$I_B_5[ D_star$iso == j & D_star$year == i]
          }
        }
      }
    }else{}
  }else{}
  D_star[,trn_aviation_intl:= trn_aviation_intl_L + trn_aviation_intl_B+trn_aviation_intl_d_L+trn_aviation_intl_d_B, by = c("iso", "year")]
  D_star = melt(D_star, id.vars = c("iso", "year"),
                  measure.vars = c("trn_aviation_intl_L","trn_aviation_intl_B","trn_aviation_intl_d_B", "trn_aviation_intl_d_L" ,"trn_freight", "trn_pass", "trn_shipping_intl", "trn_aviation_intl"))
  D_star = D_star[,.(iso, year, demand = value, sector = variable)]
  D_star<-na.omit(D_star)
  D_star[,demand:=demand*1e-6] 
  return(D_star)

}
