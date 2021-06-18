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


lvl2_demandReg <- function(tech_output, price_baseline, GDP_POP, REMIND_scenario, smartlifestyle,ICCT_data_I,ICCT_data_D,GDP_country,POP_country,REMIND2ISO_MAPPING_adj,REMIND2ISO_MAPPING,COVID_dir="COVID",COVID_scenario){
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
    decay_RPK=0.6
    decay_GDP=0.75
    decay_threshold= 1500
  }else if (REMIND_scenario == "SSP2") {
    decay_RPK=0.7
    decay_GDP=0.85
    decay_threshold= 1500
  }else if (REMIND_scenario == "SSP3") {
    decay_RPK=1
    decay_GDP=1
    decay_threshold= 1
  }else if (REMIND_scenario == "SSP4") {
    decay_RPK=1
    decay_GDP=1
    decay_threshold= 1
  }else if (REMIND_scenario == "SSP5") {
    decay_RPK=0.8
    decay_GDP=0.95
    decay_threshold= 1500
  }else{}
  
  #GDP Threshold
  #Decay of Income Elasticity
  #International
  #Leisure
  if (REMIND_scenario == "SSP1") {
    GDP_threshold= 30000
  }else if (REMIND_scenario == "SSP2") {
    GDP_threshold= 30000
  }else if (REMIND_scenario == "SSP3") {
    GDP_threshold= 30000
  }else if (REMIND_scenario == "SSP4") {
    GDP_threshold= 30000
  }else if (REMIND_scenario == "SSP5") {
    GDP_threshold= 30000
  }else{}
  

  #Domestic Derivation
  if (REMIND_scenario == "SSP1") {
    floor_domestic_leisure = 0.5
    floor_domestic_business = 0.4
    top_domestic_leisure = 1
    top_domestic_business = 1
    special_country_floor_leisure = 0.6
    special_country_floor_business = 0.55
  }else if (REMIND_scenario == "SSP2") {
    floor_domestic_leisure = 0.5
    floor_domestic_business = 0.4
    top_domestic_leisure = 1
    top_domestic_business = 1
    special_country_floor_leisure = 0.6
    special_country_floor_business = 0.55
  }else if (REMIND_scenario == "SSP3") {
    floor_domestic_leisure = 0.4
    floor_domestic_business = 0.3
    top_domestic_leisure = 1
    top_domestic_business = 1
    special_country_floor_leisure = 0.5
    special_country_floor_business = 0.45
  }else if (REMIND_scenario == "SSP4") {
    floor_domestic_leisure = 0.4
    floor_domestic_business = 0.3
    top_domestic_leisure = 1
    top_domestic_business = 1
    special_country_floor_leisure = 0.5
    special_country_floor_business = 0.45
  }else if (REMIND_scenario == "SSP5") {
    floor_domestic_leisure = 0.5
    floor_domestic_business = 0.4
    top_domestic_leisure = 1
    top_domestic_business = 1
    special_country_floor_leisure = 0.6
    special_country_floor_business = 0.55
  }else{}
  ## Create a dt with GDP, POP and GDP_cap with EDGE isos
  gdp_pop = copy(GDP_POP)
  setnames(gdp_pop, old = "weight", new = "GDP_val")
  #disaggreagte  data
  gdp_pop_c <- disaggregate_dt(gdp_pop, REMIND2ISO_MAPPING,
                               valuecol="POP_val",
                               datacols = "year",
                               weights=GDP_country)
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
                               weights=POP_country)
  gdp_pop<-merge(gdp_pop_a[,c(1,2,5)], gdp_pop_b[,c(1,2,3)])
  gdp_pop<-merge(gdp_pop, gdp_pop_c[,c(1,2,4)])
  gdp_pop[, GDP_cap := ifelse( year >= 1965 , GDP_val/POP_val , GDP_cap) ]
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
  
 #PRICE
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
  #calculate the indexes raised to the corresponding elasticities
  price_baseline[,`:=`(index_price_p_lo=trn_aviation_intl^1)]
  
  #HISTORICAL DEMAND
  #calculate demand at a sector level
  demand_tot_sector=tech_output[, .(demand_tot=sum(tech_output)), by=c("region", "year", "sector")]
  demand_tot_sector <- disaggregate_dt(demand_tot_sector, REMIND2ISO_MAPPING,
                                       valuecol="demand_tot",
                                       datacols = "year",
                                       weights=GDP_country)

  #calculate 2020 demand for aviation industry based on ICCT Data
  demand_tot_sector_avi<- demand_tot_sector[demand_tot_sector$sector %like% "trn_aviation_intl"]
  demand_tot_sector_avi[, demand_tot := ifelse( year == 2005 ,1 , demand_tot) ]
  demand_tot_sector_avi[, demand_tot := ifelse( year == 2010 ,1 , demand_tot) ]
  demand_tot_sector_avi[, demand_tot := ifelse( year == 2015 ,1 , demand_tot) ]
  ## interpolate
  demand_tot_sector_avi=approx_dt(dt = demand_tot_sector_avi, ## database to interpolate
                                  xdata = seq(1990,2020,1), ## time steps on which to interpolate
                                  ycol = "demand_tot", ## column containing the data to interpolate
                                  xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                                  idxcols=c("iso", "sector"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                                  extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  demand_tot_sector=approx_dt(dt = demand_tot_sector, ## database to interpolate
                              xdata = seq(1990,2020,1), ## time steps on which to interpolate
                              ycol = "demand_tot", ## column containing the data to interpolate
                              xcol="year", ## x-axis of the interpolation, i.e. the years that you indeed have available
                              idxcols=c("iso", "sector"), ## equivalent of "group_by" in dplyr and "by=.(....)" in data.table
                              extrapolate = T) ## extrapolate? i.e. min(xdata)<min(unique(dat$year))|max(xdata)>max(unique(dat$year))
  demand_tot_sector=dcast(demand_tot_sector, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  demand_tot_sector_avi=dcast(demand_tot_sector_avi, iso + year  ~ sector, value.var = "demand_tot", fun.aggregate = sum, margins="sector")
  #JADC historical projections
  demand_tot_sector_avi[, trn_aviation_intl := ifelse( year == 2001 ,trn_aviation_intl*12 , trn_aviation_intl) ]
  demand_tot_sector_avi[, trn_aviation_intl := ifelse( year >= 2002 ,1 , trn_aviation_intl) ]
  #Prepare international aviation data (data from: https://www.iata.org/contentassets/a686ff624550453e8bf0c9b3f7f0ab26/wats-2020-mediakit.pdf)
  #include IATA historical data
  IATA_data<-demand_tot_sector_avi
  IATA_data<- IATA_data[IATA_data$iso %like% "DEU"]
  IATA_data<-IATA_data[,c(2)]
  IATA_data <- transform( IATA_data, share = 1)
  IATA_data[, share := ifelse( year == 2000 , 1 , share) ]
  IATA_data[, share := ifelse( year == 2000 , 1 , share) ]
  IATA_data[, share := ifelse( year == 2001 , 0.98 , share) ]
  IATA_data[, share := ifelse( year == 2002 , 1.02, share) ]
  IATA_data[, share := ifelse( year == 2003 , 1.002 , share) ]
  IATA_data[, share := ifelse( year == 2004 , 1.18 , share) ]
  IATA_data[, share := ifelse( year == 2005 , 1.09 , share) ]
  IATA_data[, share := ifelse( year == 2006 , 1.08 , share) ]
  IATA_data[, share := ifelse( year == 2007 , 1.08 , share) ]
  IATA_data[, share := ifelse( year == 2008 , 1.04 , share) ]
  IATA_data[, share := ifelse( year == 2009 , 0.98 , share) ]
  IATA_data[, share := ifelse( year == 2010 , 1.08, share) ]
  IATA_data[, share := ifelse( year == 2011 , 1.07 , share) ]
  IATA_data[, share := ifelse( year == 2012 , 1.08 , share) ]
  IATA_data[, share := ifelse( year == 2013 , 1.07 , share) ]
  IATA_data[, share := ifelse( year == 2014 , 1.08 , share) ]
  IATA_data[, share := ifelse( year == 2015 , 1.07 , share) ]
  IATA_data[, share := ifelse( year == 2016 , 1.07 , share) ]
  IATA_data[, share := ifelse( year == 2017 , 1.08 , share) ]
  IATA_data[, share := ifelse( year == 2018 , 1.09 , share) ]
  IATA_data[, share := ifelse( year == 2019 , 1.06 , share) ]
  IATA_data[, share := ifelse( year == 2020 , 1.08 , share) ]
  demand_tot_sector_avi=merge(demand_tot_sector_avi,IATA_data, by = c("year"),all.x = TRUE)
    for (j in unique(demand_tot_sector_avi$iso)) {
      for(i in unique(demand_tot_sector_avi$year)[unique(demand_tot_sector_avi$year)>2001]) { 
        demand_tot_sector_avi[iso == j, trn_aviation_intl := ifelse(year == i, trn_aviation_intl[year == i-1]*share, trn_aviation_intl)]
      }
    }
  saveRDS(demand_tot_sector_avi, file = "historical_data.rds")
  demand_tot_sector_avi<-demand_tot_sector_avi[,c(1,2,3)]
  setnames(ICCT_data_I,"value", "trn_aviation_intl")
  ICCT_data_I<-ICCT_data_I[,c(1,2,3)]
  ICCT_data_I[, year := ifelse( year == 2020 , 2019 , year) ]
  demand_tot_sector_avi[, trn_aviation_intl := ifelse( year == 2019 , NA , trn_aviation_intl) ]
  demand_tot_sector_avi <- rbind(demand_tot_sector_avi,ICCT_data_I)
  demand_tot_sector_avi<-na.omit(demand_tot_sector_avi)
  #from long to wide format, so that the df has separate columns for all transport modes
  #merge all data for demand regression
  IE = 1.5
  Aviation_data<-merge(gdp_pop[,c(1,2,3,5,7,8)],demand_tot_sector_avi, by = c("iso","year"),all.x = TRUE)
  Aviation_data<-merge(Aviation_data,price_baseline[,c(1,2,7)], by = c("iso","year"),all.x = TRUE)
  Aviation_data <- transform(Aviation_data, income_elasticity_pass_lo = IE)
  Aviation_data <- transform(Aviation_data, decay_rate_RPK= 1)
  Aviation_data <- transform(Aviation_data, decay_rate_GDP= 1)
  #region specific adjustments
  iso_mapping<-fread(file.path("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/regionmappingH12.csv"), sep=";", header = T)
  Aviation_data<-merge(Aviation_data,iso_mapping, by = c("iso"),all.x = TRUE)
  Aviation_data[, income_elasticity_pass_lo := ifelse(iso_region=="OAS" , IE*0.5 , income_elasticity_pass_lo) ]
  Aviation_data[, income_elasticity_pass_lo := ifelse(iso_region=="SSA" , IE*0.5 , income_elasticity_pass_lo) ]
  Aviation_data<-Aviation_data[,c("iso_region","name"):= NULL]
  Aviation_data[, trn_aviation_intl := ifelse( year == 2019 & iso=="BOL" , 1534.48687 , trn_aviation_intl) ]
  Aviation_data[, trn_aviation_intl := ifelse( year == 2019 & iso=="IRN" , 95905.49 , trn_aviation_intl) ]
  Aviation_data[, trn_aviation_intl := ifelse( year == 2019 & iso=="VEN" , 13363.0493, trn_aviation_intl) ]
  Aviation_data <- transform(Aviation_data, RPK_Cap = trn_aviation_intl/ POP_val)
  Aviation_data <- subset(Aviation_data, Aviation_data$year <=2100 & Aviation_data$year >=1990)
  saveRDS(Aviation_data, file = "Aviation_data.rds")
  # usa to fast
  # per capita data
  # price path assumption
  # y-axis= RPK captia, x-axis= GDP capita
  #General reduction for SSP1&SSP2
  #additive trigger # low thresholds
  for (j in unique(Aviation_data$iso)) {
    for (i in unique(Aviation_data$year)[unique(Aviation_data$year)>2019]) { 
      #RPK-Threshold
      if(isTRUE(any(Aviation_data[iso == j & year %in% seq(2005, i-1), RPK_Cap] > decay_threshold))) { 
        Aviation_data[iso == j, decay_rate_RPK := ifelse(year == i, decay_rate_RPK[year == i-1] * decay_RPK, decay_rate_RPK)]
      }
      if (isTRUE(any(Aviation_data[iso == j & year %in% seq(2005, i-1), GDP_cap] > GDP_threshold))) { 
        Aviation_data[iso == j, decay_rate_GDP := ifelse(year == i, decay_rate_GDP[year == i-1] * decay_GDP, decay_rate_GDP)]
        Aviation_data[iso == j, income_elasticity_pass_lo := ifelse(year == i, income_elasticity_pass_lo[year == i-1] * decay_rate_GDP*decay_rate_RPK, income_elasticity_pass_lo)]
      }
      if(isTRUE(any(Aviation_data[iso == j & year %in% seq(2005, i-1), RPK_Cap] > decay_threshold)) & !isTRUE((any(Aviation_data[iso == j & year %in% seq(2005, i-1), GDP_cap] > GDP_threshold)))) { 
        Aviation_data[iso == j, income_elasticity_pass_lo := ifelse(year == i, income_elasticity_pass_lo[year == i-1] * decay_rate_RPK, income_elasticity_pass_lo)]
      }
      #create GDP index with IE
      Aviation_data[, index_GDPcap_p_lo :=index_GDPcap^income_elasticity_pass_lo]
      #Prepare Regression
      Aviation_data[, D_star_p_lo := index_price_p_lo*index_GDPcap_p_lo*index_POP]
      
      #demand regression
      Aviation_data[iso == j, trn_aviation_intl := ifelse(year == i, trn_aviation_intl[year == i-1]*D_star_p_lo, trn_aviation_intl)]
      #update RPK CAP
      Aviation_data[iso == j & year == i, RPK_Cap := trn_aviation_intl / POP_val]
    }
  }
  saveRDS(Aviation_data, file = "Aviation_data_post_loop.rds")
  elasticities<-Aviation_data[,c("iso","year","income_elasticity_pass_lo")]
  #Split international aviation in business and leisure based on a survey IPSOS, 2017
  Aviation_data <- transform( Aviation_data, trn_aviation_intl_L = trn_aviation_intl * 0.625)
  Aviation_data <- transform( Aviation_data, trn_aviation_intl_B = trn_aviation_intl * 0.375)
  Aviation_data <- Aviation_data[,c("iso", "year", "trn_aviation_intl", "trn_aviation_intl_L", "trn_aviation_intl_B")]
  D_star<-Aviation_data
  ## select only the columns that contains the demand
  #domestic Derivation
  ############################
  D_star <- transform( D_star, trn_aviation_intl_d_B = trn_aviation_intl_B)
  D_star <- transform( D_star, trn_aviation_intl_d_L = trn_aviation_intl_L)
  ICCT_data_D = merge(ICCT_data_D,ICCT_data_I, by=c("iso"),all.x = TRUE)
  ICCT_data_D <- transform( ICCT_data_D, share =value/ trn_aviation_intl)
  D_star<-merge(D_star,ICCT_data_D[,c("iso","share")] ,by=c("iso"), all.x = TRUE)
  #loop to replace international values with ICCT domestic data
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] > 2000) { 
        D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i]* D_star$share[D_star$iso == j & D_star$year == i]
      }
    }
  }
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] > 2000) { 
        D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i]* D_star$share[D_star$iso == j & D_star$year == i]
      }
    }
  }
  D_star<-D_star[,c(1:7)]
  ## interpolate
  Domestic_shares =fread(system.file("extdata", "Domestic_shares.csv", package = "edgeTransport"))
  Domestic_shares[, shares_d_L := ifelse( year == 2100 ,floor_domestic_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2100 , floor_domestic_business, shares_d_B) ]
  Domestic_shares[, shares_d_L := ifelse( year == 2019 ,top_domestic_leisure , shares_d_L) ]
  Domestic_shares[, shares_d_B := ifelse( year == 2019 , top_domestic_business, shares_d_B) ]
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
      if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
        D_star$trn_aviation_intl_d_L[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_L[ D_star$iso == j & D_star$year == i] * D_star$shares_d_L[ D_star$iso == j & D_star$year == i]
      }
    }
  }
  #domestic business
  for (j in unique(D_star$iso)) {
    for (i in unique(D_star$year[D_star$iso == j])) { 
      if (D_star$year[D_star$iso == j & D_star$year == i] > 2019) { 
        D_star$trn_aviation_intl_d_B[D_star$iso == j & D_star$year == i] <- D_star$trn_aviation_intl_d_B[ D_star$iso == j & D_star$year == i] * D_star$shares_d_B[ D_star$iso == j & D_star$year == i]
      }
    }
  }
  D_star<-D_star[,c("iso","year","trn_aviation_intl_L","trn_aviation_intl_B","trn_aviation_intl_d_L","trn_aviation_intl_d_B")]
  #COVID ADJUSTMENT
  ## COVID ADJUSTMENT - Totally exogenous COVID shock based on certain assumptions and actual COVID-impact data from the year 2020
  if (COVID_scenario == TRUE) {
  COVID_shock = fread(system.file("extdata", "COVID_ext.csv", package = "edgeTransport"))
  D_star=merge(D_star, COVID_shock,by = c("iso","year"), all.x = TRUE)
  ## find values to be used depending on the SSP number
  coeff_touse_L = paste0("I_L_", gsub("[^\\d]+", "", REMIND_scenario, perl=TRUE))
  coeff_touse_B = paste0("I_B_", gsub("[^\\d]+", "", REMIND_scenario, perl=TRUE))
  coeff_touse_d_L = paste0("D_L_", gsub("[^\\d]+", "", REMIND_scenario, perl=TRUE))
  coeff_touse_d_B = paste0("D_B_", gsub("[^\\d]+", "", REMIND_scenario, perl=TRUE))
  D_star<-na.omit(D_star)
  
  D_star[year > 2019, trn_aviation_intl_L :=  trn_aviation_intl_L*get(coeff_touse_L), by = c("iso", "year")]
  D_star[year > 2019, trn_aviation_intl_B :=  trn_aviation_intl_B*get(coeff_touse_B), by = c("iso", "year")]
  D_star[year > 2019, trn_aviation_intl_d_L :=  trn_aviation_intl_d_L*get(coeff_touse_d_L), by = c("iso", "year")]
  D_star[year > 2019, trn_aviation_intl_d_B :=  trn_aviation_intl_d_B*get(coeff_touse_d_B), by = c("iso", "year")]
  
  D_star<-D_star[,c("iso","year","trn_aviation_intl_L","trn_aviation_intl_B","trn_aviation_intl_d_L","trn_aviation_intl_d_B")]
  } else{}
  
  D_star[,trn_aviation_intl:= trn_aviation_intl_L + trn_aviation_intl_B+trn_aviation_intl_d_L+trn_aviation_intl_d_B, by = c("iso", "year")]
  D_star = melt(D_star, id.vars = c("iso", "year"),
                measure.vars = c("trn_aviation_intl_L","trn_aviation_intl_B","trn_aviation_intl_d_B", "trn_aviation_intl_d_L" , "trn_aviation_intl"))
  D_star = D_star[,.(iso, year, demand = value, sector = variable)]
  D_star<-na.omit(D_star)
  D_star[,demand:=demand*1e-6] 
  #report parameters
  Threshold_data = fread(file.path("C:/Users/franz/Documents/R/Master-Thesis/EDGE-T/Export Data/Threshold_data.csv"), sep=";", header = T)
  #IE
  if (REMIND_scenario == "SSP1") {
    Threshold_data$SSP1 <- ifelse(Threshold_data$data == "IE",IE, Threshold_data$SSP1)
  }else if (REMIND_scenario == "SSP2") {
    Threshold_data$SSP2 <- ifelse(Threshold_data$data == "IE",IE, Threshold_data$SSP2)
  }else if (REMIND_scenario == "SSP5") {
    Threshold_data$SSP5 <- ifelse(Threshold_data$data == "IE",IE, Threshold_data$SSP5)
  }else{}
  #decay_rate_RPK
  if (REMIND_scenario == "SSP1") {
    Threshold_data$SSP1 <- ifelse(Threshold_data$data == "decay_rate_RPK",decay_RPK, Threshold_data$SSP1)
  }else if (REMIND_scenario == "SSP2") {
    Threshold_data$SSP2 <- ifelse(Threshold_data$data == "decay_rate_RPK",decay_RPK, Threshold_data$SSP2)
  }else if (REMIND_scenario == "SSP5") {
    Threshold_data$SSP5 <- ifelse(Threshold_data$data == "decay_rate_RPK",decay_RPK, Threshold_data$SSP5)
  }else{}
  #decay_rate_GDP
  if (REMIND_scenario == "SSP1") {
    Threshold_data$SSP1 <- ifelse(Threshold_data$data == "decay_rate_GDP",decay_GDP, Threshold_data$SSP1)
  }else if (REMIND_scenario == "SSP2") {
    Threshold_data$SSP2 <- ifelse(Threshold_data$data == "decay_rate_GDP",decay_GDP, Threshold_data$SSP2)
  }else if (REMIND_scenario == "SSP5") {
    Threshold_data$SSP5 <- ifelse(Threshold_data$data == "decay_rate_GDP",decay_GDP, Threshold_data$SSP5)
  }else{}
  #RPK_threshold
  if (REMIND_scenario == "SSP1") {
    Threshold_data$SSP1 <- ifelse(Threshold_data$data == "RPK_threshold",decay_threshold, Threshold_data$SSP1)
  }else if (REMIND_scenario == "SSP2") {
    Threshold_data$SSP2 <- ifelse(Threshold_data$data == "RPK_threshold",decay_threshold, Threshold_data$SSP2)
  }else if (REMIND_scenario == "SSP5") {
    Threshold_data$SSP5 <- ifelse(Threshold_data$data == "RPK_threshold",decay_threshold, Threshold_data$SSP5)
  }else{}
  #GDP-threshold
  if (REMIND_scenario == "SSP1") {
    Threshold_data$SSP1 <- ifelse(Threshold_data$data == "GDP_threshold",GDP_threshold, Threshold_data$SSP1)
  }else if (REMIND_scenario == "SSP2") {
    Threshold_data$SSP2 <- ifelse(Threshold_data$data == "GDP_threshold",GDP_threshold, Threshold_data$SSP2)
  }else if (REMIND_scenario == "SSP5") {
    Threshold_data$SSP5 <- ifelse(Threshold_data$data == "GDP_threshold",GDP_threshold, Threshold_data$SSP5) 
  }else{}
  Demand_Regression_Data = list(D_star = D_star,
                   elasticities = elasticities,
                   Threshold_data = Threshold_data )

  return(Demand_Regression_Data)
  
}
