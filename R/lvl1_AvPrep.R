#' Produce ISO versions of all files.
#'
#' No conversion of units happening.
#'
#' @param input_data demand
#' @param EDGE_scenario EDGE transport scenario specifier
#' @param REMIND_scenario SSP scenario
#' @param REMIND2ISO_MAPPING regional mapping
#'
#' @author Sebastian Franz
#' 
#' 

IntAvPreparation <-function(tech_output_adj,input_folder,GDP_country,ICCT_dir="ICCT"){
  ICCT_data =fread(file.path(input_folder,ICCT_dir, "ICCT_data.csv"))
  GDP_POP =fread(file.path(input_folder,ICCT_dir, "GDP_POP.csv"))
  ICCT2GCAM =fread(file.path(input_folder,ICCT_dir, "ICCT2GCAM.csv"))
  GDP_country = GDP_country
  ICCT_data <- ICCT_data %>% rename(value = `international RPKs (billions)`)
  ICCT_data <- ICCT_data %>% rename(region = name)
  ICCT_data <-ICCT_data[,c(6,1)]
  GDP_country=GDP_country[year==2020]
  ICCT_data[, year := 2020]
  ICCT_data <- disaggregate_dt(ICCT_data, ICCT2GCAM,
                               valuecol="value",
                               datacols = "year",
                               weights=GDP_country)
  
  iso_mapping =fread(system.file("extdata", "regionmappingH12.csv", package = "edgeTransport"))
  iso_mapping <- iso_mapping %>% rename(iso = CountryCode)
  ICCT_data<-merge(ICCT_data, iso_mapping, by = c("iso"), all.x=TRUE)
  ICCT_data <- transform(ICCT_data, year = 2020)
  ICCT_data <-ICCT_data[,c(3,5)]
  ICCT_data <-ICCT_data %>% group_by(RegionCode)
  ICCT_data = summarise(ICCT_data, value = sum(value))
  ICCT_data <- transform(ICCT_data, sector = "trn_aviation_intl")
  ICCT_data <- transform(ICCT_data, year = 2020)
  return(ICCT_data)
}


  

  