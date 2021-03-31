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
  ICCT_data_I <- transform(ICCT_data, sector = "trn_aviation_intl")
return(ICCT_data_I)
}


  
DomAvPreparation <-function(tech_output_adj,input_folder,GDP_country,ICCT_dir="ICCT"){
  ICCT_data =fread(file.path(input_folder,ICCT_dir, "ICCT_data.csv"))
  GDP_POP =fread(file.path(input_folder,ICCT_dir, "GDP_POP.csv"))
  ICCT2GCAM =fread(file.path(input_folder,ICCT_dir, "ICCT2GCAM.csv"))
  GDP_country = GDP_country
  ICCT_data <- ICCT_data %>% rename(value = `domestic RPKs (billions)`)
  ICCT_data <- ICCT_data %>% rename(region = name)
  ICCT_data <-ICCT_data[,c(4,1)]
  GDP_country=GDP_country[year==2020]
  ICCT_data[, year := 2020]
  ICCT_data <- disaggregate_dt(ICCT_data, ICCT2GCAM,
                               valuecol="value",
                               datacols = "year",
                               weights=GDP_country)
  ICCT_data_D <- transform(ICCT_data, sector = "trn_aviation_intl")
  return(ICCT_data_D)
}


  