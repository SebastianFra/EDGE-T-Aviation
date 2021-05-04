#' Loads and prepared historical international aviation demand data
#'
#'
#' @param input_folder input folder
#' @param GDP_country GDP_country data
#' @param ICCT_dir Directory of the ICCT data
#'
#' @author Sebastian Franz
#' 
#' 


lvl0_IntAvPreparation <-function(tech_output_adj,input_folder,GDP_country,ICCT_dir="ICCT"){
  ICCT_data =fread(file.path(input_folder,ICCT_dir, "ICCT_data.csv"))
  GDP_POP =fread(file.path(input_folder,ICCT_dir, "GDP_POP.csv"))
  ICCT2GCAM =fread(file.path(input_folder,ICCT_dir, "ICCT2GCAM.csv"))
  GDP_country = GDP_country
  setnames(ICCT_data,"international RPKs (billions)", "value")
  setnames(ICCT_data,"name", "region")
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



lvl0_DomAvPreparation <-function(tech_output_adj,input_folder,GDP_country,ICCT_dir="ICCT"){
  ICCT_data =fread(file.path(input_folder,ICCT_dir, "ICCT_data.csv"))
  GDP_POP =fread(file.path(input_folder,ICCT_dir, "GDP_POP.csv"))
  ICCT2GCAM =fread(file.path(input_folder,ICCT_dir, "ICCT2GCAM.csv"))
  GDP_country = GDP_country
  setnames(ICCT_data,"domestic RPKs (billions)", "value")
  setnames(ICCT_data,"name", "region")
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


  

  