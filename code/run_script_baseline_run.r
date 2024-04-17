library(data.table)
library(data.table)
#regions 

#Set root directory
root<- r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\58. Article 6 United Nations\sisepuede_sri_lanke_and_tanzania\)"

#load emissions targets 
te_all<-read.csv(paste0(root,"\\code\\target_emissions.csv"))

#subset target emissions to preferred target
te_all <- subset(te_all, iso_code3%in%c("LKA","TZA"))
te_all$source <- NULL
te_all$Year <- NULL

#ouputfile
output.file<-"sisepuede_results_sisepuede_run_2024-04-15.csv"
data_all<-read.csv(paste0(root,"\\simulations raw\\",output.file))
rall <- unique(data_all$region)

id_vars <-c('region','time_period',"primary_id")
vars <- subset(colnames(data_all),!(colnames(data_all)%in%id_vars))
target_vars <- subset(vars,grepl("co2e_",vars)==TRUE)


#definte subsector totals
sector_totals <-c("emission_co2e_subsector_total_agrc", 
                                                "emission_co2e_subsector_total_ccsq",
                                                "emission_co2e_subsector_total_entc",
                                                "emission_co2e_subsector_total_fgtv",
                                                "emission_co2e_subsector_total_frst",
                                                "emission_co2e_subsector_total_inen",
                                                "emission_co2e_subsector_total_ippu",
                                                "emission_co2e_subsector_total_lndu",
                                                "emission_co2e_subsector_total_lsmm",
                                                "emission_co2e_subsector_total_lvst",
                                                "emission_co2e_subsector_total_scoe",
                                                "emission_co2e_subsector_total_soil",
                                                "emission_co2e_subsector_total_trns",
                                                "emission_co2e_subsector_total_trww",
                                                "emission_co2e_subsector_total_waso")

#
#ids <- c("primary_id","region","time_period")
#data_all[,c(ids,sector_totals)] <- NULL
#data_all <- data_all[,c(ids,sector_totals)] 
#read emissions attributes 
taxonomy <- read.csv(paste0(root,"\\simulations raw\\","emission_variables_taxonomy_20240117.csv"))
emissions_vars<-unique(taxonomy$field)
redundant_emission_vars<- subset(target_vars,!(target_vars%in%emissions_vars))
redundant_emission_vars <- subset(redundant_emission_vars,grepl("conversion_away_",redundant_emission_vars)==FALSE)


data_all[,sector_totals] <- NULL
data_all[,redundant_emission_vars] <- NULL


#source rescale function 
#source(paste0(root,"rescale_function_baseline.r"))

dir.output <- r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\58. Article 6 United Nations\sisepuede_sri_lanke_and_tanzania\scaled_results\Nations\)" 
initial_conditions_id <- "_15015"

source(paste0(root,"\\code\\","rescale_function_baseline.r"))

z<-2
rescale(z,rall,data_all,te_all,initial_conditions_id,dir.output)    

