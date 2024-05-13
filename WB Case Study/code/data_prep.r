#process data for country  
root<- r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\58. Article 6 United Nations\sisepuede_sri_lanke_and_tanzania\WB Case Study\)"

dir.data <- paste0(root,"scaled_results\\")
file.name <-"united_republic_of_tanzania.csv"

#load turkey data  
data <- read.csv(paste0(dir.data,file.name)) 
data <- subset(data,region=="united_republic_of_tanzania")

#emission vars only 
id_vars <-c('region','time_period',"primary_id")
vars <- subset(colnames(data),!(colnames(data)%in%id_vars))
target_vars <- subset(vars,grepl("co2e_",vars)==TRUE)
library(data.table)
data<-data.table::data.table(data)
DT.m1 = melt(data, id.vars = id_vars,
                   measure.vars = target_vars,
             )
DT.m1 <- data.frame(DT.m1)
DT.m1$variable <- as.character(DT.m1$variable)
sapply(DT.m1,class)
#add taxonomy 
#read taxonomy 

taxonomy <- read.csv(paste0(root,"simulations raw\\emission_variables_taxonomy_20240117.csv"))
#taxonomy$model_varibale <- NULL
taxonomy$X..gas_name... <- NULL 
taxonomy$model_variable_information <- NULL 
taxonomy$WB_subsector <- taxonomy$subsector

#create groups for WB visualizations 
taxonomy$WB_subsector <- gsub("Stationary Combustion and Other Energy","Buildings",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Livestock Manure Management","Livestock",taxonomy$WB_subsector)
#taxonomy$WB_subsector <- gsub("Wastewater Treatment","Waste",taxonomy$WB_subsector)
#taxonomy$WB_subsector <- gsub("Solid Waste","Waste",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Wastewater Treatment","Liquid Waste",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Solid Waste","Solid Waste",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Energy Technology","Power(electricity/heat)",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Fugitive Emissions","Power(electricity/heat)",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Soil Management","Agriculture",taxonomy$WB_subsector)
#taxonomy$WB_subsector <- gsub("Land Use","Land use and forestry(LULUCF)",taxonomy$WB_subsector)
#taxonomy$WB_subsector <- gsub("Forest","Land use and forestry(LULUCF)",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Land Use","Land use (LULUCF)",taxonomy$WB_subsector)
taxonomy$WB_subsector <- gsub("Forest","Forestry (LULUCF)",taxonomy$WB_subsector)

#unique(taxonomy$WB_subsector)
#unique(taxonomy$subsector)

#change column name to taxonomy 
taxonomy$variable <- taxonomy$field
taxonomy$field <- NULL 


#merge 
 
dim(DT.m1)
test <- merge(DT.m1,taxonomy,by="variable")
dim(test)

#
test$Year <- test$time_period + 2015 
test$time_period <- NULL 
test <- subset (test,Year>=2016)


#read attribute primary
att <- read.csv(paste0(root,"simulations raw\\ATTRIBUTE_PRIMARY.csv"))
head(att)

#merge 
dim(test)
test <- merge(test,att,by="primary_id")
dim(test)


#merge stratgy atts 
atts <- read.csv(paste0(root,"simulations raw\\ATTRIBUTE_STRATEGY.csv"))
head(atts)
#merge 
dim(test)
test <- merge(test,atts[c("strategy_id","strategy")],by="strategy_id")
dim(test)
#test <- subset(test,primary_id%in%c(119119,127127))
#test <- subset(test,primary_id%in%c(0,125125))
#test <- subset(test,primary_id%in%c(0,127127,128128))

#
test$Units <- "MtCO2e"
test$Data_Type <- "sisepuede simulation"
test$iso_code3<-"TZA"
test$Country <- "tanzania"
test$region <- NULL
test$subsector_total_field <- NULL
#test$model_variable <- NULL
test$output_type<- "emissions"



#cretae WB sector variable 
test$Sector <- "error" 
#test$Sector <- ifelse(test$WB_subsector=="Agriculture" | test$WB_subsector=="Livestock", "Agriculture", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Agriculture", "Agriculture", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Livestock", "Livestock", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Buildings", "Buildings", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="IPPU", "IPPU", test$Sector)
#test$Sector <- ifelse(test$WB_subsector=="Land use and forestry(LULUCF)", "LULUCF", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Land use (LULUCF)", "Land use-LULUCF", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Forestry (LULUCF)", "Forestry-LULUCF", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Transportation", "Transport", test$Sector)
#test$Sector <- ifelse(test$WB_subsector=="Waste", "Waste", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Liquid Waste", "Liquid Waste", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Solid Waste", "Solid Waste", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Industrial Energy"  | test$WB_subsector=="Power(electricity/heat)", "Energy", test$Sector)
test$Sector <- ifelse(test$WB_subsector=="Carbon Capture and Sequestration", "Carbon Capture and Sequestration", test$Sector)

#creqte WB subsector variable 
test$Subsector <- test$WB_subsector

test$Strategy_id<-NULL 
test$design_id<-NULL 
test$Data_Type<-NULL 
test$primary_id<-NULL 
#test$category_Value<-NULL 
#test$category_name<-NULL 
#test$WB_subsector <- NULL
test$output_type<-NULL
#test$sector<-NULL
#test$subsector<-NULL


#now aggregate test by subsector and gas 
#test<-aggregate(list(Value=test$value),list(Gas=test$gas,
#                                      Year=test$Year,
#                                      Future.ID=test$future_id,
#                                      Strategy=test$strategy,
#                                      Unit=test$Units,
#                                      Code=test$iso_code3,
#                                      Country=test$Country,
#                                      Sector=test$Sector,
#                                      WB_subsector = test$WB_subsector,
#                                      Subsector=test$Subsector),sum)

test$Notes <- ""
test$Model <- "SISEPUEDE"
test<-subset(test,Year>=2016)
#edit strategy names 
unique(test$Strategy)
#test$Strategy <- gsub("Baseline NDP","BAU Scenario",test$Strategy )
#test$Strategy <- gsub("PFLO: All transformations with partial land use reallocation","Mitigation Scenario",test$Strategy )

#now edit name of country
test$Country<-"tanzania"

#add historical comparisons 
head(test)

#can you write this  
write.csv(test, paste0(root,"Tableau\\emissions.csv"))


# work on drivers 
####
#prepare data for drivers 
###

#read simulation 
#data <- read.csv(paste0(dir.data,file.name)) 

#id_vars <-c('region','time_period',"primary_id")
#vars <- subset(colnames(data),!(colnames(data)%in%id_vars))
library(data.table)
#data<-data.table::data.table(data)
DT.m1 = melt(data, id.vars = id_vars,
                   measure.vars = vars,
             )
DT.m1 <- data.frame(DT.m1)
DT.m1$variable <- as.character(DT.m1$variable)
sapply(DT.m1,class)
#unique(DT.m1$variable)

#
#variables <- data.frame(vars=unique(DT.m1$variable))
#write.csv(variables,r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\51. WB Decarbonization Project\India_CaseStudy\new_runs\Tableau\vars.csv)", row.names=FALSE)


#now read drivers taxonomy. 
drivers <- read.csv(paste0(root,"simulations raw\\driver_variables_taxonomy_20240117.csv"))
#drivers_test <- read.csv(r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\51. WB Decarbonization Project\India_CaseStudy\new_runs\driver_variables_taxonomy_20230510_new.csv)")



#change column name to taxonomy 
drivers$variable <- drivers$field
drivers$field <- NULL 


#merge
 dim(DT.m1)
 DT.m1 <- subset(DT.m1,variable%in%unique(drivers$variable))
 dim(DT.m1)
 
#
#merge  
 dim(DT.m1)
 test2 <- merge(DT.m1,data.table(drivers),by="variable")
 dim(test2)

#
#
test2$Year <- test2$time_period + 2015 
test2$time_period <- NULL 
test2 <- subset (test2,Year>=2022)

#read attribute primary
att <- read.csv(paste0(root,"simulations raw\\ATTRIBUTE_PRIMARY.csv"))
head(att)

#merge 
dim(test2)
test2 <- merge(test2,att,by="primary_id")
dim(test2)


#merge stratgy atts 
atts <- read.csv(paste0(root,"simulations raw\\ATTRIBUTE_STRATEGY.csv"))
head(atts)
#merge 
dim(test2)
test2 <- merge(test2,atts[c("strategy_id","strategy")],by="strategy_id")
dim(test2)
#test2 <- subset(test2,primary_id%in%c(119119,127127))
#test2 <- subset(test2,primary_id%in%c(0,127127,128128))


test2$Units <- "NA"
test2$Data_Type <- "sisepuede simulation"
test2$iso_code3<-"TZA"
test2$Country <- "tanzania"
test2$region <- NULL
test2$subsector_total_field <- NULL
#test2$model_variable <- NULL
test2$gas <- NA  

test2$model_variable_information <- NULL
test2$output_type<- "drivers"

#create an additional sector variable for energy  
energy_vars <- data.frame(variable=subset(unique(test2$variable),grepl("energy",unique(test2$variable))==TRUE ))
energy_vars$energy_subsector <-"TBD"
energy_vars$energy_subsector <- ifelse(grepl("ccsq",energy_vars$variable)==TRUE,"Carbon Capture and Sequestration",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("inen",energy_vars$variable)==TRUE,"Industrial Energy",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("entc",energy_vars$variable)==TRUE,"Power(electricity/heat)",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("trns",energy_vars$variable)==TRUE,"Transportation",energy_vars$energy_subsector )
energy_vars$energy_subsector <- ifelse(grepl("scoe",energy_vars$variable)==TRUE,"Buildings",energy_vars$energy_subsector )

#merge energy vars with test2 
dim(test2)
test2 <- merge(test2,energy_vars,by="variable", all.x=TRUE)
dim(test2)




#
#test2$WB_subsector <- test2$energy_subsector
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="Agriculture", "Agriculture",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="IPPU", "IPPU",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="Livestock", "Livestock",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="Land Use", "Land use and forestry(LULUCF)",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="Solid Waste", "Waste",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="Transportation Demand", "Transportation",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$subsector=="Soil Management" , "Agriculture",test2$WB_subsector)
#test2$WB_subsector <- ifelse(is.na(test2$WB_subsector)==TRUE & test2$sector=="Socioeconomic" , "Socioeconomic",test2$WB_subsector)
#test2$energy_subsector <-NULL

#remove issues 
#test2$value <- ifelse(test2$category_value=="fuel_nuclear",0,test2$value)

#make adjustments 
#test2$category_value <- ifelse(test2$category_value=="cereals","rice_X",test2$category_value)
#test2$category_value <- ifelse(test2$category_value=="vegetables_and_vines","cereals_X",test2$category_value)
#test2$category_value <- ifelse(test2$category_value=="rice","vegetables_and_vines_X",test2$category_value)

#make final arrangement  
#test2$category_value <-gsub("_X","",test2$category_value)

#head(test2)

#unique(test2$category_value)
#unique(test2$category_name)

#write
write.csv(test2,paste0(root,"Tableau\\drivers.csv"), row.names=FALSE)


