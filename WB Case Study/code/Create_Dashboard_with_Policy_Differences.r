
dir.data <- r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\58. Article 6 United Nations\sisepuede_sri_lanke_and_tanzania\WB Case Study\simulations raw\)"

simulation.file <- "sisepuede_results_sisepuede_run_2024-05-06.csv"
fields.file <- "sisepuede_input_fields.csv"

#read data  
data <- read.csv(paste0(dir.data,simulation.file))
fields <- read.csv(paste0(dir.data,fields.file))$fields_input
length(fields)
#keep only inputs in data  
fields <- subset(fields,fields%in%colnames(data)) 
length(fields)

#subset data  
identifiers<- c("primary_id","region","time_period")
data <- data[,c(identifiers,fields)]
dim(data)
data <- subset(data,region=="united_republic_of_tanzania")
dim(data)

#subset to runs of interest 
target_ids <- c(15015,120120,123123,128128)
dim(data)
data <- subset(data,primary_id%in%target_ids)
dim(data)
ids <- unique(data$primary_id)
#now for each primary id, generate percent changes 
model_table <- list()
for (i in 1:length(ids)) 
  {
#  i <- 1
  itable <- subset(data,primary_id==ids[i])[,c("time_period",fields)] 
  itable <- subset(itable, time_period%in%c(0,35))
  itable_add<-data.frame(t(apply(itable[,fields],2, function(x) {diff(as.numeric(x))})))
  itable <- subset(itable,time_period==min(unique(itable$time_period)))
  itable$time_period <- NULL
  itable <- rbind(itable,itable_add)
  itable <- itable[2,]/itable[1,]
  rm(itable_add)
  itable$primary_id <- ids[i]
  model_table <- append(model_table, list(itable))
  }
 model_table <- do.call("rbind",model_table)

#first remove NAs 
keeps <-apply(model_table[,fields],c(1,2),function(x){ifelse(is.na(x)==TRUE,1,0)})  
keeps <- apply(keeps,2,sum)
keeps <- names(subset(keeps,keeps<max(keeps)))

#second remove Inf
keeps_p2 <-apply(model_table[,keeps],c(1,2),function(x){ifelse(is.infinite(x)==TRUE,1,0)})  
keeps_p2 <- apply(keeps_p2,2,sum)
keeps_p2 <- names(subset(keeps_p2,keeps_p2<max(keeps_p2)))

#remove now variables that do not change 
 keeps_p3 <- sapply(model_table[,keeps_p2],var)
 keeps_p3 <- subset(keeps_p3,is.na(keeps_p3)==FALSE)
 keeps_p3 <- names(subset(keeps_p3,keeps_p3>0.001))
#now we can have a table comparing the behavior of all input values,
 model_table <- model_table[,c("primary_id",keeps_p3)] 

#change format to long and merge with fields taxonomy 
taxonomy.file <- "driver_variables_taxonomy_20240117.csv"
taxonomy <- read.csv (paste0(dir.data,taxonomy.file))

#melt input file  
library(data.table)
model_table <- data.table(model_table)
model_table <- melt(model_table,id=c("primary_id"))
model_table$field <- model_table$variable
model_table$variable <- NULL  
head(model_table)

#merge 
dim(model_table)
dim(taxonomy)
model_table <- merge(model_table, data.table(taxonomy), by="field")
dim(model_table)

#merge primary_id with attribute strategy and strategy ids  
att_table <- read.csv (paste0(dir.data,"ATTRIBUTE_PRIMARY.csv"))
dim(model_table)
dim(att_table)
model_table <- merge(model_table, data.table(att_table), by="primary_id")
dim(model_table)

strg_table <- read.csv (paste0(dir.data,"ATTRIBUTE_STRATEGY.csv"))
dim(model_table)
dim(strg_table)
model_table <- merge(model_table, data.table(strg_table), by="strategy_id")
dim(model_table)

write.csv(model_table,paste0(r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\58. Article 6 United Nations\sisepuede_sri_lanke_and_tanzania\WB Case Study\Tableau\)","inputs_dashboard.csv"))

#