root <- r"(C:\Users\edmun\OneDrive\Edmundo-ITESM\3.Proyectos\58. Article 6 United Nations\sisepuede_sri_lanke_and_tanzania\scaled_results\)"
files <- list.files(paste0(root,"Nations\\"),pattern =".csv") 
data <- list()
for (i  in 1:length(files))
{
 pivot <- read.csv(paste0(root,"Nations\\",files[i]))
 pivot$X<-NULL
 data <- append(data,list(pivot))
}
data <- do.call("rbind",data)
tv1_all <- subset(colnames(data),grepl("co2e_",colnames(data))==TRUE)
data$emission_co2e_TOTAL <- rowSums(data[,tv1_all])

#write results 
write.csv(data,paste0(root,"sisepuede_results_WIDE_scaled.csv"),row.names=FALSE)

