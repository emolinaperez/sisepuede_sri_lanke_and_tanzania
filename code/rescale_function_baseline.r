rescale <-function(z,rall,data_all,te_all,initial_conditions_id,dir.output)
{
#z<-1
tregion<-rall[z]

#subset data and targets 
data <- subset(data_all, region==tregion)
te<-subset(te_all,region==tregion)

#emissions total
tv1_all <- subset(colnames(data),grepl("co2e_",colnames(data))==TRUE)

#first determine percent change across time  
data$Index <- paste0(data$region,"_",data$primary_id)
inds<-unique(data$Index)

#this has to be done for every single  var 
pct_diffs<- list()
for (i in 1:length(inds))
{
#i<-2
step1<-list()
for (j in 1:length(tv1_all))
{ 
#j<-268
pivot <- data[data$Index==inds[i],c("Index","time_period",tv1_all[j])]
#remove single cell NAs
pivot[,tv1_all[j]] <- ifelse(is.na(pivot[,tv1_all[j]])==TRUE,mean(pivot[,tv1_all[j]],na.rm=TRUE),pivot[,tv1_all[j]])
#remove full variables NAs
if (is.na(mean(pivot[,tv1_all[j]]))==TRUE) {
 pivot[,tv1_all[j]] <- 0} else {pivot[,tv1_all[j]] <- pivot[,tv1_all[j]]  }

if (mean(unique(pivot[,tv1_all[j]]))==0) {
pivot[,paste0("pct_diff_",tv1_all[j])] <- 0
} else {
pivot$diff <- c(diff(pivot[,tv1_all[j]]),0)
pivot[,paste0("pct_diff_",tv1_all[j])] <- c(0,pivot$diff[1:(nrow(pivot)-1)]/pivot[,tv1_all[j]][1:(nrow(pivot)-1)])
pivot[,paste0("pct_diff_",tv1_all[j])] <- ifelse(is.na(pivot[,paste0("pct_diff_",tv1_all[j])])==TRUE,0,pivot[,paste0("pct_diff_",tv1_all[j])])
pivot[,paste0("pct_diff_",tv1_all[j])] <- ifelse(pivot[,paste0("pct_diff_",tv1_all[j])]==Inf,0,pivot[,paste0("pct_diff_",tv1_all[j])])
}
pivot[,paste0("diff_",tv1_all[j])] <-c(0,diff(pivot[,tv1_all[j]]))
pivot <- pivot[,c("Index","time_period",paste0("pct_diff_",tv1_all[j]),paste0("diff_",tv1_all[j]))]
step1<-append(step1,list(pivot))
}
step1 <- Reduce(function(...) merge(...,), step1) 
pct_diffs<- append(pct_diffs,list(step1))
}
pct_diffs <- do.call("rbind",pct_diffs)
pct_diffs <- pct_diffs[order(pct_diffs$Index,pct_diffs$time_period),]
dim(pct_diffs)
dim(data)
#check percent differences 
#now scale initial conditions for gases, sector by sector 

#================================================================================================================================================
#circular economy  
#================================================================================================================================================
#create vectors for co2e and ch4 vars 
tv1<-subset(tv1_all,grepl("trww",tv1_all)==TRUE | grepl("waso",tv1_all)==TRUE | grepl("wali",tv1_all)==TRUE)
ch4vars <- subset(tv1,grepl("_ch4",tv1)==TRUE)

#set emissions totals targets 
target_CO2total_ce <- as.numeric(te$target_CO2total_ce) 
uncalibrated_CO2total <- sum(data [data$time_period==0 & data$Index==inds[1],tv1] )

#set CH4 targets 
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] )
target_CH4total_ce <- as.numeric(te$target_CO2total_ce)*as.numeric(te$target_CH4total_ce)

#first level: CH4 emissions,
data [data$time_period==0,ch4vars] <- data [data$time_period==0,ch4vars]*((target_CH4total_ce )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] ),4) == round(target_CH4total_ce,4)

#second level: the rest of emissions  
non_ch4vars <- subset(tv1,!(tv1%in%ch4vars))
data [data$time_period==0,non_ch4vars] <- data [data$time_period==0,non_ch4vars]*((target_CO2total_ce-target_CH4total_ce)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars] ))

#check total for the sector is correct 
round(sum(data [data$time_period==0 & data$Index==inds[1],tv1] ),4) == round(target_CO2total_ce,4)

#breakdwown adjustment for CH4 emissions
adj_ch4vars_lw <- subset(ch4vars,grepl("_trww",ch4vars)==TRUE)
adj_ch4vars_sw <- subset(ch4vars,grepl("_waso",ch4vars)==TRUE)

lw_p <- sum(data [data$time_period==0 & data$Index==inds[1],adj_ch4vars_lw] ) / sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] )
sw_p <- sum(data [data$time_period==0 & data$Index==inds[1],adj_ch4vars_sw] ) / sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] )

#adjust to target proportions 
data [data$time_period==0,adj_ch4vars_lw] <- data [data$time_period==0,adj_ch4vars_lw] * 0.73/lw_p 
data [data$time_period==0,adj_ch4vars_sw] <- data [data$time_period==0,adj_ch4vars_sw] * 0.27/sw_p

##############
###########

#if initial value is different than zero use percent differences, otherwise, use percent differences 
#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1[j]]
init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1[j])])*(target_CO2total_ce/uncalibrated_CO2total)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1[j])]))
 data[data$Index==inds[i],tv1[j]] <- init_value*time_change
 }
}
}

#estimate sector totals 
data$emission_co2e_subsector_total_trww <- rowSums(data[,subset(tv1,grepl("trww_",tv1)==TRUE)])
data$emission_co2e_subsector_total_waso <- rowSums(data[,subset(tv1,grepl("waso_",tv1)==TRUE)])
#=======================================================================================================================================================================================================
#=======================================================================================================================================================================================================


#================================================================================================================================================
#ippu 
#================================================================================================================================================
#create vectors for co2e and ch4 vars 
tv1<-subset(tv1_all,grepl("ippu",tv1_all)==TRUE)
ch4vars <- subset(tv1,grepl("_ch4",tv1)==TRUE)

#set emissions totals targets 
target_CO2total_ippu <- as.numeric(te$target_CO2total_ippu) 
uncalibrated_CO2total <- sum(data [data$time_period==0 & data$Index==inds[1],tv1] )
#set CH4 targets 
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] )
target_CH4total_ippu <- as.numeric(te$target_CO2total_ippu)*as.numeric(te$target_CH4total_ippu)

#first level: CH4 emissions,
data [data$time_period==0,ch4vars] <- data [data$time_period==0,ch4vars]*((target_CH4total_ippu )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars] ),4) == round(target_CH4total_ippu,4)

#second level the rest of emissions  
non_ch4vars <- subset(tv1,!(tv1%in%ch4vars))
data [data$time_period==0,non_ch4vars] <- data [data$time_period==0,non_ch4vars]*((target_CO2total_ippu-target_CH4total_ippu)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars] ))

#check total
round(sum(data [data$time_period==0 & data$Index==inds[1],tv1] ),4) == round(target_CO2total_ippu,4)


#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1[j])])*(target_CO2total_ippu/uncalibrated_CO2total)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1[j])]))
 data[data$Index==inds[i],tv1[j]] <- init_value*time_change
 }
}
}

#estimate sector total 
data[,"emission_co2e_subsector_total_ippu"] <- rowSums(data[,tv1])

#=======================================================================================================================================================================================================
#=======================================================================================================================================================================================================

#================================================================================================================================================
# AFOLU
#================================================================================================================================================
tv1<-subset(tv1_all,grepl("_agrc",tv1_all)==TRUE | grepl("_lsmm",tv1_all)==TRUE  | grepl("_soil",tv1_all)==TRUE | grepl("_lndu",tv1_all)==TRUE | grepl("_lvst",tv1_all)==TRUE | grepl("_frst",tv1_all)==TRUE )
tv1_agrc_p1 <- subset(tv1,grepl("_agrc",tv1)==TRUE)
tv1_agrc_p2 <- subset(tv1,grepl("_lsmm",tv1)==TRUE)
tv1_agrc_p3 <- subset(tv1,grepl("_soil",tv1)==TRUE)
tv1_agrc_p3 <- subset(tv1_agrc_p3,tv1_agrc_p3!="emission_co2e_co2_agrc_soil_carbon_organic_soils")
tv1_agrc_p4 <- subset(tv1,grepl("_lndu",tv1)==TRUE)
tv1_agrc_p5 <- subset(tv1,grepl("_lvst",tv1)==TRUE)


#agriculture
tv1_agrc <-c(tv1_agrc_p1,tv1_agrc_p2,tv1_agrc_p3,tv1_agrc_p5)
ch4vars_agrc <- subset(tv1_agrc,grepl("_ch4",tv1_agrc)==TRUE)

#set emissions totals targets 
target_CO2total_agrc <- as.numeric(te$target_CO2total_agrc) 
uncalibrated_CO2total_agrc <- sum(data [data$time_period==0 & data$Index==inds[1],tv1_agrc] )
#set CH4 targets 
target_CH4total_agrc <- as.numeric(te$target_CO2total_agrc)*as.numeric(te$target_CH4total_agrc)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_agrc] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_agrc] <- data [data$time_period==0,ch4vars_agrc]*((target_CH4total_agrc )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_agrc] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_agrc] ),4) == round(target_CH4total_agrc,4)

#second level the rest of emissions 
non_ch4vars_agrc <- subset(tv1_agrc,!(tv1_agrc%in%ch4vars_agrc))
data [data$time_period==0,non_ch4vars_agrc] <- data [data$time_period==0,non_ch4vars_agrc]*((target_CO2total_agrc-target_CH4total_agrc)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_agrc] ))
#check total
round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_agrc] ),4) == round(target_CO2total_agrc,4)

########
#make the proportion adjustment for methane accross sectors 
############
adj_ch4vars <- subset(tv1_agrc,grepl("_ch4",tv1_agrc)==TRUE)
adj_ch4vars_agrc <- subset(adj_ch4vars,grepl("_agrc",adj_ch4vars)==TRUE)
adj_ch4vars_lvst <- subset(adj_ch4vars,!(adj_ch4vars%in%adj_ch4vars_agrc))

agrc_p <- sum(data [data$time_period==0 & data$Index==inds[1],adj_ch4vars_agrc] ) / sum(data [data$time_period==0 & data$Index==inds[1],adj_ch4vars] )
lvst_p <- sum(data [data$time_period==0 & data$Index==inds[1],adj_ch4vars_lvst] ) / sum(data [data$time_period==0 & data$Index==inds[1],adj_ch4vars] )

#adjust to target proportions 
data [data$time_period==0,adj_ch4vars_agrc] <- data [data$time_period==0,adj_ch4vars_agrc] * 0.25/agrc_p 
data [data$time_period==0,adj_ch4vars_lvst] <- data [data$time_period==0,adj_ch4vars_lvst] * 0.75/lvst_p 
##############
###########


#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_agrc))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_agrc[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_agrc[j]]

 if (init_value==0) {
   data[data$Index==inds[i],tv1_agrc[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_agrc[j])])*(target_CO2total_agrc/uncalibrated_CO2total_agrc)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_agrc[j])]))
 data[data$Index==inds[i],tv1_agrc[j]] <- init_value*time_change
 }
}
}

#forest coversion 
tv1_lndu <- tv1_agrc_p4
ch4vars_lndu <- subset(tv1_lndu,grepl("_ch4",tv1_lndu)==TRUE)

#set emissions totals targets 
target_CO2total_lndu <- as.numeric(te$target_CO2total_lndu) 
uncalibrated_CO2total_lndu <- sum(data [data$time_period==0 & data$Index==inds[1],tv1_lndu] )
#set CH4 targets 
target_CH4total_lndu <- as.numeric(te$target_CO2total_lndu)*as.numeric(te$target_CH4total_lndu)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_lndu ] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_lndu] <- data [data$time_period==0,ch4vars_lndu]*((target_CH4total_lndu )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_lndu] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_lndu] ),4) == round(target_CH4total_lndu,4)

#second level the rest of emissions  
non_ch4vars_lndu <- subset(tv1_lndu,!(tv1_lndu%in%ch4vars_lndu))
data [data$time_period==0,non_ch4vars_lndu] <- data [data$time_period==0,non_ch4vars_lndu]*((target_CO2total_lndu-target_CH4total_lndu)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_lndu] ))

round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_lndu] ),4)==round(target_CO2total_lndu , 4) 

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-5
for (j in 1:length(tv1_lndu))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_lndu[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_lndu[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1_lndu[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_lndu[j])])*(target_CO2total_lndu/uncalibrated_CO2total_lndu)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_lndu[j])]))
 time_change_fix <- ifelse(time_change>5,5,time_change)
 if (rall[z]=="argentina") {time_change<-time_change_fix}else{time_change<-time_change}
 data[data$Index==inds[i],tv1_lndu[j]] <- init_value*time_change
 }
}
}


#forest  
tv1_frst <- subset(tv1,grepl("_frst",tv1)==TRUE)
ch4vars_frst <- subset(tv1_frst,grepl("_ch4",tv1_frst)==TRUE)

#set emissions totals targets 
target_CO2total_frst <- as.numeric(te$target_CO2total_frst) 
uncalibrated_CO2total_frst <- sum(data [data$time_period==0 & data$Index==inds[1],tv1_frst] )
#set CH4 targets 
target_CH4total_frst <- -1*(as.numeric(te$target_CO2total_frst)*as.numeric(te$target_CH4total_frst))
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_frst ] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_frst] <- data [data$time_period==0,ch4vars_frst]*((target_CH4total_frst )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_frst] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_lndu] ),4) == round(target_CH4total_lndu,4)

#second level the rest of emissions  
non_ch4vars_frst <- subset(tv1_frst,!(tv1_frst%in%ch4vars_frst))
data [data$time_period==0,non_ch4vars_frst] <- data [data$time_period==0,non_ch4vars_frst]*((target_CO2total_frst-target_CH4total_frst )/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_frst] ))

round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_frst] ),4)==round(target_CO2total_frst , 4) 

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_frst))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_frst[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_frst[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1_frst[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_frst[j])])*(target_CO2total_frst/uncalibrated_CO2total_frst)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_frst[j])]))
 data[data$Index==inds[i],tv1_frst[j]] <- init_value*time_change
 }
}
}

#estimate sector totals 
data[,"emission_co2e_subsector_total_agrc"]<-rowSums(data[,tv1_agrc_p1])
data[,"emission_co2e_subsector_total_frst"]<-rowSums(data[,tv1_frst])
data[,"emission_co2e_subsector_total_lndu"]<-rowSums(data[,tv1_agrc_p4])
data[,"emission_co2e_subsector_total_lsmm"]<-rowSums(data[,tv1_agrc_p2])
data[,"emission_co2e_subsector_total_lvst"]<-rowSums(data[,tv1_agrc_p5])
data[,"emission_co2e_subsector_total_soil"]<-rowSums(data[,tv1_agrc_p3])


#=======================================================================================================================================================================================================
#=======================================================================================================================================================================================================


#================================================================================================================================================
# ENERGY
#================================================================================================================================================
tv1<-subset(tv1_all,grepl("_scoe",tv1_all)==TRUE | grepl("_trns",tv1_all)==TRUE | grepl("_inen",tv1_all)==TRUE | grepl("_entc",tv1_all)==TRUE | grepl("_fgtv",tv1_all)==TRUE | grepl("_ccsq",tv1_all)==TRUE ) 

#scoe
tv1_scoe <- subset(tv1,grepl("_scoe",tv1)==TRUE)
ch4vars_scoe<- subset(tv1_scoe,grepl("_ch4",tv1_scoe)==TRUE)
#CO2 total target 
target_CO2total_scoe <- as.numeric(te$target_CO2total_scoe) 
uncalibrated_CO2total_scoe <- sum(data [data$time_period==0 & data$Index==inds[1],tv1_scoe] )

#CH4 total target 
target_CH4total_scoe <- as.numeric(te$target_CO2total_scoe)*as.numeric(te$target_CH4total_scoe)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_scoe ] )


#first level: CH4 emissions,
data [data$time_period==0,ch4vars_scoe] <- data [data$time_period==0,ch4vars_scoe]*((target_CH4total_scoe )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_scoe] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_scoe] ),4) == round(target_CH4total_scoe,4)


#second level the rest of emissions  
non_ch4vars_scoe <- subset(tv1_scoe,!(tv1_scoe%in%ch4vars_scoe))
data [data$time_period==0,non_ch4vars_scoe] <- data [data$time_period==0,non_ch4vars_scoe]*((target_CO2total_scoe-target_CH4total_scoe)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_scoe] ))

round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_scoe] ),4)==round(target_CO2total_scoe,4)

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_scoe))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_scoe[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_scoe[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1_scoe[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_scoe[j])])*(target_CO2total_scoe/uncalibrated_CO2total_scoe)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_scoe[j])]))
 data[data$Index==inds[i],tv1_scoe[j]] <- init_value*time_change
 }
}
}

#transport 
tv1_trns <- subset(tv1,grepl("_trns",tv1)==TRUE)
ch4vars_trns <- subset(tv1_trns,grepl("_ch4",tv1_trns)==TRUE)

#CO2 total target
target_CO2total_trns <- as.numeric(te$target_CO2total_trns) 
uncalibrated_CO2total_trns <- sum(data [data$time_period==0 & data$Index==inds[1],tv1_trns] )

#CH4 targets 
target_CH4total_trns <- as.numeric(te$target_CO2total_trns)*as.numeric(te$target_CH4total_trns)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_trns ] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_trns] <- data [data$time_period==0,ch4vars_trns]*((target_CH4total_trns )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_trns] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_trns] ),4) == round(target_CH4total_trns,4)

#second level the rest of emissions  
non_ch4vars_trns <- subset(tv1_trns,!(tv1_trns%in%ch4vars_trns))
data [data$time_period==0,non_ch4vars_trns] <- data [data$time_period==0,non_ch4vars_trns]*((target_CO2total_trns-target_CH4total_trns)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_trns] ))

round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_trns] ),4) == round(target_CO2total_trns , 4)

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_trns))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_trns[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_trns[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1_trns[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_trns[j])])*(target_CO2total_trns/uncalibrated_CO2total_trns)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_trns[j])]))
 data[data$Index==inds[i],tv1_trns[j]] <- init_value*time_change
 }
}
}

#inen 
tv1_inen<- subset(tv1,grepl("_inen",tv1)==TRUE)
ch4vars_inen <- subset(tv1_inen,grepl("_ch4",tv1_inen)==TRUE)

#CO2 totals target
target_CO2total_inen<- as.numeric(te$target_CO2total_inen) 
uncalibrated_CO2total_inen<- sum(data [data$time_period==0 & data$Index==inds[1],tv1_inen] )

#CH4 targets 
target_CH4total_inen <- as.numeric(te$target_CO2total_inen)*as.numeric(te$target_CH4total_inen)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_inen ] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_inen] <- data [data$time_period==0,ch4vars_inen]*((target_CH4total_inen )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_inen] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_inen] ),4) == round(target_CH4total_inen,4)


#second level the rest of emissions 
non_ch4vars_inen<- subset(tv1_inen,!(tv1_inen%in%ch4vars_inen))
data [data$time_period==0,non_ch4vars_inen] <- data [data$time_period==0,non_ch4vars_inen]*((target_CO2total_inen-target_CH4total_inen)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_inen] ))

round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_inen] ),4)==round(target_CO2total_inen,4)

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_inen))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_inen[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_inen[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1_inen[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_inen[j])])*(target_CO2total_inen/uncalibrated_CO2total_inen)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_inen[j])]))
 data[data$Index==inds[i],tv1_inen[j]] <- init_value*time_change
 }
}
}

#entc
tv1_entc<- subset(tv1,grepl("_entc",tv1)==TRUE)
ch4vars_entc <- subset(tv1_entc,grepl("_ch4",tv1_entc)==TRUE)

#CO2 targets 
target_CO2total_entc<- as.numeric(te$target_CO2total_entc) 
uncalibrated_CO2total_entc<- sum(data [data$time_period==0 & data$Index==inds[1],tv1_entc] )

#CH4 targets  
target_CH4total_entc <- as.numeric(te$target_CO2total_entc)*as.numeric(te$target_CH4total_entc)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_entc ] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_entc] <- data [data$time_period==0,ch4vars_entc]*((target_CH4total_entc )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_entc] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_entc] ),4) == round(target_CH4total_entc,4)


#second level the rest of emissions  
non_ch4vars_entc<- subset(tv1_entc,!(tv1_entc%in%ch4vars_entc))
data [data$time_period==0,non_ch4vars_entc] <- data [data$time_period==0,non_ch4vars_entc]*((target_CO2total_entc-target_CH4total_entc)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_entc] ))


round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_entc] ),4)==round(target_CO2total_entc,4)

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_entc))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_entc[j]]
init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_entc[j]]
 init_value <- ifelse(is.na(init_value)==TRUE,0,init_value)
 if (init_value==0) {
   data[data$Index==inds[i],tv1_entc[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_entc[j])])*(target_CO2total_entc/uncalibrated_CO2total_entc)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_entc[j])]))
 #time_change_fix <- ifelse(time_change>5,5,time_change)
 #if (rall[z]=="panama" | rall[z]=="jamaica") {time_change<-time_change_fix}else{time_change<-time_change}
 data[data$Index==inds[i],tv1_entc[j]] <- init_value*time_change
 }
}
}

#fugitive emissions 
tv1_fgtv<- subset(tv1,grepl("_fgtv",tv1)==TRUE)
ch4vars_fgtv<- subset(tv1_fgtv,grepl("_ch4",tv1_fgtv)==TRUE)

#CO2 total target
target_CO2total_fgtv <- as.numeric(te$target_CO2total_fgtv) 
uncalibrated_CO2total_fgtv<- sum(data [data$time_period==0 & data$Index==inds[1],tv1_fgtv] )

#CH4 total target 
target_CH4total_fgtv <- as.numeric(te$target_CO2total_fgtv)*as.numeric(te$target_CH4total_fgtv)
uncalibrated_CH4total <- sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_fgtv ] )

#first level: CH4 emissions,
data [data$time_period==0,ch4vars_fgtv] <- data [data$time_period==0,ch4vars_fgtv]*((target_CH4total_fgtv )/sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_fgtv] ))
round(sum(data [data$time_period==0 & data$Index==inds[1],ch4vars_fgtv] ),4) == round(target_CH4total_fgtv,4)

#second level the rest of emissions  
non_ch4vars_fgtv<- subset(tv1_fgtv,!(tv1_fgtv%in%ch4vars_fgtv))
data [data$time_period==0,non_ch4vars_fgtv] <- data [data$time_period==0,non_ch4vars_fgtv]*((target_CO2total_fgtv-target_CH4total_fgtv)/sum(data [data$time_period==0 & data$Index==inds[1],non_ch4vars_fgtv] ))

round(sum(data [data$time_period==0 & data$Index==inds[1],tv1_fgtv] ),4)==round(target_CO2total_fgtv,4)

#finally for every var and every index and every time period  
for (i in 1:length(inds))
{
#i<-8
for (j in 1:length(tv1_fgtv))
{ 
#j<-45
# init_value <-data[data$Index==inds[i] & data$time_period==0, tv1_fgtv[j]]
 init_value <-data[data$Index==paste0(rall[z],initial_conditions_id) & data$time_period==0, tv1_fgtv[j]]
 if (init_value==0) {
   data[data$Index==inds[i],tv1_fgtv[j]]<-init_value+cumsum(pct_diffs[pct_diffs$Index==inds[i], paste0("diff_",tv1_fgtv[j])])*(target_CO2total_fgtv/uncalibrated_CO2total_fgtv)
 } else {
 time_change <- cumprod((1+pct_diffs[pct_diffs$Index==inds[i], paste0("pct_diff_",tv1_fgtv[j])]))
 data[data$Index==inds[i],tv1_fgtv[j]] <- init_value*time_change
 }
}
}

#estimate totals 

tv1_ccsq<- subset(tv1,grepl("_ccsq",tv1)==TRUE)
data[,"emission_co2e_subsector_total_ccsq"]<-rowSums(data[,tv1_ccsq])
data[,"emission_co2e_subsector_total_entc"]<-rowSums(data[,tv1_entc])
data[,"emission_co2e_subsector_total_fgtv"]<-rowSums(data[,tv1_fgtv])
data[,"emission_co2e_subsector_total_inen"]<-rowSums(data[,tv1_inen])
data[,"emission_co2e_subsector_total_scoe"]<-rowSums(data[,tv1_scoe])
data[,"emission_co2e_subsector_total_trns"]<-rowSums(data[,tv1_trns])

#=======================================================================================================================================================================================================
#=======================================================================================================================================================================================================
#write the file 
data$Index <- NULL 
dim(data)
write.csv(data,paste0(dir.output,tregion,".csv"),row.names=FALSE)
rm(data)
print(rall[z])
}
