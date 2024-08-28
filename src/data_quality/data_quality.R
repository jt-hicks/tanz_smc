Full_data_2016<-readxl::read_xlsx("ANC_Data_Compiled.xlsx",sheet="2016_2019")
Full_data_2020<-readxl::read_xlsx("ANC_Data_Compiled.xlsx",sheet="2020_2021")
Full_data_2022<-readxl::read_xlsx("ANC_Data_Compiled.xlsx",sheet="2022")

names(Full_data_2016)[5]<-"Month"
names(Full_data_2016)[6]<-"Before_12wk"
names(Full_data_2016)[7]<-"After_12wk"
names(Full_data_2016)[8]<-"ANC_re_ad"
names(Full_data_2016)[9]<-"Total_re_ad"
names(Full_data_2016)[10]<-"ANC_test"
names(Full_data_2016)[11]<-"ANC_pos"
names(Full_data_2016)[16]<-"Hb_test"
names(Full_data_2016)[17]<-"Anaemia"

names(Full_data_2020)[5]<-"Month"
names(Full_data_2020)[6]<-"Before_12wk"
names(Full_data_2020)[7]<-"After_12wk"
names(Full_data_2020)[8]<-"ANC_re_ad"
names(Full_data_2020)[9]<-"Total_re_ad"
names(Full_data_2020)[10]<-"ANC_test"
names(Full_data_2020)[11]<-"ANC_pos"
names(Full_data_2020)[16]<-"Hb_test"
names(Full_data_2020)[17]<-"Anaemia"

names(Full_data_2022)[5]<-"Month"
names(Full_data_2022)[6]<-"Year"
names(Full_data_2022)[7]<-"Before_12wk"
names(Full_data_2022)[8]<-"After_12wk"
names(Full_data_2022)[9]<-"ANC_re_ad"
names(Full_data_2022)[10]<-"Total_re_ad"
names(Full_data_2022)[11]<-"ANC_test"
names(Full_data_2022)[12]<-"ANC_pos"

names(Full_data_2022)[17]<-"Hb_test"
names(Full_data_2022)[18]<-"Anaemia"

Full_data<-rbind(
  Full_data_2022%>%select(Region,Council,HF,Year,Month,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,Hb_test,Anaemia),
  Full_data_2020%>%select(Region,Council,HF,Year,Month,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,Hb_test,Anaemia),
  Full_data_2016%>%select(Region,Council,HF,Year,Month,Before_12wk,After_12wk,ANC_re_ad,Total_re_ad,ANC_test,ANC_pos,Hb_test,Anaemia)
)
rm(Full_data_2022,Full_data_2020,Full_data_2016)

full_data_dqa1 <- Full_data %>%
  mutate(Before_12wk = ifelse(is.na(Before_12wk), 0, Before_12wk),
         After_12wk = ifelse(is.na(After_12wk), 0, After_12wk),
         ANC_re_ad = ifelse(is.na(ANC_re_ad), 0, ANC_re_ad),
         ANC_test = ifelse(is.na(ANC_test), 0, ANC_test),
         ANC_pos = ifelse(is.na(ANC_pos), 0, ANC_pos),
         first_ad = Before_12wk + After_12wk)%>%
  #Remove rows where don't have any visits
  filter(Total_re_ad != 0) %>%
  #Remove rows where don't have any first visits
  filter(first_ad != 0)%>%
  #Remove outlier row (simiyu region)
  # filter(!(Region == 'Simiyu Region' &
  #            Council == 'Itilima District Council' &
  #            HF == 'MIGATO Dispensary' &
  #            Year == 2019 &
  #            Month == 5)) %>%
  #Remove rows where don't test for malaria
  filter(ANC_test != 0) %>%
  #Remove rows where pos >= malaria number tests
  filter(ANC_pos <= ANC_test) %>%
  #Remove rows where tests >= first attendance
  filter(ANC_test <= first_ad)


# hist(full_data_dqa1$ANC_test)
# hist(full_data_dqa1$ANC_pos)
#
# hist(full_data_dqa1[full_data_dqa1$ANC_test<200,]$ANC_test)
# print(full_data_dqa1[full_data_dqa1$ANC_test>1000,])
#
# ggplot(data=full_data_dqa1)+
#   geom_point(aes(x=first_ad,y=ANC_test))+
#   geom_abline()

##Group data by month and council
saveRDS(full_data_dqa1,'full_data_dqa1.RDS')

# mtwara_newala <- full_data_dqa1%>%
#   filter(Council%in%outlier_smc_df$Council)%>%
#   mutate(yearmon=zoo::as.yearmon(paste0(Year,"-",Month),"%Y-%m"))%>%
#   group_by(HF)%>%
#   mutate(prev=ANC_pos/ANC_test)%>%
#   mutate(special_mean_positive = (sum(ANC_pos) - ANC_pos)/(n()-1),
#          special_mean_tested = (sum(ANC_test) - ANC_test)/(n()-1),
#          special_mean_prev_unwt = (sum(prev) - prev)/(n()-1),
#          special_mean_prev_wt = special_mean_positive/special_mean_tested,
#          tested_gt3xmean = (ANC_test>=special_mean_tested*3),
#          pos_gt3xmean = (ANC_pos>=special_mean_positive*3),
#          prev_gt3xwtmean = (prev>=special_mean_prev_wt*3),
#          prev_gt3xunwtmean = (prev>=special_mean_prev_unwt*3),
#          prev_gt2xmax = (prev>=max(prev)*2),
#          prev_gtIQR = (prev>=(quantile(prev,probs=0.75)+1.5*IQR(prev))),
#          prev_gtMAD = (prev>=median(prev) + 3 * mad(prev)))%>%
#   ungroup()%>%
#   mutate(half_year = paste0(Year,'-',ifelse(Month<=6,1,2)))%>%
#   group_by(HF,half_year)%>%
#   mutate(special_mean_positive_6m = (sum(ANC_pos) - ANC_pos)/(n()-1),
#          special_mean_tested_6m = (sum(ANC_test) - ANC_test)/(n()-1),
#          special_mean_prev_unwt_6m = (sum(prev) - prev)/(n()-1),
#          special_mean_prev_wt_6m = special_mean_positive_6m/special_mean_tested_6m,
#          tested_gt3xmean_6m = (ANC_test>=special_mean_tested_6m*3),
#          pos_gt3xmean_6m = (ANC_pos>=special_mean_positive_6m*3),
#          prev_gt3xwtmean_6m = (prev>=special_mean_prev_wt_6m*3),
#          prev_gt3xunwtmean_6m = (prev>=special_mean_prev_unwt_6m*3),
#          prev_gt3xwtmean_both = (prev_gt3xwtmean&prev_gt3xwtmean_6m),
#          prev_gt3xunwtmean_both = (prev_gt3xunwtmean&prev_gt3xunwtmean_6m),
#          tested_gt3xwtmean_both = (tested_gt3xmean&tested_gt3xmean_6m),
#          pos_gt3xwtmean_both = (pos_gt3xmean&pos_gt3xmean_6m))
#
# table(mtwara_newala$HF)
# hist(mtwara_newala[mtwara_newala$ANC_pos<50,]$ANC_pos)
# print(mtwara_newala[mtwara_newala$ANC_pos>50,])
# ggplot(mtwara_newala)+
#   geom_point(aes(x=yearmon,y=ANC_test,color=tested_gt3xwtmean_both))+
#   facet_wrap(.~HF)
# ggplot(mtwara_newala)+
#   geom_point(aes(x=yearmon,y=ANC_pos))+
#   facet_wrap(.~HF)+
#   coord_cartesian(ylim=c(0,NA))
# ggplot(mtwara_newala)+
#   geom_point(aes(x=yearmon,y=prev,color=prev_gt3xwtmean_both))+
#   facet_wrap(.~HF)
