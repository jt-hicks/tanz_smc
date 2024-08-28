orderly2::orderly_shared_resource('addCIs.R','process_tz_data.R')
source('addCIs.R')
source('process_tz_data.R')
orderly2::orderly_dependency("data_quality", "latest()",
                             c('full_data_dqa1.RDS'))
#get Tanzania data at admin 2 level
# tanz_data_all_16to22_dist <- read.csv("TZ_ANC_data_district_2016_2022.csv")
full_data_dqa1 < readRDS('full_data_dqa1.RDS')

##Get SMC options
smc_options <- readxl::read_xls('MALARIA CONSORTIUM - TZ data.xls')%>%
  mutate(Region=gsub(' Region','',Region))%>%
  rename(smc_option=`SMC (O=Option)`,
         strata_2022 = `Strata 2022`,
         strata_urb_2022 = `Strata 2022 + Urban`,
         inc_2022 = `2022 confirmed Incidence per 1000`,
         pop_2022 = `Census 2022`,
         cases_2022 = `2022 Confirmed Cases`)%>%
  filter(smc_option%in%c('O1','O2'))

#Reformat council names so SMC options match data
smc_council_list <- smc_options[,c('District','smc_option')]%>%
  mutate(council = gsub(' DC',' District Council',District))%>%
  mutate(council = gsub(' TC',' Town Council',council))%>%
  mutate(council = gsub(' MC',' Municipal Council',council))

tanz_data_all_16to22_dist <-full_data_dqa1%>%
  ##Remove erroneous time points in HFs in Mbeya, Ruvuma and Mtwara
  filter(!(filter_var=Council%in%smc_council_list$council&ANC_pos>90))%>%
  mutate(yearmon=zoo::as.yearmon(paste0(Year,"-",Month),"%Y-%m"))%>%
  group_by(yearmon,Region,Council)%>%
  dplyr::summarise(count=n(),positive=sum(ANC_pos, na.rm = TRUE),tested=sum(ANC_test, na.rm=TRUE),total=sum(Total_re_ad, na.rm = TRUE))

tanz_data_smc_16to22_dist <- tanz_data_all_16to22_dist%>%
  mutate(council = gsub(' DC',' District Council',Council))%>%
  mutate(council = gsub(' TC',' Town Council',council))%>%
  mutate(council = gsub(' MC',' Municipal Council',council))%>%
  right_join(smc_council_list,by='council')

##Create Council to Region key
smc_regions <- tanz_data_smc_16to22_dist[!duplicated(tanz_data_smc_16to22_dist$Council),c('Council','Region')]
smc_region_key <- gsub(' Region','',smc_regions$Region)
names(smc_region_key) <- smc_regions$Council

##Add confidence intervals to data
tanz_data_smc_16to22_dist <- addCIs(df=tanz_data_smc_16to22_dist,Ys=tanz_data_smc_16to22_dist$positive,Ns=tanz_data_smc_16to22_dist$tested)

##Look for outliers
str(tanz_data_smc_16to22_dist)
tanz_data_smc_16to22_dist_outliers <- tanz_data_smc_16to22_dist %>%
  mutate(year = lubridate::year(zoo::as.yearmon(yearmon)),
         month = lubridate::month(zoo::as.yearmon(yearmon)))%>%
  group_by(Region,Council,District)%>%
  mutate(special_mean_positive = (sum(positive) - positive)/(n()-1),
         special_mean_tested = (sum(tested) - tested)/(n()-1),
         special_mean_prev_unwt = (sum(mean) - mean)/(n()-1),
         special_mean_prev_wt = special_mean_positive/special_mean_tested,
         tested_gt3xmean = (tested>=special_mean_tested*3),
         pos_gt3xmean = (positive>=special_mean_positive*3),
         prev_gt3xwtmean = (mean>=special_mean_prev_wt*3),
         prev_gt3xunwtmean = (mean>=special_mean_prev_unwt*3),
         prev_gt2xmax = (mean>=max(mean)*2),
         prev_gtIQR = (mean>=(quantile(mean,probs=0.75)+1.5*IQR(mean))),
         prev_gtMAD = (mean>=median(mean) + 3 * mad(mean)))%>%
  ungroup()%>%
  mutate(half_year = paste0(year,'-',ifelse(month<=6,1,2)))%>%
  group_by(Region,Council,District,half_year)%>%
  mutate(special_mean_positive_6m = (sum(positive) - positive)/(n()-1),
         special_mean_tested_6m = (sum(tested) - tested)/(n()-1),
         special_mean_prev_unwt_6m = (sum(mean) - mean)/(n()-1),
         special_mean_prev_wt_6m = special_mean_positive_6m/special_mean_tested_6m,
         tested_gt3xmean_6m = (tested>=special_mean_tested_6m*3),
         pos_gt3xmean_6m = (positive>=special_mean_positive_6m*3),
         prev_gt3xwtmean_6m = (mean>=special_mean_prev_wt_6m*3),
         prev_gt3xunwtmean_6m = (mean>=special_mean_prev_unwt_6m*3),
         prev_gt3xwtmean_both = (prev_gt3xwtmean&prev_gt3xwtmean_6m),
         prev_gt3xunwtmean_both = (prev_gt3xunwtmean&prev_gt3xunwtmean_6m))

##Visualize outliers
outliers_trend_plot <- ggplot(tanz_data_smc_16to22_dist_outliers)+
  geom_vline(xintercept = zoo::as.yearmon(c('Jan 2016','Jan 2017','Jan 2018','Jan 2019','Jan 2020','Jan 2021','Jan 2022')),linetype='dashed',color='#999999')+
  geom_line(aes(x=zoo::as.yearmon(yearmon),y=mean))+
  geom_point(aes(x=zoo::as.yearmon(yearmon),y=mean,color=prev_gt3xwtmean_both))+
  geom_errorbar(aes(x=zoo::as.yearmon(yearmon),ymin=lower,ymax=upper,color=prev_gt3xwtmean_both))+
  facet_wrap(Region~District)+
  labs(x='Month',y='ANC prevalence',title='SMC Option 1')
ggplot2::ggsave(filename='outliers_trend_plot.tiff',plot=outliers_trend_plot,width=12,height = 8,units='in')

final_data <- tanz_data_smc_16to22_dist_outliers %>%
  ungroup()%>%
  select(1:13)
final_data_list <- tanz_data_process_allonly(data=final_data,level='District')

saveRDS(final_data,'tanz_data_smc_16to22_dist.RDS')
saveRDS(final_data_list,'tanz_data_smc_16to22_dist_list.RDS')

