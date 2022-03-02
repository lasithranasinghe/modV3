library(here)
library(data.table)
library(ggplot2)
library(whomap) #devtools::install_github('glaziou/whomap')
library(lubridate)
library(ggrepel)

## --- quick map
load(here('plots/mpd.Rdata')) #mpd was saved out as the last object in Jay's maps.Rmd file
mpd <- as.data.table(mpd)
mpd[!is.finite(percent_change),percent_change:=NA]
qz <- quantile(mpd[age_group=='014',percent_change],(0:5)/5,na.rm = TRUE)
mpd[,chcat:=cut(percent_change,breaks = qz,
                include.lowest = TRUE,ordered_result = TRUE)]

whomap(mpd[age_group=='014',.(iso3,var=chcat)],
       legend.title = '% change in 014 notifications')

ggsave(here('plots/NoteChange014.pdf'),w=7,h=5)


## --- compare with COVID

CVD <- fread(here('plots/export_country_per_100k_cumulative.csv')) #Economist estimates on github
CF <- CVD[date<ymd('2021-01-01'),.(cedpc=max(cumulative_daily_covid_deaths_per_100k)),by=.(iso3=iso3c)] #max per cap deaths <2021
save(CF,file=here('plots/CF.Rdata')) #save for others

CF <- merge(CF,mpd[age_group=='014',.(iso3,percent_change)],by='iso3')

ggplot(CF,aes(cedpc,percent_change,label=iso3)) +
  geom_point() + geom_text_repel()+
  xlab('Estimated cumulative COVID deaths per 100K prior to 2021 (square root scale)')+
  ylab('Percent change in TB notifications')+
  scale_x_sqrt()+
  geom_abline(yintercept=0,slope=0,col=2)

ggsave(here('plots/COVIDvTB.pdf'),w=15,h=10)
