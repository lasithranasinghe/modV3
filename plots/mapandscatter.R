library(here)
library(data.table)
library(ggplot2)
library(whomap) #devtools::install_github('glaziou/whomap')
library(lubridate)
library(ggrepel)
library(ggpubr)
library(wbmapdata) #devtools::install_github('petedodd/wbmapdata')
library(sf)

load(here('plots/HBC.Rdata')) #30 HBC data

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


## --- better map (diverging continuous color scale)
mpdw <- dcast(mpd,iso3~age_group,value.var = 'percent_change')
mpdw <- mpdw[,.(iso3,u5=`04`,o5=`514`,u15=`014`,adults=`15plus`)]
MPD <- sp::merge(world,mpdw,by='iso3',all.x=TRUE) #merge against map

## convert & add mid-coords
MP <- st_as_sf(MPD)
MP$squ5 <- sign(MP$u5) * sqrt(abs(MP$u5)) #sqrt

##  version
p <- ggplot(data=MP) +
  geom_sf(aes(fill=squ5)) +
  scale_fill_gradient2(name='notification change\n(square root of %)',
                       na.value = 'grey')+
  theme_minimal() +
  theme(legend.position = c(.1,.45),
        legend.title.align = 0.5,
        axis.text.x = element_blank(),
        axis.text.y = element_blank())
p

ggsave(p,file=here('plots/Map_04.pdf'),w=18,h=12)


## --- barplots for 30 HBC
mpdh30 <- mpd[iso3 %in% HBC[g.hbc==TRUE,iso3]]
mpdh30[,age:=gsub('0','0-',age_group)]
mpdh30[,age:=gsub('51','5-1',age)]
mpdh30[,age:=gsub('plus','+',age)]
mpdh30 <- mpdh30[age!='0-14'] #didn't want this one
mpdh30[,cases.new:=(1+percent_change/1e2)*cases] #TODO check interpretation
tot30 <- mpdh30[,.(cases=sum(cases),cases.new=sum(cases.new)),by=age]
tot30[,c('country','percent_change'):=.('HBC30',(cases.new/cases-1)*1e2)]

cnz <- mpdh30[age!='15+',.(dfr=sum(cases-cases.new)),by=country][order(dfr),country] #countries in increasing order of 014 drop

mpdh30 <- rbind(mpdh30[,.(country,age,cases,cases.new,percent_change)],
                tot30) #add in total

## colors and labels
mpdh30[,cl:='black']
mpdh30[country=='HBC30',cl:='red']
mpdh30[country!='HBC30',totdf:=sum(cases),by=age] #total cases
mpdh30[,pctxt:=1e2*cases/totdf]
mpdh30[,pctxt:=paste0(sprintf(pctxt, fmt = '%#.1f'),'%')]
mpdh30[country=='HBC30',pctxt:='100%']

## ordering
cnzo <- c(cnz,'HBC30')
mpdh30$country <- factor(mpdh30$country,levels=rev(cnzo),ordered=TRUE)
mpdh30$age <- factor(mpdh30$age,levels=c('0-4','5-14','15+'),
                     ordered=TRUE)

## plot
ggplot(mpdh30,aes(country,percent_change,
                  fill=I(cl),col=I(cl),label=pctxt))+
  geom_bar(stat='identity')+
  geom_text(aes(x=country,y=40))+
  coord_flip()+
  facet_wrap(~age)

ggsave(here('plots/HBC30_barplot.pdf'),h=6,w=16)

## --- compare with COVID

CVD <- fread(here('plots/export_country_per_100k_cumulative.csv')) #Economist estimates on github
CF <- CVD[date<ymd('2021-01-01'),
          .(cedpc=max(cumulative_daily_covid_deaths_per_100k)),
          by=.(iso3=iso3c)] #max per cap deaths <2021
save(CF,file=here('plots/CF.Rdata')) #save for others

CF <- merge(CF,mpd[age_group=='014',.(iso3,percent_change)],by='iso3')

ggplot(CF,aes(cedpc,percent_change,label=iso3)) +
  geom_point() + geom_text_repel()+
  xlab('Estimated cumulative COVID deaths per 100K prior to 2021 (square root scale)')+
  ylab('Percent change in TB notifications')+
  scale_x_sqrt()+
  geom_abline(yintercept=0,slope=0,col=2)

ggsave(here('plots/COVIDvTB.pdf'),w=15,h=10)

## stringency data
S <- fread(here('plots/stringency_index.csv'))
S[,c('V1','country_name'):=NULL]
S <- melt(S,id='country_code')
S <- S[grepl('2020',variable)]
S <- S[,.(index=mean(value)),by=.(iso3=country_code)] #mean for 2020

## merge in
CF <- merge(CF,S,by='iso3')

## --- restrict to HBC30

CF <- CF[iso3 %in% HBC[g.hbc==TRUE,iso3]] #restrict to HBC30

ggscatter(CF,x='cedpc',y='percent_change',add='reg.line',conf.int = TRUE,
          xscale='sqrt',xlab='Estimated cumulative COVID deaths per 100K prior to 2021 (square root scale)',
          ylab='Percent change in TB notifications') +
  geom_text_repel(aes(label=iso3))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 5,size=10) +  grids()

ggsave(here('plots/COVIDvTB2.pdf'),w=15,h=10)

ggscatter(CF,x='index',
          y='percent_change',add='reg.line',conf.int = TRUE,
          xlim=c(22,70),
          xlab='Mean COVID stringency index 2021',
          ylab='Percent change in TB notifications') +
  geom_text_repel(aes(label=iso3))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 30,label.y = -60,size=10) +  grids()

ggsave(here('plots/StringvTB2.pdf'),w=15,h=10)


CFy <- merge(CF[,.(iso3,cedpc,index)],
             mpd[age_group=='04',.(iso3,percent_change)],by='iso3')

CFy <- CFy[iso3 %in% HBC[g.hbc==TRUE,iso3]] #restrict to HBC30

ggscatter(CFy,x='cedpc',y='percent_change',add='reg.line',conf.int = TRUE,
          xscale='sqrt',
          xlab='Estimated cumulative COVID deaths per 100K prior to 2021 (square root scale)',
          ylab='Percent change in TB notifications') +
  geom_text_repel(aes(label=iso3))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 5,size=10) +  grids()

ggsave(here('plots/COVIDvTB2y.pdf'),w=15,h=10)


ggscatter(CFy,x='index',
          y='percent_change',add='reg.line',conf.int = TRUE,
          xlim=c(22,70),
          xlab='Mean COVID stringency index 2021',
          ylab='Percent change in TB notifications') +
  geom_text_repel(aes(label=iso3))+
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.x = 30,label.y = -60,size=10) +  grids()

ggsave(here('plots/StringvTB2y.pdf'),w=15,h=10)
