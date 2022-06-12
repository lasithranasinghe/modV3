library(here)
library(data.table)
library(ggplot2)
library(lubridate)
library(ggrepel)
library(ggpubr)
library(wbmapdata) # devtools::install_github('petedodd/wbmapdata')
library(sf)

ssum <- function(X) sqrt(sum(X^2))


load(here("plots/HBC.Rdata")) # 30 HBC data

## --- quick map
load(here("plots/mpd.Rdata")) # mpd was saved out as the last object in Jay's maps.Rmd file
mpd <- as.data.table(mpd)
mpd[!is.finite(percent_change), percent_change := NA]
qz <- quantile(mpd[age_group == "014", percent_change], (0:5) / 5, na.rm = TRUE)
mpd[, chcat := cut(percent_change,
  breaks = qz,
  include.lowest = TRUE, ordered_result = TRUE
)]


## --- better map (diverging continuous color scale)
mpdw <- dcast(mpd, iso3 ~ age_group, value.var = "percent_change")
mpdw <- mpdw[, .(iso3, u5 = `04`, o5 = `514`, u15 = `014`, adults = `15plus`)]
MPD <- sp::merge(world, mpdw, by = "iso3", all.x = TRUE) # merge against map

## convert & add mid-coords
MP <- st_as_sf(MPD)
MP$squ5 <- sign(MP$u5) * sqrt(abs(MP$u5)) # sqrt

##  version
p <- ggplot(data = MP) +
  geom_sf(aes(fill = squ5)) +
  scale_fill_gradient2(
    name = "notification change\n(square root of %)",
    na.value = "grey"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.1, .45),
    legend.title.align = 0.5,
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  )
## p

ggsave(p, file = here("plots/Map_04.pdf"), w = 18, h = 12)


## --- barplots for 30 HBC
## now using estimated shortfall rather than notification change jj
# G <- readRDS(here('artefacts/granular_predictions.rds')) #mpd was saved out as the last object in Jay's maps.Rmd file
# G <- as.data.table(G)
# G[,unique(location)]
# G <- G[location %in% HBC[g.hbc==TRUE,iso3]] #restrict HBC30
# mpdh30 <- G[,.(iso3=location,age_group,sex,mid=point,lo=lower95,hi=upper95)]
# mpdh30[,mid.sd:=(hi-lo)/3.92]
# mpdh30 <- mpdh30[,.(mid=sum(mid),mid.sd=ssum(mid.sd)),by=.(iso3,age_group)]
# mpdh30[,c('lo','hi'):=.(mid-1.96*mid.sd,mid+1.96*mid.sd)]
# mpdh30[,unique(iso3)]
#
# ## ref cases
# mpdref <- mpd[iso3 %in% HBC[g.hbc==TRUE,iso3],.(iso3,country,age=age_group,cases)] #NOTE DRC = COD has no data
# mpdref <- mpdref[age!='014']
# mpdref[age=='04',age_group:='0-4y']
# mpdref[age=='514',age_group:='5-14y']
# mpdref[age=='15plus',age_group:='Over 15y']
#
#
# ## merge
# mpdh30 <- merge(mpdh30,mpdref,by=c('iso3','age_group'))
# ## total
# tot30 <- mpdh30[,.(mid=sum(mid),mid.sd=ssum(mid.sd),cases=sum(cases)),by=age_group]
# tot30[,c('lo','hi','iso3','country'):=.(mid-1.96*mid.sd,mid+1.96*mid.sd,'HBC30','HBC30')]
# mpdh30 <- rbind(mpdh30,tot30,fill=TRUE) #add in total
#
# ## ranking
# cnz <- mpdh30[age_group!='Over 15y',.(dfr=sum(cases-mid)),by=country][order(dfr),country] #countries in increasing order of estimated total 014 drop
#
# ## colors and labels
# mpdh30[,cl:='black']
# mpdh30[country=='HBC30',cl:='red']
# mpdh30[country!='HBC30',totdf:=sum(cases),by=age_group] #total cases
# mpdh30[,pctxt:=1e2*cases/totdf]
# mpdh30[,pctxt:=paste0(sprintf(pctxt, fmt = '%#.1f'),'%')]
# mpdh30[country=='HBC30',pctxt:='100%'] #% label us % of all cases in 2020
#
# ## ordering
# mpdh30$country <- factor(mpdh30$country,levels=rev(cnz),ordered=TRUE)
# mpdh30$age_group<- factor(mpdh30$age_group,levels=c('0-4y','5-14y','Over 15y'),
#                      ordered=TRUE)
#
# ## add % change
# mpdh30[,percent_change:=100*(cases-mid)/mid]
# mpdh30[,sqrtpc:=sign(percent_change)*sqrt(abs(percent_change))]
#
# ## plot
# scl <- 1e2
# GP <- ggplot(mpdh30,aes(country,percent_change/scl,
#                   fill=I(cl),col=I(cl),label=pctxt))+
#   geom_bar(stat='identity')+
#   geom_text(aes(x=country,y=90/scl))+
#   scale_y_continuous(limits=c(-100/scl,100/scl),label=scales::percent)+
#   coord_flip()+
#   facet_wrap(~age_group)+
#   ylab('Estimated reduction in notifications in 2020')+
#   xlab('Country or countries') + theme_light()
#
# ## add off scale arrow
# arrowdf <- data.table(iso3='LBR',country='Liberia',
#                       age_group=c('0-4y','5-14y'),pctxt='',cl='blue')
#
# GP <- GP +  geom_segment(data = arrowdf,
#                          aes(x = 3, xend = 3, y = 0, yend = .8),
#                          colour = "blue", size = 0.5, alpha=0.9, arrow = arrow())
#
#
# ggsave(GP,file=here('plots/HBC30_barplot.pdf'),h=6,w=16)
# ggsave(GP,file=here('plots/HBC30_barplot.png'),h=6,w=16)
# ggsave(GP,file=here('plots/HBC30_barplot.eps'),h=6,w=16)



## --- compare with COVID



CF <- merge(CF, mpd[age_group == "014", .(iso3, percent_change)], by = "iso3")

## stringency data
S <- fread(here("plots/stringency_index.csv"))
S[, c("V1", "country_name") := NULL]
S <- melt(S, id = "country_code")
S <- S[grepl("2020", variable)]
S <- S[, .(index = mean(value)), by = .(iso3 = country_code)] # mean for 2020

## merge in
CF <- merge(mpdh30, S, by = "iso3", all.x = TRUE, all.y = FALSE)

agz <- CF[, unique(age)]
CF[is.na(age)] # using age not age_group automatically drops HBC30
agz <- c(na.omit(agz))

CF <- CF[iso3 != "LBR"]

## loop over ages
for (a in agz) {
  GP <- ggscatter(CF[age == a],
    x = "index",
    y = "percent_change", add = "reg.line", conf.int = TRUE,
    xlim = c(22, 70),
    ylim = c(-100, 50),
    xlab = "Mean COVID stringency index 2021",
    ylab = "Percent change in TB notifications"
  ) +
    geom_text_repel(aes(label = iso3)) +
    stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
      label.x = 30, label.y = -60, size = 10
    ) + grids()
  ggsave(GP, file = here(paste0("plots/StringvTB_noLBR_", a, ".pdf")), w = 15, h = 10)
  ggsave(GP, file = here(paste0("plots/StringvTB_noLBR_", a, ".png")), w = 15, h = 10)
  ggsave(GP, file = here(paste0("plots/StringvTB_noLBR_", a, ".eps")), w = 15, h = 10)
}