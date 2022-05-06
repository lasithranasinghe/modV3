## librarites
library(here)
library(data.table)
library(ggplot2)
library(imputeTS)

## read data
missing <- readRDS(here('artefacts/missing.rds'))

## change data
missing <- as.data.table(missing)
missing[,imputed:=ifelse(is.na(cases),TRUE,FALSE)]
missing[,cases:=as.integer(cases)]
missing[,cases:=as.integer(na_interpolation(cases)),by=.(iso3,sex,age_group)]

## check valid
missing[cases<0]

## visual check
ggplot(missing,aes(year,cases,
                   group=paste0(iso3,age_group,sex),
                   col=sex))+
  geom_line()+
  geom_point(aes(shape=imputed),size=4)+
  facet_wrap(~iso3,scales='free')

## inspect
print(missing,n=Inf)

## save out
notmissing <- missing[,.(iso3,sex,age_group,year,cases)]
save(notmissing,file=here('artefacts/notmissing.Rdata'))
