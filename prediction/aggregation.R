## example of generating aggregated estimates from sub-units
library(here)
library(data.table)

## utility function to do sqrt sum sqrs NOTE similar naming!
ssum <- function(x)sqrt(sum(x^2))


## read data
pred <- readRDS(here('artefacts/predictions.rds'))
pred <- as.data.table(pred)
pred[,point.sd:=(upper95-lower95)/3.92] #SD under normal assumption


## --- do HBC30 aggregates
HBC <- list()
HBC[['sa']] <- pred[type=='hbc',
                    .(point=sum(point),
                      point.sd=ssum(point.sd)),
                    by=.(sex,age_group)] #sex/age stratified
HBC[['a']] <- pred[type=='hbc',
                   .(sex='all',
                     point=sum(point),
                     point.sd=ssum(point.sd)),
                   by=.(age_group)] #sex stratified
HBC[['not']] <- pred[type=='hbc',
                     .(sex='all',age_group='both',
                       point=sum(point),
                       point.sd=ssum(point.sd))
                     ] #notstratified
HBC <- rbindlist(HBC,use.names = TRUE)
HBC[,c('lo','hi'):=.(point-1.96*point.sd,point+1.96*point.sd)]
## NOTE may need to round after, depending on what is applied

## save out
save(HBC,file=here('artefacts/HBC.Rdata'))


## --- do Global aggregates
glob <- list()
glob[['sa']] <- pred[type=='region',
                    .(point=sum(point),
                      point.sd=ssum(point.sd)),
                    by=.(sex,age_group)] #sex/age stratified
glob[['a']] <- pred[type=='region',
                   .(sex='all',
                     point=sum(point),
                     point.sd=ssum(point.sd)),
                   by=.(age_group)] #sex stratified
glob[['not']] <- pred[type=='region',
                     .(sex='all',age_group='both',
                       point=sum(point),
                       point.sd=ssum(point.sd))
                     ] #notstratified
glob <- rbindlist(glob,use.names = TRUE)
glob[,c('lo','hi'):=.(point-1.96*point.sd,point+1.96*point.sd)]
## NOTE may need to round after, depending on what is applied

## save out
save(glob,file=here('artefacts/glob.Rdata'))

## NOTE if applying more this pattern to another case, would make sense
## (ie save lines) to wrap as function
