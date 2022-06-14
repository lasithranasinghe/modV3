library(here)
library(data.table)
library(ggplot2)
library(metafor)


## read and reform data
N <- fread(here('data/tb_cleaned_notifs.csv'))


M <- melt(N[,.(iso3,year,
               newrel_m04,newrel_m514,newrel_m15plus,
               newrel_f04,newrel_f514,newrel_f15plus)],
          id=c('iso3','year'))

M[,sex:=ifelse(grepl('f',variable),'female','male')]
M[,age:=ifelse(grepl('plus',variable),'15+',
        ifelse(grepl('04',variable),'0-4','5-14'))]

M[iso3=='AGO' & year==2014] #check

M <- dcast(M,iso3+year+age~sex,value.var = 'value')
M[,tot:=female+male]

## add logit props & variance for ML model
M[,c('yi','vi'):=escalc(measure='PLO',xi=male,ni=tot)]

## naive meta-analysis
res <- rma(measure="PLO", xi=male, ni=tot, data=M[age=='0-4'])
print(res, digits=3)
predict(res, transf=transf.ilogit, digits=3)

## years within countries
res.ML <- rma.mv(yi = yi,V = vi,
                     slab = iso3,
                     data = M[age=='0-4'],
                     random = ~ 1 | iso3/year,
                     method = "REML")
print(res.ML, digits=3)
predict(res.ML, transf=transf.ilogit, digits=3)

## loop over for output
agz <- c('0-4','5-14','15+')
ans <- list()
for(ag in agz){
  res.ML <- rma.mv(yi = yi,V = vi,
                   slab = iso3,
                   data = M[age==ag],
                   random = ~ 1 | iso3/year,
                   method = "REML")
  tmp <- as.data.table(
    predict(res.ML, transf=transf.ilogit, digits=3)
  )
  tmp[,age:=ag]
  ans[[ag]] <- tmp
}
ans <- rbindlist(ans)


fwrite(ans,file=here('plots/sexmetaresults.csv'))
