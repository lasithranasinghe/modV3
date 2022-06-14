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
predict(res.ML, transf=transf.ilogit, digits=3)

## check extraction:
tmp <- ranef(res.ML)
F <- tmp$iso3
F <- as.data.table(F)
F$iso3 <- rownames(ranef(res.ML)$iso3)
fitted(res.ML)

F[,c('mid','lo','hi'):=.(transf.ilogit(intrcpt+fitted(res.ML)[1:30]),
                         transf.ilogit(pi.lb+fitted(res.ML)[1:30]),
                         transf.ilogit(pi.ub+fitted(res.ML)[1:30])
                         )]
GS <- M[age=='0-4',
        .(mid2=sum(male,na.rm=TRUE)/sum(tot,na.rm=TRUE)),
        by=iso3]
F <- merge(F,GS,by='iso3')

ggplot(F,aes(mid2,y=mid,ymin=lo,ymax=hi))+
  geom_point()+geom_errorbar()+
  geom_abline(slope=1,intercept = 0,col=2)
## some shrinkage

## loop over for output
agz <- c('0-4','5-14','15+')
fans <- ans <- list()
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
  ## fits/predicts
  F <- ranef(res.ML)$iso3
  F <- as.data.table(F)
  F$iso3 <- rownames(ranef(res.ML)$iso3)
  F[,c('mid','lo','hi'):=.(transf.ilogit(intrcpt+
                                         fitted(res.ML)[1:nrow(F)]),
                           transf.ilogit(pi.lb+
                                         fitted(res.ML)[1:nrow(F)]),
                           transf.ilogit(pi.ub+
                                         fitted(res.ML)[1:nrow(F)])
                           )]
  F[,age:=ag]
  fans[[ag]] <- F
}
ans <- rbindlist(ans)
fans <- rbindlist(fans)


fwrite(ans,file=here('plots/sexmetaresults.csv'))


M[,c('mid','lo','hi'):=.(male/tot,NA_real_,NA_real_)]
fans$age <- factor(fans$age,levels=c('0-4','5-14','15+'),ordered = TRUE)
M$age <- factor(M$age,levels=c('0-4','5-14','15+'),ordered = TRUE)
M$iso3 <- factor(M$iso3)
fans$iso3 <- factor(fans$iso3)

## plot
ggplot(fans,aes(x=iso3,y=mid,ymin=lo,ymax=hi))+
  geom_point(data=M,shape=1,alpha=0.5)+
  geom_errorbar(width=0,col="blue")+geom_point(col="blue")+
  scale_y_continuous(label=scales::percent,limits=c(0.25,0.75))+
  ## NOTE this misses 2 LBR points
  facet_wrap(~age)+coord_flip()+
  theme_light()+
  geom_hline(yintercept=0.5,col=2)+
  ylab('Proportion of notifications from males')

ggsave(file=here('plots/forest.png'),w=10,h=7)
ggsave(file=here('plots/forest.pdf'),w=10,h=7)
