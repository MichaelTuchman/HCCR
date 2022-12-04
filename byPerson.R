## post analyses
STEP3_HCC=STEP3[,.SD,.SDcols=c('pat_id',HCCvars$HCC)]
STEPA=merge(STEP3_HCC,STEP8[,.(pat_id,Silver)])
setkey(STEPA,pat_id)

STEPA[,level:=cut(Silver,breaks=c(1,5,10,20,30,40,50,60,70,80,90,100,Inf))]

require(ggplot2)

ggplot(STEPA[Silver<125],aes(x=Silver,group=level)) + geom_histogram()

summary(STEPA$Silver)


A=Answer[,.(pat_id,Silver,cut(Silver,breaks=c(-Inf,1,5,seq(10,125,5))))]
