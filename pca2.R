
## remove things that are now out of range


D## all sections should have a minimum number of claims
dim(DPS)

## PC only table


MDPC = DPS[,.(rend_npi,proc_cd,proc_cd_desc,scode,rend_taxonomy_cd_desc)]
MDPC[,whichsample:=sample(1:10,.N,replace=TRUE)]

wide_ds=function(MDPC,threshold=500) {
  USE=MDPC[,sum(dummy),by=proc_grp][V1>=threshold] %>% lbl
  ASET=MDPC %>% inner_join(USE,by='proc_grp')
  CAST1=ASET%>% dcast.data.table(rend_npi+rend_taxonomy_cd_desc~proc_grp,fill=0,value.var='dummy',fun.aggregate = sum)
  return(CAST1)
}

###########################################################################
## do the PC analysis 
###########################################################################


analysis_set=wide_ds(MDPC[whichsample==1]) # USE 10% OF DATA ONLY
numeric_columns=function(DT) {
  DT[,.SD,.SDcols=is.numeric]
}

f = function(i,...) {
  MDPC[whichsample==i] %>% wide_ds(threshold=500) %>% numeric_columns %>% prcomp(scale.=TRUE,...)
}

models=lapply(1:3,f,rank=40)

score_data = function(DT) {
  scores= DT %>% select(contains('prcg')) %>% predict(models[[1]],newdata=.)
  output=cbind(data,scores)
  return(output)
}


wide_ds=function(MDPC,threshold=500) {
  USE=MDPC[,sum(dummy),by=proc_grp][V1>=threshold] %>% lbl
  ASET=MDPC %>% inner_join(USE,by='proc_grp')
  CAST1=ASET%>% dcast.data.table(rend_npi+rend_taxonomy_cd_desc~proc_grp,fill=0,value.var='dummy',fun.aggregate = sum)
  return(CAST1)
}

MDPC = DPS[,.(rend_npi,rend_taxonomy_cd_desc,scode,rowid=1:.N)] %>% slice_sample(prop=.30)
train_indexes=MDPC[,.(rowid)]
# filter down

X0=MDPC %>% group_by(scode) %>% summarize(a=sum(N)) %>% arrange(desc(a))
# well populated procedure codes
WPPC=X0[X0$a>99,] %>% data.table(key='scode')


# keep good vars
KGV=DPS %>% left_join(WPPC,by='scode')
KGV[is.na(a),scode:='sec_oth']

# wide
kgvWide=KGV %>% dcast.data.table(rend_npi+rend_taxonomy_cd_desc~scode,
                                 fun.aggregate=length,
                                 value.var='V1')


# cluster

# plots

# histograms

# now we can go wide
KGVWide = KGV %>% dcast.data.table(rend_npi~rend_taxonomy_cd_desc~scode,fill=0,value.var='N',fun.aggregate = 'sum')
WPPC2=WPPC %>% mutate(varindex=as.integer(str_sub(scode,5,-1))) %>% arrange(varindex)
numericVars=WPPC2$scode

# 
obj1=kgvWide[,.SD,.SDcols=numericVars] %>% prcomp(scale.=TRUE,center=FALSE)
AA %>% relocate(scode:section,before=PC1) %>% select(-V1)

obj=kgvWide[,.SD,.SDcols=numericVars] %>% prcomp(scale.=TRUE,rank=30)
# take first 55 components, for demonstration purposes

AA=as.data.frame(A) %>% mutate(scode=rownames(A)) %>% left_join(SecMatch,on='scode')
A3=AA %>% relocate(scode:section,.before=PC1) %>% select(-V1)

## histograms

# cor
covMatrix = kgvWide[,.SD,.SDcols=numericVars] %>% cov
hclObj=hclust(covMatrix)
