## AgeSexFactors.R
## Add risk scores for no HCCs, age and sex

## report date went missing

# DM=data.table(readRDS('DM.RData'))

enclose_braces=function(str) paste("{",str,"}")


ProgramStatements = AgeSexBands %>% separate(Variable,sep='_',into=c('c1','c2','c3','c4'),remove=FALSE) %>% 
       mutate(sex=str_sub(c1,1,1)) %>% select(c(1:7,10)) %>%
      select(-Description) %>% data.table 

ProgramStatements[,`:=`(sex=ifelse(sex %in% c('A','S'),NA,sex))]


ProgramStatements[,operator:='IS BETWEEN']
ProgramStatements[c4=='GT',`:=`(c4=Inf,operator='>=')]

ProgramStatements[grep('AGE.',c1),`:=`(lo=as.numeric(str_sub(c1,4,4)),operator='EQ')]

ProgramStatements[Model=='Infant',`:=`(c3=as.numeric(str_sub(c1,4,4)),hi=NA,sex=str_sub(c2,1,1))]  

# need list of variable names here

ProgramStatements[operator %in% c('>=','EQ'),`:=`(sql1=paste('case when pat_age ',
                                            operator,' ',c3,' and ',
                                            "pat_gender='",
                                            str_trim(sex),
                                            "' then 1 else 0 end as "
                                            ,Variable,sep=''),
                                  
                                  sql2=paste('when pat_age ',
                                            operator,' ',c3,' and ',
                                            "pat_gender='",
                                            str_trim(sex),
                                            "' then '",Variable,"' \n",sep=''))]
  

ProgramStatements[operator=='IS BETWEEN',`:=`(sql1=paste('case when pat_age is between ',
                                        c3,' and ',c4,
                                        " and pat_gender='",
                                        sex,
                                        "' J$then 1 else 0 end as ",
                                        Variable,sep=''),
                              
                              sql2=paste('when pat_age is between ',
                                         c3,' and ',c4,
                                         " and pat_sex='",
                                         sex,
                                         "' then '",Variable,"'\n",
                                         sep=''))]


ProgramStatements[,`:=`(lo=as.integer(c3),hi=as.integer(c4))]
ProgramStatements[operator=='EQ',`:=`(hi=lo)]

##
## create age conditions as strings for data table conditional assignment
##
## we can generalize this so we're not necessarily using pat_age and pat_gender
## as the variables
##

ProgramStatements=ProgramStatements[Model!='Adult, Child, Infant']
ProgramStatements[operator=='IS BETWEEN',`:=`(dt1=paste("pat_age<=",hi," & pat_age>=",lo))]
ProgramStatements[operator=='>=',`:=`(dt1=paste("pat_age>=",lo))]
ProgramStatements[operator=='EQ',`:=`(dt1=paste("pat_age==",lo))]
ProgramStatements[,`:=`(dt1=paste("pat_gender=='",sex,"' & (",dt1,")"))]
ProgramStatements[,`:=`(dt1=gsub(' M ','M',dt1))]
ProgramStatements[,`:=`(dt1=gsub(' F ','F',dt1))]

## create assignment of labels

ProgramStatements[,`:=`(dt2=paste("Variable='",Variable,"'",sep=''))]

# for now, just use data table related program statements

K=ProgramStatements[,.(Model,antecedent=dt1,
                       assignment=paste("X[",dt1,",`:=`(",dt2,")]"))]

## build a block of statements, each one a data table conditional assignment

## e2 variable is statements as strings, wrap in parens
## hardcoded variable names YELLOW FLAG
AgeSexModel=setterhl(K)
## convert e2 into an R expression that can be evaluated

## yellow flag:  We are not using the AGE_LAST variable




## Todo: Chain functions together
## list functions to chain
## adding additional years to the model
## preprocessing of input tables, document
## requirements for RxVars

AgeSexBands=data.table(AgeSexBands)
