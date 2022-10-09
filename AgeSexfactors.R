## AgeSexFactors.R
## Add risk scores for no HCCs, age and sex

DM=data.table(readRDS('DM.RData'))
DM[,pat_age:=round(age_rpt)]

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

ProgramStatements[,`:=`(dt2=paste("Variable:='",Variable,"'",sep=''))]

# for now, just use data table related program statements

K=ProgramStatements[,.(Model,dt1,dt2)]

## build a block of statements, each one a data table conditional assignment

## e2 variable is statements as strings, wrap in parens
## hardcoded variable names YELLOW FLAG
e2=paste("DM[",K$dt1,',',K$dt2,"]") %>% paste(collapse=';')
e2=paste(e2,"return(DM)",sep=";") %>% enclose_braces()
## convert e2 into an R expression that can be evaluated

DT_program=function(DM){}
body(DT_program)=parse_expr(e2)

Scored_DM=DT_program(DM[,.(pat_id,pat_gender,pat_age=age_rpt)])
                     

## run the statements.  Disadvantage is that this isn't very interpretable,
## since the program is hidden inside a string.  But at least now we
## see that DT_program is a function that takes a demographic table as input


## it's also not clear what inputs and outputs the program e2 actually takes
## it might be easier to build e2 as a function call so it's easier to see


e4=eval(DT_program(DM))

## now E4 has HMS_CC compatible variables. 
## One need only merge with the ModelFactors
## data set to add risk scores!



## result is that we now have a score for each variable type and metal level
## i may have made this too complicated.  All I needed was a function of 
## age, gender, and age/gender excel table

## but as my variables get more numerous, this may be OK

## current DM has some duplicate keys. it does not matter for this testing.

## final result should really be a function, not a table

## simulate eligibility
ev=runif(nrow(DM)) %>% cut(c(.05,.20,.21,.22,.23,.24,.25,.26,.27,.28,.29,1,+Inf))
levels(ev)=paste('ED_',1:12,sep='')
ev[is.na(ev)]='ED_12'

DM$ME=ev

# Simulate Metal Level.  Decided on one level per person rather
# than computing all 5 for each person, which was causing me issues
# with cartesian joins

Metals=ModelFactors$Metal%>%unique
DM$Metal=sample(Metals,nrow(DM),replace=TRUE,prob = c(5,4,3,2,.5))

# get model name (child, infant, adult)

# merge partial eligibility risk scores in and store them in variable 'EC'

DM2=merge(DM,ELIG[,.(Variable,EC=Coeff,Metal)],by.x=c('ME','Metal'),by.y=c('Variable','Metal'))

 

