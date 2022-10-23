#############################################################
## Program: score_model.R
## Combine functions created by data munging spreadsheet
## with actual data.  
##
## Dependencies: 
##  assign_hcc 
##  widenfb
##  more_vars
##  AgeSexModel
##
## is that they are developed only using the data from
## the excel sheet.  This should be the first place
## the code touches actual coalims data.
##
## might load those as a package, but I am waiting for
## other things to happen first
#############################################################
METAL_LEVEL='Platinum'    # measure for how cushy a plan is
                          # used for ACA marketplace plans
                          # we re basically just using as
                          # placeholder to assume a cushy plan

Scored_DM=AgeSexModel(DM[,
                         .(pat_id,
                           pat_gender,
                           pat_age=round(age_rpt))])



#############################################################
## Assign HCC to diagnostic codes
#############################################################

AHCCf=assign_hcc(HCC2)  # this will be our exported function
STEP2=AHCCf(D3) 

#############################################################
## Add Model Variable based on
#############################################################


STEP2A=merge(STEP2,Scored_DM,by='pat_id')
STEP2A=STEP2A[,.(pat_id,
                 pat_age=pat_age.x,
                 pat_gender=pat_gender.x,
                 HCC,
                 AgeBAND=Variable)]  

STEP2A=merge(STEP2A,AgeSexBands[,.(AgeBAND=Variable,Model)],by='AgeBAND')

rpt=function(DT) {DT[,.(pat_id,Model,AgeBAND,pat_gender,AGE_LAST,HCC)][order(pat_id)]}

f=widenfb(HCC2) # all these function builders must take spreadsheet tables as arguments
STEP3=f(STEP2A)

## note that AGE0_MALE and AGE1_MALE act like both rows and columns
## in this program.  This will become important later. It occurs
## to me while I'm writing this that we need age variables in 
## the HCC dataset as well as HCC variables.  Then we can


STEP3[,`:=`(AGE_LAST=pat_age,
            AGE0_MALE=as.integer(AgeBAND=='AGE0_MALE'),
            AGE1_MALE=as.integer(AgeBAND=='AGE1_MALE'))]



# temporary while we fix eligibility, which we kinda need to do


## RED FLAG: HARDCODE. step3 : AGE_LABELS AGE0_MALE, AND AGE1_MALE
## THESE SHOULD BE READ IN DIRECTLY FROM A SPREADSHEET
## BUT WE CAN FIX THIS LATER SINCE THE CATEGORIES ARE PRETTY CONSTANT YEAR TO 
## YEAR

## LOOK AT WHERE THE AGE/SEX FACTORS ARE DEFINED AND WHY THIS WASN'T SET THERE
## this feels hardcoded to me.   What if we inserted some female categories?
## right now, the program is assuming HHS is only looking at age 0,1 Males

## The root issue is that we're treating the age sex variables as different
## from the HCC variables.  However, AGE0_MALE and AGE1_MALE play a direct
## role in both the age/sex AND the regression model.
## this assignment needs to be moved into the more_vars function


## it is not necessary to remove NA at this point.
STEP4 = more_vars(STEP3)
varnames=copy(names(STEP4))
ids = c('pat_id','pat_age','pat_gender','AgeBAND','Model')
measures=setdiff(varnames,ids)

## YELLOW: 
## convert doubles to ints.  don't know why these fell out as doubles
## we need to enforce types earlier in the process

## now melt and score
## ignore the issue with ints being converted to doubles
## when converting to SQL we will need to be able to pivot longer
## these should be abstract functions and not immediately
## executable statements, perhaps?

## developer note:
## if we were doing this in SQL, the functions more_vars,assign_hcc,widenfb would
## all be views on SQL tables.  This means that either the target language
## is a parameter, and perhaps we have method dispatch on target language?


STEP5=melt.data.table(STEP4,ids,measures,na.rm=TRUE)

### Get coefficients
### again, make these functions?
### step6 = function(STEP5, etc.)
### this cartesian jo

## RED FLAG : hardcode metal value to avoid cartesian join
## but there are other, better ways, even using data table
## task: look into these

STEP6=merge(STEP5,ModelFactors[Metal==METAL_LEVEL],by.x=c('variable','Model'),
                              ,by.y=c('Variable','Model'))

## step 7 is useful for seeing how a score breaks down for a particular person

STEP7=STEP6[,.(pat_id,Model,variable,pat_age,pat_gender,AgeBAND,value,Coeff,variable,y=value*Coeff)][y!=0]

STEP8 = STEP7[,sum(y),by=.(pat_id,Model,pat_age,pat_gender,AgeBAND)]

# merge the Age Sex Enrollment variable in

STEP9 = merge(STEP8,ModelFactors[Metal==METAL_LEVEL],by.x='AgeBAND',by.y='Variable')
STEP9=STEP9[,.(pat_id,Model=Model.x,pat_age,pat_gender,V1,AgeBAND,AgeSexScore=Coeff,HCC_RISK=V1+Coeff)]



## yellow flag: temporary fix
## 

## i fwe add age score and risk score, we will be overcounting
## for age0_male and age1_male

STEP9[AgeBAND %like% 'AGE[01]',HCC_RISK:=V1]

## 
## expand to include age and eligibility
## assume A has ENROLDURATION and AgeBand
## columns

noClaims=function(A) {
  merge(A,ModelFactors,by='Variable')
}

HasClaims=STEP2A[,NA,by=pat_id]
setkey(HasClaims,pat_id)
NC=Scored_DM[!HasClaims,.SD,by=pat_id]
N2C= merge(NC,ModelFactors[Metal==METAL_LEVEL],by='Variable')
N2C=N2C[,.(pat_id,Model,pat_age,pat_gender,V1=NA,AgeBAND=Variable,AgeSexScore=Coeff,HCC_RISK=Coeff)]

# FinalScoring

Answer=bind_rows(STEP9,N2C)
