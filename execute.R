#############################################################
## execute our new functions with diagnosis data
#############################################################

AHCCf=assign_hcc(HCC2)  # this will be our exported function
STEP2=AHCCf(D3) 
f=widenfb(HCC2) # all these function builders must take spreadsheet tables as arguments
STEP3=f(STEP2)
# temporary while we fix eligibility, which we kinda need to do
STEP3[,ENROLDURATION:=12] 

## RED FLAG: HARDCODE. step3 : AGE_LABELS AGE0_MALE, AND AGE1_MALE
## THESE SHOULD BE READ IN DIRECTLY FROM A SPREADSHEET
## BUT WE CAN FIX THIS LATER SINCE THE CATEGORIES ARE PRETTY CONSTANT YEAR TO 
## YEAR

## LOOK AT WHERE THE AGE/SEX FACTORS ARE DEFINED AND WHY THIS WASN'T SET THERE

STEP3[,`:=`(AGE_LAST=pat_age,
            AGE0_MALE=as.numeric(pat_age==0 & pat_gender=='M'),
            AGE1_MALE=as.numeric(pat_age==1 & pat_gender=='M'))]

STEP4 = more_vars(STEP3)
  
a=names(STEP3)
col_names=a[grep('pat_',a,invert=TRUE)]
## write a function that replaces all numeric NA with zero

STEP3=STEP3[,lapply(.SD,replace_na,0),.SDcols=col_names]

STEP4=more_vars(STEP3)
STEP3=STEP3[,.SD,.SDcols=col_names]

count_na=function(v) sum(is.na(v))
checkNA=function(DT) DT[,lapply(.SD,count_na)]

