RPCGR=dbGetQuery(con,qry.pcg) %>% data.table
setkey(PCGR,proc_cd)



# assign groups to procedure codes so that we only match within
# major categories

PCGR=PCGR %>% mutate(G1=grp_proc_cd(proc_cd),G2=grp_proc_cd(range_start))
PCGR_Resolve=PCGR[G1==G2]

TROUBLE=PCGR %>% anti_join(PCGR_Resolve,by='proc_cd')
# add covid codes to list


## override ranges for office visits and remote patient management
## we won't use them.

# RED FLAG: THIS IS A RISK. WE MINIMIZE IT BY ONLY APPLYING
# THIS LOGIC TO LESSER KNOWN CODES.

L_PC= PCGRCV19[in_range(proc_cd,range_start,range_end)] %>% 
  dkey('proc_cd') %>% filter(dupkeyctr==1) %>% select(proc_cd,section) 

# assign section number variables to make PCA easier

SN=L_PC %>% count(section) %>% mutate(proc_grp=paste('prcg_',1:nrow(.),sep=''))
L_PC=L_PC %>% merge(SN,by='section')


# data new hcpcs codes
R1=dbGetQuery(con,range1.qry) %>% data.table
NH=fread('missing_codes_fix.csv',colClasses = c('character','character'))
R1=rbind(R1,NH,fill=TRUE) %>% select(code,short_desc) %>% data.table
R1=R1[,.(code,short_desc)] %>% distinct %>% filter(str_length(code)==5) %>% mutate(g1=grp_proc_cd(code),k1=str_sub(code,1,3))
setkey(R1,g1,k1)
