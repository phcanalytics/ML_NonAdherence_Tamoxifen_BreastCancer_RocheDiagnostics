# Purpose:       Create response variable pdc1flag to indicate adherence to Tamoxifen during follow-up     
# 
# INPUT FILES: <List all datasets used within the program>
#
#  f_extract_d_post_365days, drug_codes
#
# OUTPUT FILES: <List all files created by the program>
#
#  adherdata
# --------------------------------------------------------------------------

#####################
# Parameters set up #
#####################

studyNo <- "rwddia_328"  # project ID
setwd(paste0("/opt/bee/analyses/RWDDIA/",studyNo))

# Define required libraries and increase RAM to 8GBs to avoid OUT OF MEMORY ERROR
options(java.parameters="-Xmx8192m")
libVector <- c("RocheTeradata","data.table","stringi","stringr","Rcpp", "scales","compare", "dplyr","readxl","xlsx", "ggplot2")               
lapply(libVector, require, character.only=T)
options(tibble.width=Inf)

connect_teradata(datalab=studyNo, pwd=readLines("~/.ssh/.tdpass"))

tamox_365 <- data.table(query_teradata("
SELECT enrolid, indexdt, svcdate as svcdt, sum(daysupp) as daysupp
FROM f_extract_d_post_365days
WHERE ndcnum IN (SELECT ndccode FROM drug_codes WHERE gennme='Tamoxifen Citrate')
GROUP BY enrolid, indexdt, svcdt
HAVING sum(daysupp)>0
ORDER BY enrolid, svcdt") %>% 
rename_all(tolower) %>%
mutate_at(vars(contains('dt')), as.Date, format='%Y-%m-%d') %>%
# cutoff last prescription date (rxendtdt) to 365-days post-index
mutate(rxstartdt=svcdt, rxendtdt=pmin(svcdt+daysupp, indexdt+365)) %>%
group_by(enrolid) %>%
mutate(nextrxstartdt=lead(rxstartdt, n=1)) %>%
mutate(daysdiff_orig=pmin(365,as.numeric(lead(rxstartdt, n=1)-rxstartdt))))

tamox_365[rxendtdt>=nextrxstartdt,rxendtdt:=nextrxstartdt-1]
# Calculate days on drug by removing the overlapping supplied days
tamox_365[,daysondrug:=as.numeric(rxendtdt-rxstartdt)]
tamox_365[,daysdiff:=as.numeric(nextrxstartdt-rxendtdt)]
tamox_365[is.na(daysdiff), daysdiff:=0]
tamox_365[is.na(daysdiff_orig), daysdiff_orig:=0]

adherdata <- tamox_365[, .(pdc1=sum(daysondrug)/365), by=enrolid]

# Patients with <80% proportion of days coverage (PDC) in the year 
# following treatment initiation were classified non-adherent
adherdata[,pdc1flag:=ifelse(pdc1>=0.8, 1, 0)]

