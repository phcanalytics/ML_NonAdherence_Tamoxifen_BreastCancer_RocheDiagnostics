# Purpose:       Cohort creation      
# Programer:     Thanos Siadimas [thanos.siadimas@roche.com]
# 
# INPUT FILES: <List all datasets used within the program>
#
#  [RWD_VDM_CCAE] V02_CCAE_S, V02_CCAE_0, V02_CCAE_T, V02_CCAE_D, V02_CCAE_F - Commercial Claims & Encounters 
#  [RWD_VDM_MDCR] V02_MDCR_S, V02_MDCR_0, V02_MDCR_T, V02_MDCR_D, V02_MDCR_F - Medicaire Supplement
#  [rwddia_212]   drug_codes
# 
# OUTPUT FILES: <List all files created by the program>
#
#  [rwddia_328] f_cohort, f_extract_d_post_365days, f_primary_care_visits_365d
#
# --------------------------------------------------------------------------

 #####################
 # Parameters set up #
 #####################
 studyNo       <- "rwddia_328"  # project ID
 spstdt        <- "2012-01-01"  # start date of study period            
 spendt        <- "2018-12-31"  # end   date of study period            
 idpstdt       <- "2013-01-01"  # start date of identification period   
 idpendt       <- "2017-12-31"  # end   date of identification period  
 enrollmentGap <- c(7)         # allowable gap for enrollment discontinuation
 valid_date    <- '2019-10-28'
 
 suffix        <- ""            # suffix teradata tables
 setwd(paste0("/opt/bee/analyses/RWDDIA/",studyNo))
 
 # Define required libraries and increase RAM to 8GBs to avoid OUT OF MEMORY ERROR
 options(java.parameters = "-Xmx8192m")
 libVector <- c("RocheTeradata","data.table","stringi","stringr","profvis",
                "compare", "dplyr","readxl", "xlsx", "tidyr")         
 
 lapply(libVector, require, character.only=T)
 options(tibble.width=Inf)
 
 connect_teradata(datalab=studyNo, pwd=readLines("~/.ssh/.tdpass"))
   
 primDiagCode          <- c("174%","C50%")
 primDiagCodeNoBreast  <- c("140%","141%","142%","143%","144%","145%","146%","147%","148%","149%","150%","151%","152%","153%","154%",
                           "155%","156%","157%","158%","159%","160%","161%","162%","163%","164%","165%","170%","171%","176%","179%",
                           "180%","181%","182%","183%","184%","188%","189%","190%","191%","192%","193%","194%","195%","199.2","200%",
                           "201%","202%","203%","204%","205%","206%","207%","208%","209%",
                           "C00%","C01","C02%","C03%","C04%","C05%","C06%","C07","C08%","C09%","C10%","C11%","C12","C13%","C14%","C15%",
                           "C16%","C17%","C18%","C19","C20","C210","C211","C218","C220","C221","C222","C227","C228","C229","C23","C24%",
                           "C25%","C26%","C48%","C30%","C31%","C32%","C33","C3400","C3410","C342","C3430","C3490","C3480","C35%","C36%",
                           "C37","C380","C381","C382","C383","C388","C384","C39%","C4000","C4010","C4020","C4030","C41%","C478","C490",
                           "C4910","C4920","C493","C494","C495","C496","C498","C499","C462","C464","C4650","C463","C467","C469","C510",
                           "C511","C512","C519","C52","C53%","C54%","C55","C569","C5700","C5710","C573","C5720","C574","C577","C578","C579",
                           "C58","C649","C659","C67%","C680","C681","C688","C689","C6940","C6960","C6950","C6900","C6910","C6920","C6930",
                           "6980","C6990","C700","C709","C720","C721","C701","C729","C7250","C71%","C73","C74%","C75%","C76%","C802",
                           "C831%","C833%","C835%","C837%","C838%","C846%","C847%","C810%","C811%","C812%","C813%","C814%","C817%","C819%",
                           "C829%","C840%","C841%","C96A%","C9140","C9141","C960","C962","C96Z","C844%","C858%","C849%","C964","C969","C900%",
                           "C901%","C888","C902%","C903%","C910%","C911%","C91Z0","C91Z1","C91Z2","C919%","C920%","C922%","C924%","C925%",
                           "C838%","C922%","C923%","C924%","C925%","C929%","C92Z0","C92Z1","C92Z2","C930%","C931%","C939%","C93Z0","C93Z1",
                           "C93Z2","C940%","C942%","C943%","D45","C959%","C950%","C951%","C7A019","C7A010","C7A011","C7A012","C7A029","C7A020",
                           "C7A021","C7A022","C7A023","C7A024","C7A025","C7A026","C7A00","C7A090","C7A091","C7A092","C7A093","C7A094","C7A095",
                           "C7A096","C7A097","C7A098","C7A1","C4A%","D3A%","C7B00","C7B01","C7B02","C7B03","C7B04","C7B1","C7B09","C7B8")

 metastatDiagCode  <- c(
  "1960","1962","1963","1965", "1966", "1968", "1969", "1970", "1971", "1972", "1973", "1974", 
  "1975", "1976", "1977", "1978", "1980", "1981", "1983", "1984", "1985", "1986","1987", "19882", "19889","C770","C771",
  "C772","C773","C774","C775","C778","C779","C7800","C7801","C7802","C781","C782",
  "C7830","C7839","C784","C785","C786","C787","C7880","C7889","C7900","C901","C902",
  "C910","C7911","C7919","C792","C7931","C7932","C7940","C7949","C7951","C7952","C7960","C7961","C7962","C7970","C7982","C7989")

# drug_codes <- data.table(read_excel("/opt/bee/analyses/RWDDIA/rwddia_328/import/drug_codes.xlsx"))
# write_teradata(dbname=studyNo, tablename="drug_codes", df=drug_codes, overwrite=T)

 # Convert parameters to Teradata format
 primDiagCode         <- toString(paste0("'",str_replace(primDiagCode,"\\.", ""),"'"))
 primDiagCodeNoBreast <- toString(paste0("'",str_replace(primDiagCodeNoBreast,"\\.", ""),"'"))
 metastatDiagCode     <- toString(paste0("'",str_replace(metastatDiagCode,"\\.", ""),"'"))
 spstyear   <- as.numeric(format(as.Date(spstdt, format ="%Y-%m-%d"),'%Y'))
 spendyear  <- as.numeric(format(as.Date(spendt, format ="%Y-%m-%d"),'%Y'))
 idpstyear  <- as.numeric(format(as.Date(idpstdt, format ="%Y-%m-%d"),'%Y'))
 idpendyear <- as.numeric(format(as.Date(idpendt, format ="%Y-%m-%d"),'%Y'))

 
 ############################################
 # Count totalpatients in Truven MarketScan #
 ############################################
 
 # count persons in database. Overall and by sub-database ( CCAE and MDCR )
 query_teradata("
 SELECT z.db, count(distinct z.enrolid) as nof_enrolid
 FROM (
       SELECT x.enrolid, x.year_, x.db
       FROM
           (
            SELECT distinct enrolid, year_, 'ccae' as db
            FROM rwd_vdm_ccae.v02_ccae_a
            WHERE DATE '",valid_date,"' BETWEEN valid_start AND valid_end
            AND year_ BETWEEN 2012 AND 2017 AND INDEX(VERSION_,'E') = 0 
            UNION ALL                
            SELECT distinct enrolid, year_, 'mdcr' as db
            FROM rwd_vdm_mdcr.v02_mdcr_a
            WHERE DATE '",valid_date,"' BETWEEN valid_start AND valid_end
            AND year_ BETWEEN 2012 AND 2017 AND INDEX(VERSION_,'E') = 0 
           ) x
              QUALIFY ROW_NUMBER() OVER (PARTITION BY x.enrolid ORDER BY x.year_)=1
 ) z 
 GROUP BY rollup(z.db)") %>%
 transmute(
    db=if_else(is.na(db),"total",db),
    nof_enrolid=paste0(format(nof_enrolid, big.mark=","), 
                       " (", round(100*nof_enrolid/max(nof_enrolid)), "%)")) %>%
 spread(.,db,nof_enrolid)
 
 # ccae              mdcr            total             
 # 98,447,516 (94%)  5,992,374 (6%)  104,439,890 (100%)
 
 # report persons enrolled in Truven during 2012-2017, by gender
 query_teradata("
 SELECT sex, count(distinct enrolid) as nof_enrolid
  FROM 
      (
       SELECT distinct enrolid,sex
       FROM rwd_vdm_ccae.v02_ccae_a
       WHERE DATE '",valid_date,"' BETWEEN valid_start AND valid_end
       AND year_ BETWEEN 2012 AND 2017 AND INDEX(VERSION_,'E') = 0
       UNION 
       SELECT distinct enrolid,sex
       FROM rwd_vdm_mdcr.v02_mdcr_a
       WHERE DATE '",valid_date,"' BETWEEN valid_start AND valid_end
       AND year_ BETWEEN 2012 AND 2017 AND INDEX(VERSION_,'E') = 0 
       ) z 
   GROUP BY rollup(z.sex) ") %>%
    rename_all(tolower) %>%
    transmute(
       gender=case_when (is.na(sex) ~ 'Total', # na was generated from teradata and denoted the overall counts  
                         sex==1     ~ 'Male',  # sex was either 1 for Males or 2 for Females
                         sex==2     ~ 'Female'),
       nof_perc_enrolid=paste0(format(nof_enrolid, big.mark=","), 
                          " (", round(100*nof_enrolid/max(nof_enrolid)), "%)"))
 
 # gender nof_perc_enrolid    
 # Male   " 50,874,362 (49%)" 
 # Female " 53,565,528 (51%)" 
 # Total  "104,439,890 (100%)"
 
#################
# 1st criterion #
#################
 
# Women who received one or more incident FDA-approved oral chemotherapy 
# indicated for advanced breast cancer (Stage III or greater)                   
# between 1 January 2013 and 29DEC2017 are eligible for inclusion. 
# The date of service of the first oral chemotherapy prescription claim will be assigned the index date

 drop_teradata("f_crit1")
 update_teradata(paste0("
 CREATE TABLE f_crit1 AS (
 SELECT t3.enrolid, t3.svcdate as indexdt, t3.age as indexage 
 
 FROM (
 
 /* Extract CCAE drug (d) table */
 SELECT DISTINCT t1.enrolid, t1.svcdate, t1.age, t1.daysupp,t1.sex,t1.ndcnum
 FROM rwd_vdm_ccae.v02_ccae_d t1, drugCodes_tamoxifen t2
 WHERE (t1.svcdate BETWEEN '",idpstdt,"' AND '",idpendt,"')
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND	INDEX(VERSION_,'E') = 0
 AND t1.ndcnum=t2.ndccode
 AND t1.daysupp>0
 AND t1.sex=2
 AND t1.enrolid IS NOT NULL

 UNION
 
 /* Extract MDCR drug (d) table */
 SELECT DISTINCT t1.enrolid, t1.svcdate, t1.age, t1.daysupp,t1.sex,t1.ndcnum
 FROM rwd_vdm_mdcr.v02_mdcr_d t1, drugCodes_tamoxifen t2 
 WHERE (t1.svcdate BETWEEN '",idpstdt,"' AND '",idpendt,"')
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 AND t1.ndcnum=t2.ndccode
 AND t1.daysupp>0
 AND t1.sex=2
 AND t1.enrolid IS NOT NULL
 
 ) t3 QUALIFY ROW_NUMBER() OVER (PARTITION BY t3.enrolid ORDER BY t3.svcdate)=1
 ) WITH DATA;")) 

 query_teradata("SELECT count(*), count(distinct enrolid) FROM f_crit1")
 
 ###########################
 # 2nd criterion - enrolment  
 ###########################
 
 # patients with continuous enrollment (<=7days gap) during the 365-day pre-index period
 # & (<=7days gap) during the 365-day post-index period 
 
 # Pull enrollment records
 drop_teradata("f_extract_t")
 update_teradata(paste0("
 CREATE TABLE f_extract_t AS (
 
 SELECT DISTINCT enrolid, dtstart, dtend, rx
 FROM  rwd_vdm_ccae.v02_ccae_t
 WHERE DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND	 INDEX(VERSION_,'E') = 0
 AND   enrolid IN (SELECT enrolid FROM f_crit1)
 AND   (year_ BETWEEN ",spstyear-1," AND ",spendyear,")
 AND   rx='1'
 
 UNION
 
 SELECT DISTINCT enrolid, dtstart, dtend, rx
 FROM  rwd_vdm_mdcr.v02_mdcr_t
 WHERE DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND	 INDEX(VERSION_,'E') = 0
 AND   enrolid IN (SELECT enrolid FROM f_crit1)
 AND   (year_ BETWEEN ",spstyear-1," AND ",spendyear,")
 AND   rx='1'
 
 ) WITH DATA;"))
 
 # Combine continuous enrollment episodes
 drop_teradata("f_enrol_episodes")
 update_teradata(paste0("
 CREATE TABLE f_enrol_episodes AS (
 SELECT t5.enrolid, MIN(t5.dtstart) AS dtstart, MAX(t5.dtend) dtend
 FROM
 (
 SELECT t4.enrolid, t4.dtstart, t4.dtend,
 SUM(t4.ceStop_flag) OVER (PARTITION BY t4.enrolid ORDER BY t4.dtstart, t4.dtend DESC ROWS UNBOUNDED PRECEDING) AS groupBy_flag
 FROM
 (
 SELECT enrolid, dtstart, dtend,
 CASE WHEN COALESCE(MAX(dtend) OVER (PARTITION BY enrolid ORDER BY dtstart,dtend DESC ROWS BETWEEN 1 PRECEDING AND 1 PRECEDING), CAST(DATE '2099-12-31' AS DATE)) - dtstart +1 + ",enrollmentGap," >= 0
 THEN 0 ELSE 1
 END AS ceStop_flag
 FROM
 (
 SELECT t2.enrolid, MIN(t2.dtstart) AS dtstart, MAX(t2.dtend) dtend
 FROM
 (
 SELECT t1.enrolid, t1.dtstart, t1.dtend,
 SUM(t1.ceStop_flag) OVER (PARTITION BY t1.enrolid ORDER BY t1.dtstart, t1.dtend DESC ROWS UNBOUNDED PRECEDING) AS groupBy_flag
 FROM
 (
 SELECT enrolid, dtstart, dtend,
 CASE WHEN COALESCE(MAX(dtend) OVER (PARTITION BY enrolid ORDER BY dtstart,dtend DESC ROWS BETWEEN 1 PRECEDING AND 1 PRECEDING), CAST(DATE '2099-12-31' AS DATE)) - dtstart +1 + ",enrollmentGap," >= 0
 THEN 0 ELSE 1
 END AS ceStop_flag
 FROM f_extract_t
 )t1
 )t2
 GROUP BY t2.enrolid, t2.groupBy_flag
 ) t3
 ) t4
 )t5
 GROUP BY t5.enrolid, t5.groupBy_flag
 ) WITH DATA;"))
 
 drop_teradata("f_enrol_flags")
 update_teradata("
 CREATE TABLE f_enrol_flags AS(
 SELECT t1.enrolid,t1.dtstart, t1.dtend, t2.indexdt,
 CASE WHEN (t2.indexdt-t1.dtstart>=365) AND (t1.dtend-t2.indexdt>=-1+365) THEN 1 ELSE 0 END AS critfn2
 FROM f_enrol_episodes t1, f_crit1 t2
 WHERE t1.enrolid=t2.enrolid
 ) WITH DATA;")
 
 drop_teradata("f_enrol")
 update_teradata("
 CREATE TABLE f_enrol AS( 
 SELECT DISTINCT t1.enrolid, t1.indexdt
 FROM f_enrol_flags t1, f_crit1 t2
 WHERE critfn2=1
 AND t1.enrolid=t2.enrolid
 ) WITH DATA;")
 
 query_teradata("select count(distinct enrolid) from f_enrol")
 
 ###############
 # 3rd criterion
 ###############
 # include patients with >=1 inpatient claim OR 2 outpatient claims (where each claim is 30-90 days apart)
 # between "01JAN2012" and index date 
 
 drop_teradata("f_table_in")
 update_teradata(paste0("
 CREATE TABLE f_table_in AS (
 SELECT t3.*
 FROM
 (
 
 /* Extract CCAE inpatient table */
 SELECT t1.enrolid, t1.svcdate, t1.age
 FROM rwd_vdm_ccae.v02_ccae_s t1, f_enrol t2
 WHERE t1.enrolid=t2.enrolid
 AND(svcdate BETWEEN '",spstdt,"' AND t2.indexdt)
 AND (dx1 like any (",primDiagCode,") OR dx2 like any (",primDiagCode,") OR dx3 like any (",primDiagCode,") OR  dx4 like any (",primDiagCode,") OR dx5 like any (",primDiagCode,") OR pdx like any (",primDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND	INDEX(VERSION_,'E') = 0
 QUALIFY ROW_NUMBER() OVER (PARTITION BY t1.enrolid ORDER BY t1.svcdate)=1
 
 UNION
 
 /* Extract MDCR inpatient table */
 SELECT t1.enrolid, t1.svcdate, t1.age
 FROM rwd_vdm_mdcr.v02_mdcr_s t1, f_enrol t2
 WHERE t1.enrolid=t2.enrolid
 AND(svcdate BETWEEN '",spstdt,"' AND t2.indexdt)
 AND (dx1 like any (",primDiagCode,") OR dx2 like any (",primDiagCode,") OR dx3 like any (",primDiagCode,") OR  dx4 like any (",primDiagCode,") OR dx5 like any (",primDiagCode,") OR pdx like any (",primDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND	INDEX(VERSION_,'E') = 0
 QUALIFY ROW_NUMBER() OVER (PARTITION BY t1.enrolid ORDER BY t1.svcdate)=1
 
 ) t3 QUALIFY ROW_NUMBER() OVER (PARTITION BY t3.enrolid ORDER BY t3.svcdate)=1
 ) WITH DATA;"))

 drop_teradata("f_table1_out")
 update_teradata(paste0("
 CREATE TABLE f_table1_out AS (
 
 /* CCAE outpatient table */
 SELECT DISTINCT t1.enrolid, t1.svcdate, t1.age
 FROM rwd_vdm_ccae.v02_ccae_o t1, f_enrol t2
 WHERE t1.enrolid=t2.enrolid
 AND (svcdate BETWEEN '",spstdt,"' AND t2.indexdt)
 AND (dx1 like any (",primDiagCode,") OR dx2 like any (",primDiagCode,") OR dx3 like any (",primDiagCode,") OR dx4 like any (",primDiagCode,") OR dx5 like any (",primDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 UNION
 
 /* MDCR outpatient table */
 SELECT DISTINCT t1.enrolid, t1.svcdate, t1.age
 FROM rwd_vdm_mdcr.v02_mdcr_o t1, f_enrol t2
 WHERE t1.enrolid=t2.enrolid
 AND (svcdate BETWEEN '",spstdt,"' AND t2.indexdt)
 AND (dx1 like any (",primDiagCode,") OR dx2 like any (",primDiagCode,") OR dx3 like any (",primDiagCode,") OR dx4 like any (",primDiagCode,") OR dx5 like any (",primDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 ) WITH DATA;"))
 
 # 2 outpatient claims 30-90 days apart
 drop_teradata("f_table_out")
 update_teradata("
 CREATE TABLE f_table_out AS (
 SELECT t2.enrolid, min(t2.svcdate) as svcdate, min(t2.age) as age
 FROM f_table1_out t1, f_table1_out t2
 WHERE t1.enrolid=t2.enrolid
 AND   t1.svcdate-t2.svcdate BETWEEN 30 AND 90
 GROUP BY t2.enrolid 
 ) WITH DATA;")
 
 # find the 1st diagnosis claim
 drop_teradata("f_firstdiag")
 update_teradata("
 CREATE TABLE f_firstdiag AS (
 SELECT t2.enrolid, t2.firstdiagdt, t2.age, t3.indexdt
 FROM
     (
      SELECT t1.enrolid, min(t1.svcdate) as firstdiagdt, min(t1.age) as age
      FROM
          (
           SELECT * FROM f_table_in
           UNION
           SELECT * FROM f_table_out
           ) t1 
           WHERE t1.age>=18
           GROUP BY t1.enrolid
           
 ) t2 INNER JOIN f_crit1 t3 ON t2.enrolid=t3.enrolid
 
 ) WITH DATA;")
 
 query_teradata("SELECT count(*), count(distinct enrolid) FROM firstdiag") 
 query_teradata("SELECT count(*), count(distinct enrolid) FROM f_firstdiag") 
 
 
 # Patients were further required to have at least one inpatient 
 # or two outpatient non-diagnostic claims on separate dates with metastatic cancer diagnoses 
 # within 60 days prior to or subsequent to a breast cancer claim, up to and including the index date. 
 
 drop_teradata("f_table_in_3b")
 update_teradata(paste0("
 CREATE TABLE f_table_in_3b AS (
 SELECT enrolid, min(svcdate) as first_inpatient_diag_3b
 FROM
 (
 
 /* Extract CCAE inpatient table */
 SELECT t1.enrolid, svcdate
 FROM rwd_vdm_ccae.v02_ccae_s t1, f_firstdiag t2
 WHERE t1.enrolid=t2.enrolid
 AND (svcdate BETWEEN firstdiagdt-60 AND t2.indexdt)
 AND (dx1 like any (",metastatDiagCode,") OR dx2 like any (",metastatDiagCode,") OR dx3 like any (",metastatDiagCode,") OR  dx4 like any (",metastatDiagCode,") OR dx5 like any (",metastatDiagCode,") OR pdx like any (",metastatDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 UNION
 
 /* Extract MDCR inpatient table */
 SELECT t1.enrolid, svcdate
 FROM rwd_vdm_mdcr.v02_mdcr_s t1, f_firstdiag t2
 WHERE t1.enrolid=t2.enrolid
 AND (svcdate BETWEEN firstdiagdt-60 AND t2.indexdt)
 AND (dx1 like any (",metastatDiagCode,") OR dx2 like any (",metastatDiagCode,") OR dx3 like any (",metastatDiagCode,") OR  dx4 like any (",metastatDiagCode,") OR dx5 like any (",metastatDiagCode,") OR pdx like any (",metastatDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 ) h 
 GROUP BY enrolid
 ) WITH DATA;"))
 
 drop_teradata("f_table1_out_3b")
 update_teradata(paste0("
 CREATE TABLE f_table1_out_3b AS (
 
 /* CCAE outpatient table */
 SELECT DISTINCT t1.enrolid, svcdate 
 FROM rwd_vdm_ccae.v02_ccae_o t1, f_firstdiag t2
 WHERE t1.enrolid=t2.enrolid
 AND(svcdate BETWEEN firstdiagdt-60 AND t2.indexdt)
 AND (dx1 like any (",metastatDiagCode,") OR dx2 like any (",metastatDiagCode,") OR dx3 like any (",metastatDiagCode,") OR dx4 like any (",metastatDiagCode,") OR dx5 like any (",metastatDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 UNION
 
 /* MDCR outpatient table */
 SELECT DISTINCT t1.enrolid, svcdate 
 FROM rwd_vdm_mdcr.v02_mdcr_o t1, f_firstdiag t2
 WHERE t1.enrolid=t2.enrolid
 AND(svcdate BETWEEN firstdiagdt-60 AND t2.indexdt)
 AND (dx1 like any (",metastatDiagCode,") OR dx2 like any (",metastatDiagCode,") OR dx3 like any (",metastatDiagCode,") OR dx4 like any (",metastatDiagCode,") OR dx5 like any (",metastatDiagCode,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 ) WITH DATA;"))
 
 # 2 outpatient claims 30-90 days apart
 drop_teradata("f_table_out_3b")
 update_teradata("
 CREATE TABLE f_table_out_3b AS (
 SELECT t2.enrolid, min(t2.svcdate) as svcdate
 FROM f_table1_out_3b t1, f_table1_out_3b t2
 WHERE t1.enrolid=t2.enrolid
 AND   t1.svcdate-t2.svcdate BETWEEN 30 AND 90
 GROUP BY t2.enrolid 
 ) WITH DATA;")
 
 # find the 1st diagnosis claim
 drop_teradata("f_crit_3b")
 update_teradata("
 CREATE TABLE f_crit_3b AS (
 SELECT DISTINCT enrolid FROM f_table_in_3b
 UNION
 SELECT DISTINCT enrolid FROM f_table_out_3b
 ) WITH DATA")
 
 drop_teradata("f_crit3")
 update_teradata("
 CREATE TABLE f_crit3 AS (
 SELECT t1.*
 FROM f_firstdiag t1, crit_3b t2
 WHERE t1.enrolid=t2.enrolid
 ) WITH DATA")
 
 query_teradata("SELECT count(*), count(distinct enrolid) FROM f_crit3") 
 
 # Exclude patients with dx cancer claim other than breast during baseline (12 months preceding the index) 
 drop_teradata("f_previously_diagnosed")
 update_teradata("
 CREATE TABLE f_previously_diagnosed AS (
 
 /* CCAE inpatient table */
 SELECT DISTINCT t1.enrolid
 FROM rwd_vdm_ccae.v02_ccae_s t1, f_crit3 t2
 WHERE t1.enrolid=t2.enrolid
 AND t1.svcdate BETWEEN t2.indexdt-365 AND t2.indexdt-1
 AND (dx1 like any (",primDiagCodeNoBreast,") OR dx2 like any (",primDiagCodeNoBreast,") OR dx3 like any (",primDiagCodeNoBreast,") OR  dx4 like any (",primDiagCodeNoBreast,") OR dx5 like any (",primDiagCodeNoBreast,") OR pdx like any (",primDiagCodeNoBreast,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 UNION
 
 /* MDCR inpatient table */
 SELECT t1.enrolid
 FROM rwd_vdm_mdcr.v02_mdcr_s t1, f_crit3 t2
 WHERE t1.enrolid=t2.enrolid
 AND t1.svcdate BETWEEN t2.indexdt-365 AND t2.indexdt-1
 AND (dx1 like any (",primDiagCodeNoBreast,") OR dx2 like any (",primDiagCodeNoBreast,") OR dx3 like any (",primDiagCodeNoBreast,") OR  dx4 like any (",primDiagCodeNoBreast,") OR dx5 like any (",primDiagCodeNoBreast,") OR pdx like any (",primDiagCodeNoBreast,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 UNION
 
 /* CCAE outpatient table */
 SELECT DISTINCT t1.enrolid
 FROM rwd_vdm_ccae.v02_ccae_o t1, f_crit3 t2
 WHERE t1.enrolid=t2.enrolid
 AND t1.svcdate BETWEEN t2.indexdt-365 AND t2.indexdt-1
 AND (dx1 like any (",primDiagCodeNoBreast,") OR dx2 like any (",primDiagCodeNoBreast,") OR dx3 like any (",primDiagCodeNoBreast,") OR dx4 like any (",primDiagCodeNoBreast,") OR dx5 like any (",primDiagCodeNoBreast,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 UNION
 
 /* MDCR outpatient table */
 SELECT DISTINCT t1.enrolid
 FROM rwd_vdm_mdcr.v02_mdcr_o t1, f_crit3 t2
 WHERE t1.enrolid=t2.enrolid
 AND t1.svcdate BETWEEN t2.indexdt-365 AND t2.indexdt-1
 AND (dx1 like any (",primDiagCodeNoBreast,") OR dx2 like any (",primDiagCodeNoBreast,") OR dx3 like any (",primDiagCodeNoBreast,") OR dx4 like any (",primDiagCodeNoBreast,") OR dx5 like any (",primDiagCodeNoBreast,"))
 AND DATE '",valid_date,"' BETWEEN valid_start AND valid_end
 AND INDEX(VERSION_,'E') = 0
 
 ) WITH DATA;")
 
 # patients with cancer claim other than breast during baseline (12months preceding the index)
 query_teradata("SELECT count(distinct enrolid) FROM f_previously_diagnosed") 
 
 # Final cohort
 drop_teradata("f_cohort")
 update_teradata("
 CREATE TABLE f_cohort AS(
 SELECT *
 FROM f_crit3
 WHERE enrolid NOT IN (SELECT ENROLID FROM f_previously_diagnosed)
 ) WITH DATA;")
 
 cohort <- data.table(tdRWDSquery("SELECT * FROM f_cohort;"), stringsAsFactors=F)
 

 query_teradata("SELECT count(distinct enrolid) FROM f_crit1")     # 96360
 query_teradata("SELECT count(distinct enrolid) FROM f_enrol")     # 42215
 query_teradata("SELECT count(distinct enrolid) FROM f_firstdiag") # 28905
 query_teradata("SELECT count(distinct enrolid) FROM f_crit3")     # 3548
 query_teradata("SELECT count(distinct enrolid) FROM f_cohort")    # 3022

 #######################################################################
 # Extract all medication records during the 365-day post-index period #
 #######################################################################
 drop_teradata("f_extract_d_post_365days")
 update_teradata(paste0("
 CREATE TABLE f_extract_d_post_365days AS (
 
 SELECT tx.enrolid, tx.indexdt, tx.svcdate, sum(tx.daysupp) AS daysupp, tx.ndcnum, tx.gennme
 
 FROM (
 
       SELECT t1.enrolid,t2.indexdt,t1.svcdate,t1.daysupp,t1.ndcnum,t3.gennme
       FROM rwd_vdm_ccae.v02_ccae_d t1, f_cohort t2, drug_codes t3
       WHERE t1.enrolid=t2.enrolid
       AND   t1.svcdate BETWEEN t2.indexdt AND t2.indexdt+365
       AND   t1.ndcnum=t3.ndccode
       AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
       AND	 INDEX(VERSION_,'E') = 0
       
       UNION
       
       SELECT t1.enrolid,t2.indexdt,t1.svcdate,t1.daysupp,t1.ndcnum,t3.gennme
       FROM rwd_vdm_mdcr.v02_mdcr_d t1, f_cohort t2, drug_codes t3
       WHERE t1.enrolid=t2.enrolid
       AND   t1.svcdate BETWEEN t2.indexdt AND t2.indexdt+365
       AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
       AND	 INDEX(VERSION_,'E') = 0 
       
       ) tx
       
 GROUP BY tx.enrolid, tx.indexdt, tx.svcdate, tx.ndcnum, tx.gennme  
 
 ) WITH DATA;")) 
 
 query_teradata("SELECT count(*), count(distinct enrolid) FROM f_extract_d_post_365days") 
 
 ####################################################################
 # Count primary_care_visits within 1 year following the index date #
 ####################################################################
 
# The variables stdplac and stdprov correspond to place of service 
# and provider type respectively and can be used to identify primary care visits. 
 
 drop_teradata("f_primary_care_visits_365d")
 update_teradata(paste0("
 CREATE TABLE f_primary_care_visits_365d AS (
 
 SELECT tx.enrolid, count(distinct svcdate) as nof_visits, max(days_from_index) as max_days_from_index
 
 FROM (
 
   SELECT t1.enrolid, t1.svcdate, t1.svcdate-t2.indexdt as days_from_index 
   FROM  rwd_vdm_ccae.v02_ccae_o t1, f_cohort t2 
   WHERE t1.enrolid=t2.enrolid
   AND   t1.svcdate BETWEEN t2.indexdt+1 AND t2.indexdt+365
   AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
   AND	INDEX(VERSION_,'E') = 0
   AND   (stdplac IN (11,15,17,19,26,49,71,72,95) OR stdprov IN (15,200,240,360))
   
   UNION ALL 
   
   SELECT t1.enrolid, t1.svcdate, t1.svcdate-t2.indexdt as days_from_index 
   FROM  rwd_vdm_ccae.v02_ccae_s t1, f_cohort t2 
   WHERE t1.enrolid=t2.enrolid
   AND   t1.svcdate BETWEEN t2.indexdt+1 AND t2.indexdt+365
   AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
   AND	INDEX(VERSION_,'E') = 0
   AND   (stdplac IN (11,15,17,19,26,49,71,72,95) OR stdprov IN (15,200,240,360))

   UNION ALL
   
   SELECT t1.enrolid, t1.svcdate, t1.svcdate-t2.indexdt as days_from_index 
   FROM  rwd_vdm_ccae.v02_ccae_f t1, f_cohort t2 
   WHERE t1.enrolid=t2.enrolid
   AND   t1.svcdate BETWEEN t2.indexdt+1 AND t2.indexdt+365
   AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
   AND	INDEX(VERSION_,'E') = 0
   AND   (stdplac IN (11,15,17,19,26,49,71,72,95) OR stdprov IN (15,200,240,360))

   UNION ALL 

   SELECT t1.enrolid, t1.svcdate, t1.svcdate-t2.indexdt as days_from_index 
   FROM  rwd_vdm_mdcr.v02_mdcr_o t1, f_cohort t2 
   WHERE t1.enrolid=t2.enrolid
   AND   t1.svcdate BETWEEN t2.indexdt+1 AND t2.indexdt+365
   AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
   AND	INDEX(VERSION_,'E') = 0
   AND   (stdplac IN (11,15,17,19,26,49,71,72,95) OR stdprov IN (15,200,240,360))
   
   UNION ALL 
   
   SELECT t1.enrolid, t1.svcdate, t1.svcdate-t2.indexdt as days_from_index 
   FROM  rwd_vdm_mdcr.v02_mdcr_s t1, f_cohort t2 
   WHERE t1.enrolid=t2.enrolid
   AND   t1.svcdate BETWEEN t2.indexdt+1 AND t2.indexdt+365
   AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
   AND	INDEX(VERSION_,'E') = 0
   AND   (stdplac IN (11,15,17,19,26,49,71,72,95) OR stdprov IN (15,200,240,360))

   UNION ALL
   
   SELECT t1.enrolid, t1.svcdate, t1.svcdate-t2.indexdt as days_from_index 
   FROM  rwd_vdm_mdcr.v02_mdcr_f t1, f_cohort t2 
   WHERE t1.enrolid=t2.enrolid
   AND   t1.svcdate BETWEEN t2.indexdt+1 AND t2.indexdt+365
   AND   DATE '",valid_date,"' BETWEEN valid_start AND valid_end
   AND	INDEX(VERSION_,'E') = 0
   AND   (stdplac IN (11,15,17,19,26,49,71,72,95) OR stdprov IN (15,200,240,360))
   ) tx
       
 GROUP BY tx.enrolid
 
 ) WITH DATA;")) 
 
 cohort_primVisits <- data.table(tdRWDSquery("SELECT * FROM f_primary_care_visits_365d;"), stringsAsFactors=F)
 
