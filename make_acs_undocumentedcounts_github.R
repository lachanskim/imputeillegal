## This is the R adaptation of Borjas and Cassidy's ACS files to impute undocumented status
## using ACS microdata. It is adapted from the programs of Borjas and Cassidy (2019)
# memory.size(); memory.size(size = 32000) # you will need a lot of RAM unfortunately
library(data.table); library(dplyr); library(ipumsr)

# download your copy the ACS from IPUMS as well as the associated XML 

rawdir = # YOUR DIRECTORY CONTAINING THE IPUMS ACS
setwd(rawdir)

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

XML = # YOUR XML FILE THAT GOES WITH THE IPUMS ACS DOWNLOAD
 
ddi <- read_ipums_ddi(XML)
acs <- read_ipums_micro(ddi) %>% as.data.table %>% setkey(PERNUM, SERIAL, YEAR, SAMPLE); beep()

# sizes= object.size(data); print(sizes, units = "GB"); get a grip on what we're dealing with

# DROP PREVIOUS CODES, SINCE I RE-SAVE USING SAME DATA FILE
acs[,imm := (CITIZEN==2 | CITIZEN==3)]

# This will be the Borjas undocumented algorithm
# acs[,gbtype := NULL]

# DEFINE THE NATIVE BORN
acs[imm==0, gbtype := 3]

# A PERSON IS LEGAL IF ARRIVED BEFORE 1980
acs[imm == 1 & YRIMMIG < 1980, gbtype := 2]

# A PERSON IS LEGAL IF A CITIZEN
acs[imm == 1 & CITIZEN == 2, gbtype := 2]

# A PERSON IS LEGAL IF RECEIVES SOCIAL SECURITY, SSI, MEDICAID, MEDICARE, OR MILITARY INSURANCE
acs[imm == 1 & INCSS > 0 & INCSS < 99999, gbtype := 2]
acs[imm == 1 & INCSUPP > 0 & INCSUPP < 99999, gbtype := 2]
acs[imm == 1 & (HINSCAID == 2 | HINSCARE == 2 | HINSVA == 2), gbtype := 2]

# A PERSON IS LEGAL IF WORKS FOR ARMED FORCES, GOVERNMENT, OR IS VETERAN; CLASSWKRD IS DETAILED CLASS
acs[imm == 1 & (CLASSWKRD>=25 & CLASSWKRD<=28), gbtype := 2]
acs[imm == 1 & VETSTAT == 2, gbtype := 2]

# CUBANS ARE LEGAL; BPLD FOR "DETAILED" BIRTHPLACE
acs[imm == 1 & BPLD==25000, gbtype:= 2]

# WORKERS IN LICENSED OCCUPATIONS ARE LEGAL; FROM LABOUR ECON PAPER
licensed.occs = c(3,4,16,19,23,35,36,43,
                  seq.int(84,89,1),seq.int(95,97,1),seq.int(155,163,1),178,179,seq.int(203,208,1),
                  226,227,375,seq.int(417,423,1),489,905) 

# George likes 1990 OCC for some reason. Note also that the Borjas algorithm says that only military occupations with unspecified 
# rank #s should be automatically added to legal immigrants. In theory, I think probably all military ranks should be added 
# to this, but in fact the IPUMS ACS ONLY includes "rank not specified" military occupations, so it doesn't matter for YEAR 
# < 2018. However, in 2018 the occupational coding changes, and we need to explicitly include all military occupations. 

licensed.occs2018 = c(30, 400, 4461, 4465, 800, 6660, 9410, 3750, 8740, 565, 1430, 300, 1305, 1306, 1440, 1541, 3065, 3070,
                      3090, 3100, 3270, 3010, 3250, 3040, 3120, 3140, 3260, 3300, 3255, 3256, 3257, 3258, 3050, 3030, 2300, 
                      2310, 2320, 2330, 2360, 2002, 2006, 2105, 2110, 3300, 3310, 3515, 3323, 3500, 3321, 3322, 3324, 3330,
                      9030, 9040, 5840, 3740, 3870, 3801, 3802, 3820, 3830, 3840, 6010, 9800, 9810, 9830)
# correct this for 2018 data
acs[(YEAR <2018 & imm==1 & OCC1990 %in% licensed.occs) | (YEAR == 2018 & imm == 1 & (OCC %in% licensed.occs2018)), gbtype:= 2]

#H1B CORRECTION
acs[,ysm := YEAR - YRIMMIG]; acs[imm==0,ysm:=0]
acs[,edcode := 1*(EDUCD <= 50) + 2*(EDUCD>=60 & EDUCD<=64) + 3*(EDUCD >=65 & EDUCD <=90) + 4*(EDUCD==100 | EDUCD==101) + 
      5*(EDUCD>=110)]

# MOST H1Bs BORN IN A FEW COUNTRIES AND HAVE A FEW OCCUPATIONS
H1B.BPL = c(150,500,502,515, 521); H1B.OCC = c(110,seq.int(1000,1240,1),seq.int(1300,1560,1),2200)
H1B = c("h1b_ctr", "h1b_occ", "h1b_alt2", "h1b_alt1")
acs[, `:=`(h1b_ctr = (BPL %in% H1B.BPL),
              h1b_occ = (OCC %in% H1B.OCC))]
acs[,`:=`(h1b_alt2 = (h1b_occ==1 & edcode >= 4))]; acs[,`:=`(h1b_alt1 = (h1b_alt2 & h1b_ctr==1))]
acs[, h1b:= (h1b_alt2 & (ysm <= 6))]

# PRESERVE NO H1B CORRECTION FOR COMPARISON
acs[ ,`:=`(gbtype_noh1b = gbtype, 
           gbtype_h1balt1 = gbtype, 
           gbtype_h1balt2 = gbtype)]; acs[imm == 1 & h1b == 1,gbtype := 2]; 
acs[imm == 1 & h1b_alt1 == 1, gbtype_h1balt1 :=2]; 
acs[imm == 1 & h1b_alt2 == 1, gbtype_h1balt2:= 2]

#GET RID OF USELESS VARIABLES
acs[,`:=`(ysm = NULL, edcode = NULL)]

acs %>% group_by(gbtype) %>%  summarise(n = n()) 


#CREATES VARIABLE FINE, INDICATING PERSON IS EITHER LEGAL OR NATIVE-BORN
acs[imm == 0 | gbtype == 2,fine := 1]
acs[imm != 0 & !gbtype %in% 2,fine := 0]
acs[imm == 0 | gbtype_noh1b == 2,fine_noh1b := 1]
acs[imm != 0 & !gbtype_noh1b %in% 2,fine_noh1b := 0]
acs[imm == 0 | gbtype_h1balt1 == 2,fine_h1balt1 := 1]
acs[imm != 0 & !gbtype_h1balt1 %in% 2,fine_h1balt1 := 0]
acs[imm == 0 | gbtype_h1balt2 == 2,fine_h1balt2 := 1]
acs[imm != 0 & !gbtype_h1balt2 %in% 2,fine_h1balt2 := 0]


#HEADHOLDER SPOUSE OF LEGAL "SPOUSE" IS LEGAL
acs[,good := 0]; acs[(RELATE==2 & imm == 1), good:= 1]
acs %>% count(good)

acs[, `:=` (slegal         = mean(good*fine, na.rm = T),
            slegal_noh1b1  = mean(good*fine_noh1b, na.rm = T),
            slegal_h1balt1 = mean(good*fine_h1balt1, na.rm = T),
            slegal_h1balt2 = mean(good*fine_h1balt2, na.rm = T)),
            by=.(YEAR,SERIAL)]

summary(acs$slegal)

acs %>% count(slegal)
#STATA TABULATE COMMAND EQUIVALENT TO FOLLOWING IN R
acs %>% group_by(slegal) %>%  summarise(n = n()) %>%
  mutate(totalN = (cumsum(n)), percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)), 3))

acs %>% group_by(gbtype) %>%  summarise(n = n()) 

acs %>%
  mutate(totalN = (cumsum(n)), percent = round((n / sum(n)), 3),
         cumpercent = round(cumsum(freq = n / sum(n)), 3))

acs[slegal > 0 & imm==1, diag := 1]
acs[slegal > 0 & imm==1 & (RELATE ==1 & MARST == 1),        gbtype:= 2 ]
acs[slegal_noh1b1 > 0 & imm==1 & (RELATE ==1 & MARST == 1),  gbtype_noh1b:= 2 ]
acs[slegal_h1balt1 > 0 & imm==1 & (RELATE ==1 & MARST == 1), gbtype_h1balt1 := 2 ]
acs[slegal_h1balt2 > 0 & imm==1 & (RELATE ==1 & MARST == 1), gbtype_h1balt2 := 2 ]

acs %>% count(gbtype)
acs %>% count(gbtype_noh1b)

#SPOUSES, CHILDREN, GRANDCHILDREN, OF LEGAL HOUSEHOLDERS ARE LEGAL
acs[,good:=0]; acs[RELATE==1 & imm==1,good:=1]
acs[, `:=` (hlegal = mean(good*fine, na.rm = T),
            hlegal_noh1b1 = mean(good*fine_noh1b, na.rm = T),
            hlegal_h1balt1 = mean(good*fine_h1balt1, na.rm = T),
            hlegal_h1balt2 = mean(good*fine_h1balt2, na.rm = T)),
    by=.(YEAR,SERIAL)]

acs %>% group_by(hlegal) %>%  summarise(n = n()) %>% #STATA TABULATE COMMAND EQUIVALENT TO FOLLOWING IN R
  mutate(totalN = (cumsum(n)), percent = round((n / sum(n)), 3),
  cumpercent = round(cumsum(freq = n / sum(n)), 3))
  
acs[, spouses.children.grandchildren := (RELATE == 2 & MARST == 1) | (RELATE == 3 | RELATE == 9)+ 0]
acs[spouses.children.grandchildren==1 & hlegal>0 & imm==1,  gbtype := 2]
acs[spouses.children.grandchildren==1 & hlegal_noh1b1 > 0 & imm==1,   gbtype_noh1b := 2]
acs[spouses.children.grandchildren==1 & hlegal_h1balt1 > 0 & imm==1,  gbtype_h1balt1 := 2]
acs[spouses.children.grandchildren==1 & hlegal_h1balt2 > 0 & imm==1,  gbtype_h1balt2 := 2]

#THE RESIDUAL IMMIGRANTS ARE UNDOCUMENTED
acs[is.na(gbtype), gbtype := 1]; acs[is.na(gbtype_noh1b), gbtype_noh1b := 1]
acs[is.na(gbtype_h1balt1), gbtype_h1balt1 := 1]; acs[is.na(gbtype_h1balt2), gbtype_h1balt2 := 1]

#CODE =1 FOR NATIVES, 2 FOR LEGAL IMMIGRANTS, 3 FOR UNDOCUMENTED IMMMIGRANTS
acs[gbtype == 3, code := 1]; acs[gbtype == 2, code := 2]; acs[gbtype == 1, code := 3];
acs[gbtype_noh1b == 3, code_noh1b := 1]; acs[gbtype_noh1b == 2, code_noh1b := 2]; acs[gbtype_noh1b == 1, code_noh1b := 3];
acs[gbtype_h1balt1 == 3, code_h1balt1 := 1]; acs[gbtype_h1balt1 == 2, code_h1balt1 := 2]; acs[gbtype_h1balt1 == 1, code_h1balt1 := 3];
acs[gbtype_h1balt2 == 3, code_h1balt2 := 1]; acs[gbtype_h1balt2 == 2, code_h1balt2 := 2]; acs[gbtype_h1balt2 == 1, code_h1balt2 := 3];

acs %>% group_by(YEAR, code) %>% count()

acs %>% group_by(YEAR, code) %>% count(wt = PERWT)

acs[ , gbtype := NULL]; acs[ , gbtype_noh1b := NULL]; acs[ , gbtype_h1balt1 := NULL]; acs[ , gbtype_h1balt2 := NULL];
fwrite(acs, "acs_undocumenteds.csv"); beep(); acs %>% count(code); acs[YEAR==2018] %>% count(code, wt = PERWT)