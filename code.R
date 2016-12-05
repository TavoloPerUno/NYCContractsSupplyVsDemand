library(XLConnect)
library(reshape)
library(reshape2)
library(foreign)
library(sp) 
library(gstat)
library(rgdal)
library(maptools)
library(foreign)
library(sp) 
library(gstat)
library(stringr)
library(gdata)
library(plyr)
library(dplyr)
library(checkmate)
library(BBmisc)

nyct <- read.dbf("nyct2010wi.dbf")

censusdat <- read.csv("acs0812.csv")

censusdat$BORO <- 0

censusdat[grepl( "Bronx" , censusdat$Geo_NAME ),]$BORO <- 2
censusdat[grepl( "Manhattan" , censusdat$Geo_NAME ),]$BORO <- 1
censusdat[grepl( "Brooklyn" , censusdat$Geo_NAME ),]$BORO <- 3
censusdat[grepl( "Queens" , censusdat$Geo_NAME ),]$BORO <- 4
censusdat[grepl( "Staten Island" , censusdat$Geo_NAME ),]$BORO <- 5

censusdat$TRACTID <- as.numeric(colsplit(colsplit(colsplit(censusdat$Geo_NAME, ", ", c("Name", "Junk"))$"Name", " ", c("P1", "P2"))$"P2", " ", c("gbg", "TRACT"))$"TRACT")
colnames(censusdat)[which(names(censusdat) == "BORO")] <- "BoroCode"
colnames(censusdat)[which(names(censusdat) == "Geo_TRACT")] <- "CT2010"
colnames(censusdat)[which(names(censusdat) == "SE_T001_001")] <- "POPTOT"
colnames(censusdat)[which(names(censusdat) == "SE_T002_002")] <- "POPDTY"
colnames(censusdat)[which(names(censusdat) == "SE_T004_002")] <- "MALE"
colnames(censusdat)[which(names(censusdat) == "SE_T004_003")] <- "FEMALE"
colnames(censusdat)[which(names(censusdat) == "SE_T005B006")] <- "MALEOVER18"
colnames(censusdat)[which(names(censusdat) == "SE_T005B018")] <- "FEMALEOVER18"
colnames(censusdat)[which(names(censusdat) == "SE_T008B005")] <- "POPOVER18"
colnames(censusdat)[which(names(censusdat) == "SE_T012_001")] <- "MEDIANAGE"
colnames(censusdat)[which(names(censusdat) == "SE_T012_002")] <- "MEDIANAGEMALE"
colnames(censusdat)[which(names(censusdat) == "SE_T012_003")] <- "MEDIANAGEFEMALE"
colnames(censusdat)[which(names(censusdat) == "SE_T013_002")] <- "EUROPEAN"
colnames(censusdat)[which(names(censusdat) == "SE_T013_003")] <- "AFRICAN"
colnames(censusdat)[which(names(censusdat) == "SE_T013_005")] <- "ASIAN"
colnames(censusdat)[which(names(censusdat) == "SE_T013_004")] <- "AMERICAN"
colnames(censusdat)[which(names(censusdat) == "SE_T013_006")] <- "PACIFIC"
colnames(censusdat)[which(names(censusdat) == "SE_T013_007")] <- "OTHERETHNICITY"
colnames(censusdat)[which(names(censusdat) == "SE_T013_008")] <- "MIXED"
colnames(censusdat)[which(names(censusdat) == "SE_T014_010")] <- "HISPANIC"
colnames(censusdat)[which(names(censusdat) == "SE_T017_001")] <- "HOUSEHOLDS"
colnames(censusdat)[which(names(censusdat) == "SE_T021_001")] <- "HOUSEHOLDSIZE"
colnames(censusdat)[which(names(censusdat) == "SE_T025_002")] <- "LESSTHANHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T025_003")] <- "HIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T025_004")] <- "COLLEGE"
colnames(censusdat)[which(names(censusdat) == "SE_T025_005")] <- "BACHELOR"
colnames(censusdat)[which(names(censusdat) == "SE_T025_006")] <- "MASTER"
colnames(censusdat)[which(names(censusdat) == "SE_T025_007")] <- "PROFESSIONAL"
colnames(censusdat)[which(names(censusdat) == "SE_T025_008")] <- "DOCTORATE"
colnames(censusdat)[which(names(censusdat) == "SE_T150_002")] <- "ONLYLESSTHANHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T150_003")] <- "ONLYHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T150_004")] <- "ONLYCOLLEGE"
colnames(censusdat)[which(names(censusdat) == "SE_T150_005")] <- "ONLYBACHELOR"
colnames(censusdat)[which(names(censusdat) == "SE_T150_006")] <- "ONLYMASTER"
colnames(censusdat)[which(names(censusdat) == "SE_T150_007")] <- "ONLYPROFESSIONAL"
colnames(censusdat)[which(names(censusdat) == "SE_T150_008")] <- "ONLYDOCTORATE"
colnames(censusdat)[which(names(censusdat) == "SE_T151_002")] <- "MALEONLYLESSTHANHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T151_003")] <- "MALEONLYHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T151_004")] <- "MALEONLYCOLLEGE"
colnames(censusdat)[which(names(censusdat) == "SE_T151_005")] <- "MALEONLYBACHELOR"
colnames(censusdat)[which(names(censusdat) == "SE_T151_006")] <- "MALEONLYMASTER"
colnames(censusdat)[which(names(censusdat) == "SE_T151_007")] <- "MALEONLYPROFESSIONAL"
colnames(censusdat)[which(names(censusdat) == "SE_T151_008")] <- "MALEONLYDOCTORATE"
colnames(censusdat)[which(names(censusdat) == "SE_T152_002")] <- "FEMALEONLYLESSTHANHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T152_003")] <- "FEMALEONLYHIGHSCHOOL"
colnames(censusdat)[which(names(censusdat) == "SE_T152_004")] <- "FEMALEONLYCOLLEGE"
colnames(censusdat)[which(names(censusdat) == "SE_T152_005")] <- "FEMALEONLYBACHELOR"
colnames(censusdat)[which(names(censusdat) == "SE_T152_006")] <- "FEMALEONLYMASTER"
colnames(censusdat)[which(names(censusdat) == "SE_T152_007")] <- "FEMALEONLYPROFESSIONAL"
colnames(censusdat)[which(names(censusdat) == "SE_T152_008")] <- "FEMALEONLYDOCTORATE"
colnames(censusdat)[which(names(censusdat) == "SE_T031_002")] <- "MALEDROP"
colnames(censusdat)[which(names(censusdat) == "SE_T032_002")] <- "FEMALEDROP"
colnames(censusdat)[which(names(censusdat) == "SE_T031_001")] <- "MALE16TO19"
colnames(censusdat)[which(names(censusdat) == "SE_T032_001")] <- "FEMALE16TO19"
colnames(censusdat)[which(names(censusdat) == "SE_T034_002")] <- "MALEINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T035_002")] <- "FEMALEINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T033_002")] <- "POPINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T034_006")] <- "MALEUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T035_006")] <- "FEMALEUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T033_006")] <- "POPUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T041_001")] <- "AFRICANINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T041_003")] <- "AFRICANUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T042_001")] <- "AMERICANINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T042_003")] <- "AMERICANUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T040_001")] <- "EUROPEANINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T040_003")] <- "EUROPEANUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T043_001")] <- "ASIANINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T043_003")] <- "ASIANUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T044_001")] <- "PACIFICINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T044_003")] <- "PACIFICUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T046_001")] <- "MIXEDINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T046_003")] <- "MIXEDUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T047_001")] <- "HISPANICINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T047_003")] <- "HISPANICUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T045_001")] <- "OTHERINLABOURFORCE"
colnames(censusdat)[which(names(censusdat) == "SE_T045_003")] <- "OTHERUNEMPLOYED"
colnames(censusdat)[which(names(censusdat) == "SE_T056_002")] <- "LESSTHAN10000"
colnames(censusdat)[which(names(censusdat) == "SE_T057_001")] <- "MEDIANINCOME"
colnames(censusdat)[which(names(censusdat) == "SE_T078_002")] <- "WITHSOCIALSECURITY"
colnames(censusdat)[which(names(censusdat) == "SE_T079_002")] <- "WITHSSI"
colnames(censusdat)[which(names(censusdat) == "SE_T080_002")] <- "WITHPUBASST"
colnames(censusdat)[which(names(censusdat) == "SE_T157_001")] <- "GINI"
colnames(censusdat)[which(names(censusdat) == "SE_T118_002")] <- "POOR"
colnames(censusdat)[which(names(censusdat) == "SE_T118_003")] <- "STRUGGLING"
colnames(censusdat)[which(names(censusdat) == "SE_T118_004")] <- "POORORSTRUGGLING"
colnames(censusdat)[which(names(censusdat) == "SE_T118_005")] <- "OKAY"
colnames(censusdat)[which(names(censusdat) == "SE_T158_002")] <- "COMMUTELESSTHAN5"
colnames(censusdat)[which(names(censusdat) == "SE_T158_003")] <- "COMMUTE5TO14"
colnames(censusdat)[which(names(censusdat) == "SE_T158_004")] <- "COMMUTE15TO29"
colnames(censusdat)[which(names(censusdat) == "SE_T158_005")] <- "COMMUTE30TO44"
colnames(censusdat)[which(names(censusdat) == "SE_T158_006")] <- "COMMUTE45TO59"
colnames(censusdat)[which(names(censusdat) == "SE_T158_007")] <- "COMMUTE60TO89"
colnames(censusdat)[which(names(censusdat) == "SE_T158_008")] <- "COMMUTEMORETHAN90"

censusdat <- censusdat[, c("Geo_FIPS", 
                      "Geo_GEOID", 
                      "BoroCode",
                      "TRACTID",
                      "Geo_NAME", 
                      "Geo_QName", 
                      "Geo_STUSAB", 
                      "Geo_COUNTY", 
                      "Geo_STATE", 
                      "Geo_PLACE", 
                      "CT2010", 
                      "POPTOT",
                      "POPDTY",
                      "MALE",
                      "FEMALE",
                      "MALEOVER18",
                      "FEMALEOVER18",
                      "POPOVER18",
                      "MEDIANAGE",
                      "MEDIANAGEMALE",
                      "MEDIANAGEFEMALE",
                      "EUROPEAN",
                      "AFRICAN",
                      "ASIAN",
                      "AMERICAN",
                      "PACIFIC",
                      "OTHERETHNICITY",
                      "MIXED",
                      "HISPANIC",
                      "HOUSEHOLDS",
                      "HOUSEHOLDSIZE",
                      "LESSTHANHIGHSCHOOL",
                      "HIGHSCHOOL",
                      "COLLEGE",
                      "BACHELOR",
                      "MASTER",
                      "PROFESSIONAL",
                      "DOCTORATE",
                      "ONLYLESSTHANHIGHSCHOOL",
                      "ONLYHIGHSCHOOL",
                      "ONLYCOLLEGE",
                      "ONLYBACHELOR",
                      "ONLYMASTER",
                      "ONLYPROFESSIONAL",
                      "ONLYDOCTORATE",
                      "MALEONLYLESSTHANHIGHSCHOOL",
                      "MALEONLYHIGHSCHOOL",
                      "MALEONLYCOLLEGE",
                      "MALEONLYBACHELOR",
                      "MALEONLYMASTER",
                      "MALEONLYPROFESSIONAL",
                      "MALEONLYDOCTORATE",
                      "FEMALEONLYLESSTHANHIGHSCHOOL",
                      "FEMALEONLYHIGHSCHOOL",
                      "FEMALEONLYCOLLEGE",
                      "FEMALEONLYBACHELOR",
                      "FEMALEONLYMASTER",
                      "FEMALEONLYPROFESSIONAL",
                      "FEMALEONLYDOCTORATE",
                      "MALEDROP",
                      "FEMALEDROP",
                      "MALE16TO19",
                      "FEMALE16TO19",
                      "MALEINLABOURFORCE",
                      "FEMALEINLABOURFORCE",
                      "POPINLABOURFORCE",
                      "MALEUNEMPLOYED",
                      "FEMALEUNEMPLOYED",
                      "POPUNEMPLOYED",
                      "AFRICANINLABOURFORCE",
                      "AFRICANUNEMPLOYED",
                      "AMERICANINLABOURFORCE",
                      "AMERICANUNEMPLOYED",
                      "EUROPEANINLABOURFORCE",
                      "EUROPEANUNEMPLOYED",
                      "ASIANINLABOURFORCE",
                      "ASIANUNEMPLOYED",
                      "PACIFICINLABOURFORCE",
                      "PACIFICUNEMPLOYED",
                      "MIXEDINLABOURFORCE",
                      "MIXEDUNEMPLOYED",
                      "HISPANICINLABOURFORCE",
                      "HISPANICUNEMPLOYED",
                      "OTHERINLABOURFORCE",
                      "OTHERUNEMPLOYED",
                      "LESSTHAN10000",
                      "MEDIANINCOME",
                      "WITHSOCIALSECURITY",
                      "WITHSSI",
                      "WITHPUBASST",
                      "GINI",
                      "POOR",
                      "STRUGGLING",
                      "POORORSTRUGGLING",
                      "OKAY",
                      "COMMUTELESSTHAN5",
                      "COMMUTE5TO14",
                      "COMMUTE15TO29",
                      "COMMUTE30TO44",
                      "COMMUTE45TO59",
                      "COMMUTE60TO89",
                      "COMMUTEMORETHAN90")]

nyct$BoroCode <- as.numeric(nyct$BoroCode)
nyct$NTACode <- as.character(nyct$NTACode)

censusdat$BoroCT2010 <- paste(as.character(censusdat$BoroCode), str_pad(censusdat$CT2010, 6, pad="0"), sep="")

censusdat$NTACode <- as.character(sapply(censusdat$BoroCT2010, function(x) nyct[nyct$BoroCT2010 == x,]$NTACode))

censusdat_nta <- censusdat %>% group_by(NTACode) %>% summarise_each(funs(sum),POPTOT,
                                                                              POPDTY,
                                                                              MALE,
                                                                              FEMALE,
                                                                              MALEOVER18,
                                                                              FEMALEOVER18,
                                                                              POPOVER18,
                                                                              MEDIANAGE,
                                                                              MEDIANAGEMALE,
                                                                              MEDIANAGEFEMALE,
                                                                              EUROPEAN,
                                                                              AFRICAN,
                                                                              ASIAN,
                                                                              AMERICAN,
                                                                              PACIFIC,
                                                                              OTHERETHNICITY,
                                                                              MIXED,
                                                                              HISPANIC,
                                                                              HOUSEHOLDS,
                                                                              LESSTHANHIGHSCHOOL,
                                                                              HIGHSCHOOL,
                                                                              COLLEGE,
                                                                              BACHELOR,
                                                                              MASTER,
                                                                              PROFESSIONAL,
                                                                              DOCTORATE,
                                                                              ONLYLESSTHANHIGHSCHOOL,
                                                                              ONLYHIGHSCHOOL,
                                                                              ONLYCOLLEGE,
                                                                              ONLYBACHELOR,
                                                                              ONLYMASTER,
                                                                              ONLYPROFESSIONAL,
                                                                              ONLYDOCTORATE,
                                                                              MALEONLYLESSTHANHIGHSCHOOL,
                                                                              MALEONLYHIGHSCHOOL,
                                                                              MALEONLYCOLLEGE,
                                                                              MALEONLYBACHELOR,
                                                                              MALEONLYMASTER,
                                                                              MALEONLYPROFESSIONAL,
                                                                              MALEONLYDOCTORATE,
                                                                              FEMALEONLYLESSTHANHIGHSCHOOL,
                                                                              FEMALEONLYHIGHSCHOOL,
                                                                              FEMALEONLYCOLLEGE,
                                                                              FEMALEONLYBACHELOR,
                                                                              FEMALEONLYMASTER,
                                                                              FEMALEONLYPROFESSIONAL,
                                                                              FEMALEONLYDOCTORATE,
                                                                              MALEDROP,
                                                                              FEMALEDROP,
                                                                              MALE16TO19,
                                                                              FEMALE16TO19,
                                                                              MALEINLABOURFORCE,
                                                                              FEMALEINLABOURFORCE,
                                                                              POPINLABOURFORCE,
                                                                              MALEUNEMPLOYED,
                                                                              FEMALEUNEMPLOYED,
                                                                              POPUNEMPLOYED,
                                                                              AFRICANINLABOURFORCE,
                                                                              AFRICANUNEMPLOYED,
                                                                              AMERICANINLABOURFORCE,
                                                                              AMERICANUNEMPLOYED,
                                                                              EUROPEANINLABOURFORCE,
                                                                              EUROPEANUNEMPLOYED,
                                                                              ASIANINLABOURFORCE,
                                                                              ASIANUNEMPLOYED,
                                                                              PACIFICINLABOURFORCE,
                                                                              PACIFICUNEMPLOYED,
                                                                              MIXEDINLABOURFORCE,
                                                                              MIXEDUNEMPLOYED,
                                                                              HISPANICINLABOURFORCE,
                                                                              HISPANICUNEMPLOYED,
                                                                              OTHERINLABOURFORCE,
                                                                              OTHERUNEMPLOYED,
                                                                              LESSTHAN10000,
                                                                              MEDIANINCOME,
                                                                              WITHSOCIALSECURITY,
                                                                              WITHSSI,
                                                                              WITHPUBASST,
                                                                              POOR,
                                                                              STRUGGLING,
                                                                              POORORSTRUGGLING,
                                                                              OKAY,
                                                                              COMMUTELESSTHAN5,
                                                                              COMMUTE5TO14,
                                                                              COMMUTE15TO29,
                                                                              COMMUTE30TO44,
                                                                              COMMUTE45TO59,
                                                                              COMMUTE60TO89,
                                                                              COMMUTEMORETHAN90)

censusdat_nta <- merge(censusdat_nta, cbind(censusdat %>% group_by(NTACode) %>% summarise_each(funs(mean), GINI, HOUSEHOLDSIZE)), by="NTACode")

write.csv(censusdat_nta, "census_nta.csv")

write.csv(censusdat, "census_ct.csv")

contracts <- read.csv('mergedcontracts.csv')


#filter out NA values for startdate, enddate and scode
#contracts <- with(contracts, contracts[!is.na(startdate) & !is.na(enddate) & !is.na(scode) & !is.na(Latitude.service) & !is.na(Longitude.service), ])
contracts <- with(contracts, contracts[!is.na(scode),])
contracts$startdate <- as.Date(contracts$startdate, format = "%m/%d/%y")
contracts$enddate <- as.Date(contracts$enddate, format = "%m/%d/%y")

contracts_EMP <- with(contracts, contracts[ (scode == 'EDUC' | scode == 'EMPM' | scode== 'EMPL' | scode == 'EMPO'),])
contracts_EMP$id <- as.character(contracts_EMP$id)
colnames(contracts_EMP)[which(names(contracts_EMP) == "Boro.service")] <- "Boro_service"
colnames(contracts_EMP)[which(names(contracts_EMP) == "fiscal.year")] <- "Fiscal_year"

colnames(contracts_EMP)[which(names(contracts_EMP) == "CensusTract.service")] <- "CensusTract_service"

coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    x[i] <- y[i]
    x},
    list(...))
}


getBoroCTByContractId <- function(ID){
  boro = coalesce2(contracts_EMP[contracts_EMP$id == ID,]$Boro_service, 
                   contracts_EMP[contracts_EMP$id == ID,]$BoroCode)
  tract = contracts_EMP[contracts_EMP$id == ID,]$Censustract_service %??%
                    contracts_EMP[contracts_EMP$id == ID,]$CensusTract
  return (censusdat[censusdat$BoroCode == boro &
                     censusdat$TRACTID == tract,]$BoroCT2010)
}

contracts_EMP$BoroCT2010 <- as.character(sapply(contracts_EMP$id, getBoroCTByContractId))
contracts_EMP$NTACode <- as.character(sapply(contracts_EMP$BoroCT2010, function(x) nyct[nyct$BoroCT2010 == x,]$NTACode))
contracts_EMP$POPTOT <- as.numeric(sapply(contracts_EMP$NTACode, function(x) censusdat_nta[censusdat_nta$NTACode==x,]$POPTOT))

empcontracts_cleaned <- contracts_EMP

contracts_emp_by_year <- data.frame(BoroCT2010 = nyct$BoroCT2010,
                                    NTACode = nyct$NTACode)

`%between%`<-function(x,rng) x>rng[1] & x<rng[2]

getNoContractsByYear<- function(BoroCT, year){
  return (nrow(with(empcontracts_cleaned, empcontracts_cleaned[(Fiscal_year == year | (year %between% c(Fiscal_year, Fiscal_year + contractlen))) &
                                                                 BoroCT2010 == BoroCT,])))
}

getDollarByYear<- function(BoroCT, year){
  contset <- with(empcontracts_cleaned, empcontracts_cleaned[(Fiscal_year == year | (year %between% c(Fiscal_year, Fiscal_year + contractlen))) &
                                    BoroCT2010 == BoroCT,])
  return (sum(contset$origamt/contset$contractlen))
}
getDollarByBoroCT<- function(BoroCT){
  contset <- with(empcontracts_cleaned, empcontracts_cleaned[BoroCT2010 == BoroCT,])
  return (sum(contset$origamt/contset$contractlen))
}
contracts_emp_by_year$TOTALNO <- sapply(contracts_emp_by_year$BoroCT2010, function(x) (nrow(with(empcontracts_cleaned, empcontracts_cleaned[BoroCT2010 == x,]))))
contracts_emp_by_year$TOTALDOL <- sapply(contracts_emp_by_year$BoroCT2010, function(x) getDollarByBoroCT(x))

contracts_emp_by_year$cnt2002 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2002) 
contracts_emp_by_year$dol2002 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2002) 

contracts_emp_by_year$cnt2003 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2003) 
contracts_emp_by_year$dol2003 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2003) 

contracts_emp_by_year$cnt2004 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2004) 
contracts_emp_by_year$dol2004 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2004) 

contracts_emp_by_year$cnt2005 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2005) 
contracts_emp_by_year$dol2005 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2005) 

contracts_emp_by_year$cnt2006 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2006) 
contracts_emp_by_year$dol2006 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2006) 

contracts_emp_by_year$cnt2007 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2007) 
contracts_emp_by_year$dol2007 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2007) 

contracts_emp_by_year$cnt2008 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2008) 
contracts_emp_by_year$dol2008 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2008) 

contracts_emp_by_year$cnt2009 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2009) 
contracts_emp_by_year$dol2009 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2009) 

contracts_emp_by_year$cnt2010 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2010) 
contracts_emp_by_year$dol2010 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2010) 

contracts_emp_by_year$cnt2011 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2011) 
contracts_emp_by_year$dol2011 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2011) 

contracts_emp_by_year$cnt2012 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getNoContractsByYear(x, y), y = 2012) 
contracts_emp_by_year$dol2012 <- sapply(contracts_emp_by_year$BoroCT2010, function(x, y) getDollarByYear(x, y), y = 2012) 

contracts_emp_by_year$BoroCT2010 <- as.numeric(as.character(contracts_emp_by_year$BoroCT2010))
contracts_emp_by_year$POPTOT <- as.numeric(sapply(contracts_emp_by_year$BoroCT2010, function(x) censusdat[censusdat$BoroCT2010 == x,]$POPTOT))


contracts_emp_by_year$delcnt2005 <- contracts_emp_by_year$cnt2005 + contracts_emp_by_year$cnt2004 - contracts_emp_by_year$cnt2003 - contracts_emp_by_year$cnt2002
contracts_emp_by_year$delcnt2007 <- contracts_emp_by_year$cnt2007 + contracts_emp_by_year$cnt2006 - contracts_emp_by_year$cnt2005 - contracts_emp_by_year$cnt2004
contracts_emp_by_year$delcnt2008 <- contracts_emp_by_year$cnt2008 + contracts_emp_by_year$cnt2007 - contracts_emp_by_year$cnt2006 - contracts_emp_by_year$cnt2005
contracts_emp_by_year$delcnt2010 <- contracts_emp_by_year$cnt2010 + contracts_emp_by_year$cnt2009 - contracts_emp_by_year$cnt2008 - contracts_emp_by_year$cnt2007
contracts_emp_by_year$delcnt2012 <- contracts_emp_by_year$cnt2012 + contracts_emp_by_year$cnt2011 - contracts_emp_by_year$cnt2010 - contracts_emp_by_year$cnt2009


contracts_emp_by_year$deldol2005 <- contracts_emp_by_year$dol2005 + contracts_emp_by_year$dol2004 - contracts_emp_by_year$dol2003 - contracts_emp_by_year$dol2002
contracts_emp_by_year$deldol2007 <- contracts_emp_by_year$dol2007 + contracts_emp_by_year$dol2006 - contracts_emp_by_year$dol2005 - contracts_emp_by_year$dol2004
contracts_emp_by_year$deldol2008 <- contracts_emp_by_year$dol2008 + contracts_emp_by_year$dol2007 - contracts_emp_by_year$dol2006 - contracts_emp_by_year$dol2005
contracts_emp_by_year$deldol2010 <- contracts_emp_by_year$dol2010 + contracts_emp_by_year$dol2009 - contracts_emp_by_year$dol2008 - contracts_emp_by_year$dol2007
contracts_emp_by_year$deldol2012 <- contracts_emp_by_year$dol2012 + contracts_emp_by_year$dol2011 - contracts_emp_by_year$dol2010 - contracts_emp_by_year$dol2009

contracts_emp_by_year_nta <- contracts_emp_by_year_nta <- contracts_emp_by_year %>% group_by(NTACode) %>% summarise_each(funs(sum), -BoroCT2010)
contracts_emp_by_year_nta$TOTALDOLPERCAPITA <- contracts_emp_by_year_nta$TOTALDOL/contracts_emp_by_year_nta$POPTOT

contracts_emp_by_year$pcdelcnt2005 <- ifelse(contracts_emp_by_year$delcnt2005 ==0, 0, contracts_emp_by_year$delcnt2005 / (contracts_emp_by_year$cnt2003 + contracts_emp_by_year$cnt2002))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdelcnt2005),]$pcdelcnt2005 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdelcnt2005),]$pcdelcnt2005)
contracts_emp_by_year$pcdelcnt2007 <- ifelse(contracts_emp_by_year$delcnt2007 == 0, 0, contracts_emp_by_year$delcnt2007 / (contracts_emp_by_year$cnt2005 + contracts_emp_by_year$cnt2004))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdelcnt2007),]$pcdelcnt2007 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdelcnt2007),]$pcdelcnt2007)
contracts_emp_by_year$pcdelcnt2010 <- ifelse(contracts_emp_by_year$delcnt2010 == 0, 0, contracts_emp_by_year$delcnt2010 / (contracts_emp_by_year$cnt2007 + contracts_emp_by_year$cnt2008))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdelcnt2010),]$pcdelcnt2010 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdelcnt2010),]$pcdelcnt2010)
contracts_emp_by_year$pcdelcnt2012 <- ifelse(contracts_emp_by_year$delcnt2012 == 0, 0, contracts_emp_by_year$delcnt2012 / (contracts_emp_by_year$cnt2009 + contracts_emp_by_year$cnt2010))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdelcnt2012),]$pcdelcnt2012 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdelcnt2012),]$pcdelcnt2012)

contracts_emp_by_year$pcdeldol2005 <- ifelse(contracts_emp_by_year$deldol2005 == 0, 0, contracts_emp_by_year$deldol2005 / (contracts_emp_by_year$dol2003 + contracts_emp_by_year$dol2002))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdeldol2005),]$pcdeldol2005 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdeldol2005),]$pcdeldol2005)
contracts_emp_by_year$pcdeldol2007 <- ifelse(contracts_emp_by_year$deldol2007 == 0, 0, contracts_emp_by_year$deldol2007 / (contracts_emp_by_year$dol2005 + contracts_emp_by_year$dol2004))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdeldol2007),]$pcdeldol2007 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdeldol2007),]$pcdeldol2007)
contracts_emp_by_year$pcdeldol2010 <- ifelse(contracts_emp_by_year$deldol2010 == 0, 0, contracts_emp_by_year$deldol2010 / (contracts_emp_by_year$dol2007 + contracts_emp_by_year$dol2008))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdeldol2010),]$pcdeldol2010 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdeldol2010),]$pcdeldol2010)
contracts_emp_by_year$pcdeldol2012 <- ifelse(contracts_emp_by_year$deldol2012 == 0, 0, contracts_emp_by_year$deldol2012 / (contracts_emp_by_year$dol2009 + contracts_emp_by_year$dol2010))
contracts_emp_by_year[is.infinite(contracts_emp_by_year$pcdeldol2012),]$pcdeldol2012 <- max(contracts_emp_by_year[!is.infinite(contracts_emp_by_year$pcdeldol2012),]$pcdeldol2012)

contracts_emp_by_year$pcapcnt2005 <- ifelse(contracts_emp_by_year$cnt2005 == 0, 0, contracts_emp_by_year$cnt2005 / contracts_emp_by_year$POPTOT)
contracts_emp_by_year$pcapcnt2007 <- ifelse(contracts_emp_by_year$cnt2007 == 0, 0, contracts_emp_by_year$cnt2007 / contracts_emp_by_year$POPTOT)
contracts_emp_by_year$pcapcnt2010 <- ifelse(contracts_emp_by_year$cnt2010 == 0, 0, contracts_emp_by_year$cnt2010 / contracts_emp_by_year$POPTOT)
contracts_emp_by_year$pcapcnt2012 <- ifelse(contracts_emp_by_year$cnt2012 == 0, 0, contracts_emp_by_year$cnt2012 / contracts_emp_by_year$POPTOT)


contracts_emp_by_year_nta$pcapdol2005 <- ifelse(contracts_emp_by_year_nta$dol2005 == 0, 0, contracts_emp_by_year_nta$dol2005 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapdol2007 <- ifelse(contracts_emp_by_year_nta$dol2007 == 0, 0, contracts_emp_by_year_nta$dol2007 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapdol2010 <- ifelse(contracts_emp_by_year_nta$dol2010 == 0, 0, contracts_emp_by_year_nta$dol2010 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapdol2012 <- ifelse(contracts_emp_by_year_nta$dol2012 == 0, 0, contracts_emp_by_year_nta$dol2012 / contracts_emp_by_year_nta$POPTOT)


contracts_emp_by_year_nta$pcdelcnt2005 <- ifelse(contracts_emp_by_year_nta$delcnt2005 == 0, 0, contracts_emp_by_year_nta$delcnt2005 / (contracts_emp_by_year_nta$cnt2003 + contracts_emp_by_year_nta$cnt2002))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdelcnt2005),]$pcdelcnt2005 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdelcnt2005),]$pcdelcnt2005)
contracts_emp_by_year_nta$pcdelcnt2007 <- ifelse(contracts_emp_by_year_nta$delcnt2007 == 0, 0, contracts_emp_by_year_nta$delcnt2007 / (contracts_emp_by_year_nta$cnt2005 + contracts_emp_by_year_nta$cnt2004))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdelcnt2007),]$pcdelcnt2007 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdelcnt2007),]$pcdelcnt2007)
contracts_emp_by_year_nta$pcdelcnt2010 <- ifelse(contracts_emp_by_year_nta$delcnt2010 == 0, 0, contracts_emp_by_year_nta$delcnt2010 / (contracts_emp_by_year_nta$cnt2007 + contracts_emp_by_year_nta$cnt2008))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdelcnt2010),]$pcdelcnt2010 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdelcnt2010),]$pcdelcnt2010)
contracts_emp_by_year_nta$pcdelcnt2012 <- ifelse(contracts_emp_by_year_nta$delcnt2012 == 0, 0, contracts_emp_by_year_nta$delcnt2012 / (contracts_emp_by_year_nta$cnt2009 + contracts_emp_by_year_nta$cnt2010))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdelcnt2012),]$pcdelcnt2012 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdelcnt2012),]$pcdelcnt2012)

contracts_emp_by_year_nta$pcdeldol2005 <- ifelse(contracts_emp_by_year_nta$deldol2005 == 0, 0, contracts_emp_by_year_nta$deldol2005 / (contracts_emp_by_year_nta$dol2003 + contracts_emp_by_year_nta$dol2002))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdeldol2005),]$pcdeldol2005 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdeldol2005),]$pcdeldol2005)
contracts_emp_by_year_nta$pcdeldol2007 <- ifelse(contracts_emp_by_year_nta$deldol2007 == 0, 0, contracts_emp_by_year_nta$deldol2007 / (contracts_emp_by_year_nta$dol2005 + contracts_emp_by_year_nta$dol2004))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdeldol2007),]$pcdeldol2007 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdeldol2007),]$pcdeldol2007)
contracts_emp_by_year_nta$pcdeldol2010 <- ifelse(contracts_emp_by_year_nta$deldol2010 == 0, 0, contracts_emp_by_year_nta$deldol2010 / (contracts_emp_by_year_nta$dol2007 + contracts_emp_by_year_nta$dol2008))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdeldol2010),]$pcdeldol2010 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdeldol2010),]$pcdeldol2010)
contracts_emp_by_year_nta$pcdeldol2012 <- ifelse(contracts_emp_by_year_nta$deldol2012 == 0, 0, contracts_emp_by_year_nta$deldol2012 / (contracts_emp_by_year_nta$dol2009 + contracts_emp_by_year_nta$dol2010))
contracts_emp_by_year_nta[is.infinite(contracts_emp_by_year_nta$pcdeldol2012),]$pcdeldol2012 <- max(contracts_emp_by_year_nta[!is.infinite(contracts_emp_by_year_nta$pcdeldol2012),]$pcdeldol2012)

contracts_emp_by_year_nta$pcapcnt2005 <- ifelse(contracts_emp_by_year_nta$cnt2005 == 0, 0, contracts_emp_by_year_nta$cnt2005 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapcnt2007 <- ifelse(contracts_emp_by_year_nta$cnt2007 == 0, 0, contracts_emp_by_year_nta$cnt2007 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapcnt2010 <- ifelse(contracts_emp_by_year_nta$cnt2010 == 0, 0, contracts_emp_by_year_nta$cnt2010 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapcnt2012 <- ifelse(contracts_emp_by_year_nta$cnt2012 == 0, 0, contracts_emp_by_year_nta$cnt2012 / contracts_emp_by_year_nta$POPTOT)


contracts_emp_by_year_nta$pcapdol2005 <- ifelse(contracts_emp_by_year_nta$dol2005 == 0, 0, contracts_emp_by_year_nta$dol2005 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapdol2007 <- ifelse(contracts_emp_by_year_nta$dol2007 == 0, 0, contracts_emp_by_year_nta$dol2007 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapdol2010 <- ifelse(contracts_emp_by_year_nta$dol2010 == 0, 0, contracts_emp_by_year_nta$dol2010 / contracts_emp_by_year_nta$POPTOT)
contracts_emp_by_year_nta$pcapdol2012 <- ifelse(contracts_emp_by_year_nta$dol2012 == 0, 0, contracts_emp_by_year_nta$dol2012 / contracts_emp_by_year_nta$POPTOT)

write.csv(contracts_emp_by_year, 'contracts_emp_by_year.csv', row.names = FALSE)
write.csv(contracts_emp_by_year_nta, 'contracts_emp_by_year_nta.csv', row.names = FALSE)

nycb <- read.dbf("nycb2010.dbf")


nyct$BoroCT2010 <- as.character(nyct$BoroCT2010)
nycb$BCTCB2010 <- as.character(nycb$BCTCB2010)
nycb$BoroCode <- as.character(nycb$BoroCode)
nycb$CT2010 <- as.character(nycb$CT2010)

nycb$BoroCT2010 <- as.character(sapply(nycb$BCTCB2010, function(x) with(nyct, nyct[BoroCode == nycb[nycb$BCTCB2010 == x,]$BoroCode &
                                                     CT2010 == nycb[nycb$BCTCB2010 == x,]$CT2010,]$BoroCT2010)))



write.csv(nycb, file="nycb.csv")

lehd <- read.dbf("lehd_pts/lehd_pts_1_copy.dbf")
lehd_ct <- lehd %>% group_by(boroct2010) %>% summarise_each(funs(sum), -shape_leng, 
                                                                       -shape_area,
                                                                       -borocode,
                                                                       -boroname,
                                                                        -boroct2010,
                                                                        -cb2010,
                                                                         -ct2010,
                                                                         -bctcb2010,
                                                                         -cartodb_id)


lehd_ct$NTACode <- sapply(lehd_ct$boroct2010, function(x) with(nyct, nyct[BoroCT2010 == x,]$NTACode))

lehd_nta <- lehd_ct %>% group_by(NTACode) %>% summarise_each(funs(sum), -boroct2010)

getPCDelLowIncomeJobsRatioByYear <- function(nta, year){
  ntr22 <- paste("ce01_", str_sub(year, -2), sep="")
  dtr22 <- paste("c000_", str_sub(year, -2), sep="")
  ntr21 <- paste("ce01_", str_sub(as.character(as.numeric(year)-1), -2), sep="")
  dtr21 <- paste("c000_", str_sub(as.character(as.numeric(year)-1), -2), sep="")
  ntr12 <- paste("ce01_", str_sub(as.character(as.numeric(year)-2), -2), sep="")
  dtr12 <- paste("c000_", str_sub(as.character(as.numeric(year)-2), -2), sep="")
  ntr11 <- paste("ce01_", str_sub(as.character(as.numeric(year)-3), -2), sep="")
  dtr11 <- paste("c000_", str_sub(as.character(as.numeric(year)-3), -2), sep="")
  rec <- lehd_nta[lehd_nta$NTACode ==nta,]
  return ((((rec[,ntr22] + rec[,ntr21])/ 
              (rec[,dtr22] + rec[,dtr21]) ) - ((rec[,ntr11] + rec[,ntr12])/ 
                                                 (rec[,dtr11] + rec[,dtr12])) ) / 
            ((rec[,ntr11] + rec[,ntr12])/ 
               (rec[,dtr11] + rec[,dtr12])))
}

lehd_nta$pcdel2005 <- as.numeric(unlist((sapply(lehd_nta$NTACode, function(x, y) getPCDelLowIncomeJobsRatioByYear(x, y), y = '2005'))))

lehd_nta$pcdel2007 <- as.numeric(unlist((sapply(lehd_nta$NTACode, function(x, y) getPCDelLowIncomeJobsRatioByYear(x, y), y = '2007'))))

lehd_nta$pcdel2010 <- as.numeric(unlist((sapply(lehd_nta$NTACode, function(x, y) getPCDelLowIncomeJobsRatioByYear(x, y), y = '2010'))))


lehd_nta$pcdel2012 <- as.numeric(unlist((sapply(lehd_nta$NTACode, function(x, y) getPCDelLowIncomeJobsRatioByYear(x, y), y = '2012'))))

getPCDelLowIncomeJobsRatioByYearCT <- function(boroct, year){
  ntr22 <- paste("ce01_", str_sub(year, -2), sep="")
  dtr22 <- paste("c000_", str_sub(year, -2), sep="")
  ntr21 <- paste("ce01_", str_sub(as.character(as.numeric(year)-1), -2), sep="")
  dtr21 <- paste("c000_", str_sub(as.character(as.numeric(year)-1), -2), sep="")
  ntr12 <- paste("ce01_", str_sub(as.character(as.numeric(year)-2), -2), sep="")
  dtr12 <- paste("c000_", str_sub(as.character(as.numeric(year)-2), -2), sep="")
  ntr11 <- paste("ce01_", str_sub(as.character(as.numeric(year)-3), -2), sep="")
  dtr11 <- paste("c000_", str_sub(as.character(as.numeric(year)-3), -2), sep="")
  rec <- lehd_ct[lehd_ct$boroct2010 ==boroct,]
  return ((((rec[,ntr22] + rec[,ntr21])/ 
              (rec[,dtr22] + rec[,dtr21]) ) - ((rec[,ntr11] + rec[,ntr12])/ 
                                                 (rec[,dtr11] + rec[,dtr12])) ) / 
            ((rec[,ntr11] + rec[,ntr12])/ 
               (rec[,dtr11] + rec[,dtr12])))
}


lehd_ct$pcdel2005 <- as.numeric(unlist((sapply(lehd_ct$boroct2010, function(x, y) getPCDelLowIncomeJobsRatioByYearCT(x, y), y = '2005'))))

lehd_ct$pcdel2007 <- as.numeric(unlist((sapply(lehd_ct$boroct2010, function(x, y) getPCDelLowIncomeJobsRatioByYearCT(x, y), y = '2007'))))

lehd_ct$pcdel2010 <- as.numeric(unlist((sapply(lehd_ct$boroct2010, function(x, y) getPCDelLowIncomeJobsRatioByYearCT(x, y), y = '2010'))))


lehd_ct$pcdel2012 <- as.numeric(unlist((sapply(lehd_ct$boroct2010, function(x, y) getPCDelLowIncomeJobsRatioByYearCT(x, y), y = '2012'))))

write.csv(lehd_ct, file="lehd_ct.csv")
write.csv(lehd_nta, file="lehd_nta.csv")


