library(tidyverse)
library(jsonlite)
library(rjson)
library(RJSONIO)
library(data.table)
library(rapport)
library(stringi)
country = read.csv("country.csv",header = FALSE)
colnames(country) = c("country_code","country_name")
time = read.csv("timezone.csv",header = FALSE)
colnames(time) = c("zone_id","abbreviation","time_start","gmt_offset","dst")
zone = read.csv("zone.csv",header = FALSE)
colnames(zone) = c("zone_id","country_code","zone_name")
json_file = fromJSON("https://raw.githubusercontent.com/annexare/Countries/master/data/countries.json")
json_file <- lapply(json_file, function(x) {
  x[sapply(x, is.null)] <- NA
  unlist(x)
})

df<-as.data.frame(do.call("cbind", json_file))
tran = transpose(df)
rownames(tran) = colnames(df)
colnames(tran) = rownames(df)
languages = tran %>% select(name,languages1,languages2)
langCode= read.csv("https://pkgstore.datahub.io/JohnSnowLabs/iso-639-1-and-639-2-language-codes-list/iso-639-1-and-639-2-language-codes-list-csv_csv/data/8e505595c047ed56df3cf5104b2d848e/iso-639-1-and-639-2-language-codes-list-csv_csv.csv")
languages[1:3] = lapply(languages[1:3],as.character)
rownames(languages) = seq(1,nrow(languages),by = 1)
langCode = langCode[3:4]
langCode[1:2] = lapply(langCode[1:2],as.character)

set1 = langCode %>%full_join(languages,by = c("Alpha2_Code" = "languages1"),copy = TRUE,keep = TRUE)
set2 = set1 %>%full_join(langCode,by = c("languages2" = "Alpha2_Code"))
set2 = set2[-which(is.na(set2$name)),]
languages = set2[,c(3,1,2,4,5)]
colnames(languages) = c("country","codePrimary","namePrimary","codeSecondary","nameSecondary")
timeZone = full_join(zone,time,by = "zone_id")
countryTimeZone = full_join(timeZone,country,by = "country_code")
countryTimeZone$country_name = gsub("Russian Federation","Russia",countryTimeZone$country_name)

#countryTimeZone = countryTimeZone %>% filter(dst == 1)
countryTimeZone$country_code = tolower(countryTimeZone$country_code)
countryTimeZoneRanked= countryTimeZone %>% 
  select(zone_id,zone_name,country_code,country_name,abbreviation,time_start,gmt_offset,dst) %>%
  group_by(country_name,gmt_offset) %>%
  mutate(id = row_number(),count = n())

countryTimeZoneLang =  full_join(countryTimeZoneRanked,languages,by = c("country_name"="country"))
countryTimeZoneLang = countryTimeZoneLang %>% select(country_name,country_code,
                                                     namePrimary,codePrimary,nameSecondary,codeSecondary,
                                                     zone_name,zone_id,abbreviation,gmt_offset,time_start,dst)
colnames(countryTimeZoneLang) = c("country_name","country_code","namePrimary","codePrimary","nameSecondary","codeSecondary","zone_name","zone_id","abbreivation",     
                                  "gmt_offset","time_start","dst")


for (i in 1:nrow(countryTimeZoneLang)) {
    if (is.na(countryTimeZoneLang$nameSecondary[i])) {
      countryTimeZoneLang$codeSecondary[i] = NA
    }}


countryTimeZoneLang$gmt_offset_hours = round(as.double(countryTimeZoneLang$gmt_offset/3600),2)



countryTimeZoneLangDistinct = countryTimeZoneLang %>% group_by(country_name,zone_name,gmt_offset_hours,dst) %>% mutate(rank = row_number())
countryTimeZoneLangDistinct = countryTimeZoneLangDistinct %>% ungroup()
countryTimeZoneLangDistinct = countryTimeZoneLangDistinct %>% filter(countryTimeZoneLangDistinct$rank == 1)

countryTimeZoneLangDistinct = countryTimeZoneLangDistinct[-which(is.na(countryTimeZoneLangDistinct$gmt_offset_hours)),]
countryTimeZoneLangDistinctRanked =countryTimeZoneLangDistinct %>%  group_by(country_name,zone_name,dst) %>% distinct() %>% arrange(country_name,desc(time_start)) %>%mutate(timeRank = row_number())
countryTimeZoneLangDistinctRanked = countryTimeZoneLangDistinctRanked %>% filter(timeRank == 1)


splits = as.data.frame(str_split_fixed(countryTimeZoneLangDistinctRanked$zone_name,pattern = '/',n = 2))
countryTimeZoneLangDistinctRanked$zone_name = splits$V2


colnames(countryTimeZoneLangDistinctRanked)
View(countryTimeZoneLangDistinctRanked)
final = countryTimeZoneLangDistinctRanked[c(1,2,3,4,5,6,7,9,10,12,13)]
write.csv(final,'TimeZoneCountryLanguage.csv')
