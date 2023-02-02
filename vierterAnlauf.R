
##Data Cleaning

library(dplyr)
library(tidyr)
library(tidyverse)
library(rvest)
library(readxl)


##Reading data tables from directory
df1 <- read.csv("DisasterDeclarationsSummaries.csv")
df2 <- read.csv("HousingAssistanceOwners.csv")
UScounties <- read_excel("UScounties.xlsx")
# Reading in the table from Wikipedia
page = read_html("https://en.wikipedia.org/wiki/List_of_United_States_counties_by_per_capita_income")

# Obtain the piece of the web page that corresponds to the "wikitable" node
my.table = html_node(page, ".wikitable")
# Convert the html table element into a data frame
my.table = html_table(my.table, fill = TRUE)
# Extracting and tidying the column "PerCapitaIncome"from the table and adding row names
x = as.numeric(gsub("\\[.*","",my.table[,4]))
names(x) = gsub("\\[.*","",my.table[,2])
# Excluding non-states and averages from the table
per.capita.income = x[!names(x) %in% c("United States", "Northern Mariana Islands", "Guam", "American Samoa", "Puerto Rico", "U.S. Virgin Islands")]

my.table <- my.table %>% 
  rename(county = `County or county-equivalent`)

#rename
df1$designatedArea <- sub("\\(.*", "", df1$designatedArea)
df2$county <- sub("\\(.*", "", df2$county)
UScounties$county <- sub("\\(.*", "", UScounties$county)

str(df1)
str(df2)

#df2_unique <- df2 %>% distinct(disasterNumber,.keep_all = TRUE)

# merge <- full_join(x = df1, y = df2, by = c('designatedArea' = 'county', 'disasterNumber' = 'disasterNumber'))
# 
# merge_id <- full_join(x = df1, y = df2, by = c('disasterNumber' = 'disasterNumber'))
# merge_na <- merge_id[!with(merge_id,is.na("totalMaxGrants")& is.na("repairReplaceAmount")),]
# 
# merge_na <- merge_id
# 
# merge_neu <- merge_na %>%
#   filter(!if_all(c(totalMaxGrants, repairReplaceAmount), is.na))
# summary(merge_neu)
# 
# merge_neuee <- merge_neu %>% drop_na()
# 
# summary(merge_neuee)
# 
# row.has.na <- apply(merge_na, 1, function(x){any(is.na(x))})
# sum(row.has.na)
# final.filtered <- merge_na[!row.has.na,]
# row.has.na1 <- apply(final.filtered, 1, function(x){any(is.na(x))})
# sum(row.has.na1)



merge_i <- inner_join(x = df1, y = df2, 
                      by = c('designatedArea' = 'county', 'disasterNumber' = 'disasterNumber'))

# row.has.na <- apply(merge_i, 1, function(x){any(is.na(x))})
# final.filtered <- merge_i[!row.has.na,]

#merge_i verkleinern
merge_unique <- merge_i %>% distinct(disasterNumber,.keep_all = TRUE)
#merge_unique <- merge_i %>% 
# group_by(disasterNumber) %>% 
#summarize(across(where(is.numeric), sum)) %>% 
#distinct(disasterNumber, .keep_all = TRUE)
merge_unique

merge_unique %>% 
  select(designatedArea, zipCode, state.x) %>% 
  as_tibble()

UScounties %>% 
  select(county, county_ascii, state_id) %>% 
  as_tibble()


library(stringr)
merge_unique$designatedArea <- str_remove(string = merge_unique$designatedArea, pattern = " +$")
#str_extract(string = , pattern = "")

###left join
merge_county <- left_join(x = merge_unique, y = UScounties, by = c('designatedArea' = 'county', "state.x" = "state_id"))
###left join
merge_capita <- left_join(x = merge_county, y = my.table, by = c('designatedArea' = 'county', "state_name" ="State, federal district or territory"))

#generate better names
names(merge_capita) <- names(merge_capita) %>%  make.names()

##my.table transforming the data type
merge_capita$Population <- sapply(merge_capita$Population, function(x) as.numeric(gsub(",", "", x)))
merge_capita$Number.ofhouseholds <- sapply(merge_capita$Number.ofhouseholds, function(x) as.numeric(gsub(",", "", x)))
merge_capita$Rank <- sapply(merge_capita$Rank, function(x) as.numeric(gsub(",", "", x)))
merge_capita$Per.capitaincome <- sapply(merge_capita$Per.capitaincome, function(x) as.numeric(gsub("[$,]", "", x)))
merge_capita$Medianhouseholdincome <- sapply(merge_capita$Medianhouseholdincome, function(x) as.numeric(gsub("[$,]", "", x)))
merge_capita$Medianfamilyincome <- sapply(merge_capita$Medianfamilyincome, function(x) as.numeric(gsub("[$,]", "", x)))

##dropping variables not used for our project
colnames(merge_capita) # show all variables
df.prep = subset(merge_capita, select = c(disasterNumber, fyDeclared, incidentType, 
                                          declarationTitle, incidentBeginDate, incidentEndDate,
                                          designatedArea, totalDamage,
                                          totalApprovedIhpAmount, repairReplaceAmount,
                                          state_name, lat, lng, population, Per.capitaincome))
colnames(df.prep)
##transform variable to right data type
df.prep$incidentBeginDate <- str_remove(string = df.prep$incidentBeginDate, pattern = "T00:00:00.000Z")
df.prep$incidentEndDate <- str_remove(string = df.prep$incidentEndDate, pattern = "T00:00:00.000Z")
df.prep.1 <- df.prep
df.prep.1$incidentBeginDate <- as.Date(df.prep.1$incidentBeginDate)
df.prep.1$incidentEndDate <- as.Date(df.prep.1$incidentEndDate)

df.prep.1$duration <- difftime(df.prep.1$incidentEndDate, df.prep.1$incidentBeginDate, units="days")
str(df.prep.1)

##drop columns without lang/lat because it is not a US-State (see PR)
df.prep.2 <- df.prep.1 %>% drop_na(lat)

##change 'incident Type' from character to factor
df.prep.3 <- df.prep.2
class(df.prep.3$incidentType) 
df.prep.3$incidentType <- as.factor(df.prep.3$incidentType) 

class(df.prep.3$state_name) 
df.prep.3$state_name <- as.factor(df.prep.3$state_name) 
colnames(df.prep.3)
#df.prep.3$declaration.title.fac <- as.factor(df.prep.3$declarationTitle) 

#reorder and rename columns to make the dataset more readable and more logically organized
df.prep.4 <- df.prep.3[,c("disasterNumber","incidentType","designatedArea", "state_name",
                             "lat", "lng", "totalDamage", "totalApprovedIhpAmount",
                             "repairReplaceAmount", "population", "Per.capitaincome",
                             "incidentBeginDate", "incidentEndDate", "duration")]

df.prep.5 <- df.prep.4
colnames(df.prep.5) <- c("Disaster#", "Type", "County", "State", "Latitude",
                                "Longitude", "Damage$", "Approved$",
                                "Repair$", "Population", "IncomeCapita", "DisasterBegin",
                                "DisasterEnd", "Duration")


#graphical analysis
##first steps
boxplot(df.prep.3$totalDamage ~ df.prep.3$incidentType)







