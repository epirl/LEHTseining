

library(tidyverse)
library(lubridate)
library(gt)
library(webshot)

#GET THE DATA IN
#library(RODBC)
#db<- "Z:/BBP Public/BBP Research Data/Barnegat Bay Partnership Research Database.accdb"
#con <- odbcConnectAccess2007(db)
#collections <- sqlFetch(con, "tblCollections")
##lengths <- sqlFetch(con, "tblSpecimens")
#species <- sqlFetch(con, "tblTowSpecies")
#tows <- sqlFetch(con, "tblTows")

combo1 <- merge(tows, collections, by.x= "CollectionID", by.y="CollectionID", all.x=TRUE)

sites<- c("East Green Street Cove", "East Green Street Point", "Iowa Cove", "Iowa Point") #multiple site selection

LS_collections <- combo1 %>%
  filter(`Project Name` == "Long Term Seining") %>%
  filter(`Site Name` %in% sites) %>%
  mutate (Year = year(CollectionDt)) %>%
  filter(Year <= 2020) %>%
  rename(Site_Name='Site Name')

LS_collections$Year <- as.factor(LS_collections$Year)
LS_collections <- droplevels.data.frame(LS_collections)

LS_collections <- LS_collections %>%
  mutate (Site_Name2 = case_when(Site_Name == 'Iowa Cove' ~ 'Iowa Control',
                                 Site_Name == 'Iowa Point' ~ 'Iowa Treatment',
                                 Site_Name == 'East Green Street Cove' ~ 'East Green Street Treatment',
                                 Site_Name == 'East Green Street Point' ~ 'East Green Street Control'))


######Number of seine hauls set each year###
effort <- table (LS_collections$`Site_Name2`, LS_collections$Year)
#ftable(effort)  #gives a quick look at effort table
effort<- as.data.frame(effort)

effort <- effort %>%
  rename(Site_Name=Var1) %>%
  rename(Year=Var2) %>%
  pivot_wider(names_from = Year, values_from = "Freq")

effort_table <- effort %>%
  gt(rowname_col = "Site_Name") %>%
  tab_header( title = "Seining Effort") %>%
  gtsave("Table 1-LEHT_seining_effort.png", path = "C:/Users/jvasslides/Documents/R_Projects/LEHTseining")
      
  
########Species abundance by location######

LS <- left_join(LS_collections, species, by="TowID") %>%
  unite(GenSp, Genus:Species, sep = " ", remove = TRUE, na.rm = FALSE) %>%
  #replace_na('Total Number' = 0)
  group_by(GenSp, Site_Name2) %>%
  summarise(Total = sum('Total Number', na.rm = TRUE))

## to get species sums by location 
totals <- tapply(LS$'Total Number', list(LS$GenSp, LS$'Site_Name2'), sum)
totals <- as.data.frame(totals)
totals <-cbind(rownames(totals), totals)
totals[is.na(totals)] <- "-"


