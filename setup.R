library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(car)

table2=read.csv("Table2ReportSelector.csv", stringsAsFactors = FALSE)%>%
  rename(PlaceFIPS=Expr1007)%>%
  gather(variable, value, -Full_name:-PlaceFIPS)

areas=read.csv("RCS County Place names.csv", stringsAsFactors = FALSE)%>%
  mutate(id=as.numeric(paste0(countyfips,placefips)))

sdo=table2%>%
  select(CountyFIPS:value)%>%
  filter(grepl("Tp", variable) | grepl("Gqp", variable) | grepl("Hp", variable) | grepl("Pph", variable) | 
           grepl("Thu", variable) | grepl("Ohu", variable) | grepl("Vhu", variable) | grepl("Vr", variable), 
         str_sub(variable, -1,-1)!="l")%>%
  mutate(year=ifelse(str_sub(variable, -1,-1)=="c" | str_sub(variable, -1,-1)=="x", paste0("20", str_sub(variable, -2-1)), paste0("20", str_sub(variable, -2,-1))),
         year=ordered(year, levels=c("2010c", "2010x", "2010", "2011", "2012", "2013", "2014", "2015", "2016"),
                      labels=c("2010 Census", "2010 Adj. Census", "July 2010", "July 2011", "July 2012", "July 2013",
                               "July 2014", "July 2015", "July 2016")),
         variable=ifelse(grepl("Gqp", variable) | grepl("Pph", variable) | grepl("Thu", variable) | grepl("Ohu", variable) | grepl("Vhu", variable),
                         str_sub(variable, 1,3), str_sub(variable, 1,2)),
         variable=car::recode(variable, "'Tp'='Total Population'; 'Gqp'='Group Quarters Population'; 'Hp'='Household Population';
                              'Pph'='Household Size'; 'Thu'='Total Housing Units'; 'Ohu'= 'Households'; 'Vhu'='Vacant Housing Units'; 
                              'Vr'='Vacancy Rate'"),
         variable=ordered(variable, levels=c("Total Population", "Group Quarters Population", "Household Population", 
                                             "Household Size", "Total Housing Units", "Households", "Vacant Housing Units",
                                             "Vacancy Rate")),
         id=as.numeric(paste0(CountyFIPS,PlaceFIPS)))%>%
  select(id, Year=year, Variable=variable, value)%>%
  spread(Year, value)

contact=table2%>%
  filter(variable=="Thu10")%>%
  mutate(id=as.numeric(paste0(CountyFIPS,PlaceFIPS)))%>%
  select(id, Name=Full_name, Title, 
        Email=Email, Address=Complete_address, Fax=Fax)

housing=table2%>%
  filter(grepl("LocalBP", variable) | grepl("LocalCO", variable) | grepl("LocalDemo", variable) | grepl("LocalMHChange", variable) |
           grepl("CensusBP", variable) | grepl("Thu", variable), variable!="Thu10c",variable!="Thu10x", variable!="Thu10l",
         variable!="Thu11l", variable!="Thu12l")%>%
  mutate(year=as.numeric(paste0("20",str_sub(variable, -2,-1))),
         variable=str_sub(variable, 1, nchar(variable)-2),
         id=as.numeric(paste0(CountyFIPS,PlaceFIPS)))%>%
  select(id, year, variable, value)%>%
  spread(variable, value)%>%
  group_by(id)%>%
  arrange(year)%>%
  mutate(ThuChange=as.numeric(Thu)-lag(as.numeric(Thu)))%>%
  gather(variable, value, -id:-year)%>%
  filter(year>2010, variable!="Thu")%>%
  mutate(year=ordered(year, levels=2011:2016,
         labels=c("2010 to 2011", "2011 to 2012", "2012 to 2013", "2013 to 2014", "2014 to 2015", "2015 to 2016")),
         variable=ordered(variable, levels=c("ThuChange", "CensusBP", "LocalBP", "LocalCO", "LocalDemo", "LocalMHChange"),
                          labels=c("Total Housing Unit Change", "Census Building Permits", "Local Building Permits",
                          "Local Certificates of Occupancy", "Local Demolition Permits", "Local Mobile Home Change")),
         value=ifelse(is.na(value), 0, value))%>%
  select(id, Variable=variable, year, value)%>%
  spread(year, value)%>%
  ungroup()
  

census=table2%>%
  filter(grepl("POPESTIMATE", variable) | grepl("hu_", variable))%>%
  mutate(year=str_sub(variable, -4,-1),
         variable=str_sub(variable, 1, nchar(variable)-4),
         variable=ifelse(variable=="POPESTIMATE", "Total Population", "Total Housing Units"),
         id=as.numeric(paste0(CountyFIPS,PlaceFIPS)))%>%
  select(id, year, Variable=variable, value)%>%
  spread(year, value)

