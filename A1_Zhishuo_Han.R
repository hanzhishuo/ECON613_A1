library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
options(scipen=999)
hhpath = "D:/duke/ECON613/A1/Data/hh"
indpath = "D:/duke/ECON613/A1/Data/ind"


#Exercise1
assign(paste0("dathh2007"), fread(paste0(hhpath,"/","dathh2007.csv")))
assign(paste0("dathh2005"), fread(paste0(hhpath,"/","dathh2005.csv")))
assign(paste0("datind2008"), fread(paste0(indpath,"/","datind2008.csv")))
assign(paste0("datind2016"), fread(paste0(indpath,"/","datind2016.csv")))
assign(paste0("datind2019"), fread(paste0(indpath,"/","datind2019.csv")))
assign(paste0("datind2009"), fread(paste0(indpath,"/","datind2009.csv")))
assign(paste0("datind2005"), fread(paste0(indpath,"/","datind2005.csv")))
assign(paste0("datind2010"), fread(paste0(indpath,"/","datind2010.csv")))
assign(paste0("dathh2011"), fread(paste0(hhpath,"/","dathh2011.csv")))
assign(paste0("datind2011"), fread(paste0(indpath,"/","datind2011.csv")))

#Number of households surveyed in 2007
nrow(dathh2007[dathh2007$idmen])

#Number of households with marital status "Couple with kids" in 2005.
nrow(dathh2005[dathh2005$mstatus=="Couple, with Kids"])

#Number of individuals surveyed in 2008.
nrow(datind2008[datind2008$idind])

#Number of individuals aged between 25 and 35 in 2016
nrow(datind2016[datind2016$age>=25 & datind2016$age<=35])

#Cross-table gender/profession in 2009.
datind2009 %>% count(gender,profession)

#Distribution of wages in 2005 and 2019. Report the mean, the standard deviation, 
#the inter-decile ratio D9/D1 and the Gini coefficient.

mean(datind2005$wage, na.rm = TRUE)
sd(datind2005$wage, na.rm = TRUE)
D9 = quantile(datind2005$wage, probs=0.9, na.rm = TRUE)
D1 = quantile(datind2005$wage, probs=0.1, na.rm = TRUE)
ratio_2005 = D9/D1

#GINI function
gini <- function(x, weights=rep(1,length=length(x))){
  ox <- order(x)
  x <- x[ox]
  weights <- weights[ox]/sum(weights)
  p <- cumsum(weights)
  nu <- cumsum(weights*x)
  n <- length(nu)
  nu <- nu / nu[n]
  sum(nu[-1]*p[-n]) - sum(nu[-n]*p[-1])
}

datind2005 = datind2005 %>%
  drop_na(wage)

gini(datind2005$wage)

mean(datind2019$wage, na.rm = TRUE)
sd(datind2019$wage, na.rm = TRUE)
D9 = quantile(datind2019$wage, probs=0.9, na.rm = TRUE)
D1 = quantile(datind2019$wage, probs=0.1, na.rm = TRUE)
ratio_2019 = D9/D1

datind2019 = datind2019 %>%
  drop_na(wage)
gini(datind2019$wage)




#Distribution of age in 2010. Plot an histogram. Is there any difference between men and women?
hist(datind2010$age)

male2010 <- datind2010[datind2010$gender=="Male"]
female2010 <- datind2010[datind2010$gender=="Female"]
hist(male2010$age)
hist(female2010$age)

#Number of individuals in Paris in 2011
m_2011 <- merge(dathh2011,datind2011, by.x = "idmen", by.y = "idmen")
nrow(m_2011[m_2011$location=="Paris"])

#exercise2
#Read all individual datasets from 2004 to 2019. Append all these datasets.
setwd(indpath)
ind_file = list.files(path=indpath)
ind2 <- lapply(ind_file, function(x){
  df <- fread(file = x, stringsAsFactors =FALSE)
  df$idind <- as.character(df$idind)
  df$idmen <- as.character(df$idmen)
  df
  
})

ind <- do.call(rbind, ind2)

#Read all household datasets from 2004 to 2019. Append all these datasets.
setwd(hhpath)
hh_file = list.files(path=hhpath)

hh2 <- lapply(hh_file, function(x){
  df <- fread(file = x, stringsAsFactors =FALSE)
  df$idmen <- as.character(df$idmen)
  df
    
  })
  

hh <- do.call(rbind, hh2)

#List the variables that are simultaneously present in the individual and household datasets.
intersect(names(hh), names(ind))

#Merge the appended individual and household datasets.
ind_hh <- ind %>%
  full_join(hh, by=c('idmen','year'))

#Number of households in which there are more than four family members
hh_four <- ind_hh %>%
  group_by(idmen, year) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count > 4) %>%
  distinct(idmen, .keep_all = TRUE)
nrow(hh_four[hh_four$count])

#Number of households in which at least one member is unemployed
hh_unemployed = ind_hh %>%
  group_by(idmen, year) %>%
  filter(empstat=="Unemployed") %>%
  mutate(count = n()) %>%
  ungroup() %>%
  filter(count >=1) %>%
  distinct(idmen, .keep_all = TRUE)
nrow(hh_unemployed[hh_unemployed$count])


#Number of households in which at least two members are of the same profession
hh_profession = ind_hh %>%
  drop_na(profession) %>%
  group_by(idmen, year, profession) %>%
  mutate(count = n())%>%
  ungroup() %>%
  filter(count > 1) %>%
  distinct(idmen, .keep_all = TRUE)
nrow(hh_profession[hh_profession$count])

#Number of individuals in the panel that are from household-Couple with kids
ind_couplekids = ind_hh %>%
  filter(mstatus=="Couple, with Kids") %>%
  distinct(idind, .keep_all = TRUE)
nrow(ind_couplekids)

#Number of individuals in the panel that are from Paris.
ind_Paris = ind_hh %>%
  filter(location=="Paris") %>%
  distinct(idind, .keep_all = TRUE)
nrow(ind_Paris)

#Find the household with the most number of family members. Report its idmen.
hh_max = ind_hh %>%
  group_by(idmen, year)%>%
  mutate(count = n())%>%
  ungroup() %>%
  distinct(idmen,year, .keep_all = TRUE)%>%
  arrange(desc(count))


hh_max$idmen[1]
hh_max$idmen[2]

#Number of households present in 2010 and 2011.
hh_1011 = ind_hh %>%
  filter(year == 2010|year ==2011)%>%
  group_by(year) %>%
  distinct(idmen, .keep_all = TRUE)

nrow(hh_1011)

#exercise 3
#Find out the year each household enters and exit the panel. Report the 
#distribution of the time spent in the survey for each household.
hh_enter_exit = ind_hh %>%
  group_by(idmen) %>%
  mutate(max = max(year)) %>%
  mutate(min = min(year)) %>%
  mutate(time_spent = max(year) - min(year)) %>%
  ungroup() %>%
  distinct(idmen, .keep_all = TRUE)

dist <- hh_enter_exit %>%
  select(idmen, time_spent)

#Based on datent, identify whether or not a household moved into its current 
#dwelling at the year of survey.

hh_datent = ind_hh %>%
  drop_na(datent)%>%
  mutate(identify_dwelling = ifelse(year == datent, 1,0))

#report first 10 rows
report_10 <- hh_datent %>%
  select(idmen, identify_dwelling)
head(report_10, 10)
  
hh_datent = hh_datent %>%
  group_by(year) %>%
  mutate(share = sum(identify_dwelling == 1)/sum(identify_dwelling >= 0))%>%
  distinct(year, .keep_all = TRUE)

#plot share
share <- hh_datent %>%
  select(share, year)

plot_share <- ggplot(share, aes(x=year, y=share)) + 
  geom_line()
  
plot_share 

#Based on myear and move, identify whether or not household migrated at the year of survey.
hh_myear <- ind_hh %>%
  filter(year <= 2014) %>%
  filter(!is.na(myear)) %>%
  mutate(move = ifelse(myear == year, 2,1))

hh_move <- ind_hh %>%
  filter(year > 2014) %>%
  filter(!is.na(move))

myear_move <- rbind(hh_myear, hh_move)

#Report the first 10 rows of your result
report_10_move <- myear_move %>%
  select(idmen, move)
head(report_10_move, 10)

myear_move <- myear_move %>%
  group_by(year) %>%
  mutate(share2 = sum(move == 2)/sum(move >= 0)) %>%
  distinct(year, .keep_all = TRUE)

#plot the share of individuals in that situation across years
share2 <- myear_move %>%
  select(share2, year)

plot_share2 <- ggplot(share2, aes(x=year, y=share2)) + 
  geom_line()

plot_share2


#Mix the two plots you created above in one graph, clearly label the graph
mix <- share %>%
  full_join(share2, by = "year")

mix_plot = ggplot() + 
  geom_line(data = share, aes(x = year, y = share), color = "blue") +
  geom_line(data = share2, aes(x = year, y = share2), color = "red") +
  xlab('Year') +
  ylab('Share')

mix_plot


#For households who migrate, find out how many households had at least one family 
#member changed his/her profession or employment status.

hh_mirgrate = ind_hh %>%
  drop_na(datent)%>%
  mutate(identify_dwelling = ifelse(year == datent, 1,0)) %>%
  filter(identify_dwelling == 1) %>%
  distinct(idmen, .keep_all = TRUE) %>%
  select(idmen, identify_dwelling)
  
migrate = hh_mirgrate %>%
  full_join(ind_hh, by = 'idmen')

migrate = migrate %>%
  drop_na(datent) %>%
  filter(identify_dwelling==1)%>%
  filter(!is.na(profession)|!is.na(empstat))%>%
  group_by(idmen, idind) %>%
  mutate(last_profession = lag(profession, 1, order_by = year))%>%
  mutate(last_empstat = lag(empstat, 1, order_by = year)) %>%
  ungroup() %>%
  group_by(idmen, year) %>%
  distinct(idind, .keep_all = TRUE) %>%
  ungroup() %>%
  mutate(change_pro_emp = ifelse(profession != last_profession|empstat != last_empstat,1, 0)) %>%
  group_by(idmen) %>%
  mutate(total_change = sum(!is.na(change_pro_emp) & change_pro_emp == 1)) %>%
  ungroup() %>%
  filter(total_change >= 1)%>%
  distinct(idmen, .keep_all = TRUE)
  
nrow(migrate)
  






#exercise 4
exit_enter <- ind_hh %>%
  group_by(idind) %>%
  mutate(max = max(year)) %>%
  mutate(min = min(year)) %>%
  ungroup() %>%
  distinct(idind, year, .keep_all = TRUE)
attrition <- ind_hh %>%
  left_join(exit_enter, by = "idind") %>%
  rename(year = year.x) %>%
  filter(year!=2004 & year!=2019) %>%
  mutate(entry = ifelse(year==min, 1, 0)) %>%
  mutate(exit = ifelse(year==max, 1, 0)) %>%
  select(year, entry, exit) %>%
  group_by(year) %>%
  mutate(total_entry = sum(entry == 1)) %>%
  mutate(total_exit = sum(exit == 1)) %>%
  mutate(proportion  = total_exit/total_entry) %>%
  ungroup() %>%
  distinct(year, .keep_all = TRUE) %>%
  select(year, proportion)
attrition
  
  
  
  
  
  
  
