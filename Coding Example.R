#Group 9, Ka Long (Clark) Lei, A14746787


China.investment <- read.csv("ChinaFDI.csv")

#subset data 
#only use the data for research and  Chinese Official Development Assistance 
#in the strictest sense
China.investment.1 <- subset(China.investment, recommended_for_research == "TRUE" & flow_class == "ODA-like")

#only observe the countries in Asia, Africa, and the Pacific
China.investment.2 <- subset(China.investment.1, recipient_region == "Africa")

#generate variable from 2000 to 2014
observed_year <- c()
for( i in 2000:2014){ 
  observed_year <- c(observed_year,i)
} 

#generate variable for the recipent countries
observed_countries <- unique(China.investment.2$recipient_condensed)

#make a table that show the total China investments to recipents by each year
RECIPIENT <- c()
YEAR <- c()
AMOUNT_PER_YEAR <- c()
f <- c()

for( i in 1:length(observed_countries)){
  for( j in 1:length(observed_year)) {
    f <- c(f, i)
    YEAR <- c(YEAR, observed_year[j])
    AMOUNT_PER_YEAR <- c(AMOUNT_PER_YEAR, 
                         sum(China.investment.2$usd_defl_2014[China.investment.2$year == observed_year[j] 
                                                              & China.investment.2$recipient_condensed == observed_countries[i]], na.rm = TRUE) )
  }
}

RECIPIENT <- observed_countries[f]

#final table (IV)
China.investment.3 <- cbind.data.frame(RECIPIENT,YEAR,AMOUNT_PER_YEAR)



#81a, 81b, 89e
library(foreign)
SouthAmerica <- read.spss("merged_r6_data_2016_36countries2.sav",to.data.frame=TRUE)
write.table(SouthAmerica, "mydataFormR.txt")

#q81a
#create table to store the data of question 81a
q81a <- cbind.data.frame(SouthAmerica$COUNTRY, SouthAmerica$Q81A)

african_countries <- unique(q81a$`SouthAmerica$COUNTRY`)
o <- c()
p <- c()
q <- c()
r <- c()
opinions <- c()

for ( i in 1:length(african_countries)) {
  o <- c(o, sum(q81a$`SouthAmerica$COUNTRY` == african_countries[i]
                & q81a$`SouthAmerica$Q81A` == "A lot", na.rm = TRUE))
  p <- c(p, sum(q81a$`SouthAmerica$COUNTRY` == african_countries[i]
                & q81a$`SouthAmerica$Q81A` == "Some", na.rm = TRUE))   
  q <- c(q, sum(q81a$`SouthAmerica$COUNTRY` == african_countries[i]
                & q81a$`SouthAmerica$Q81A` == "A little", na.rm = TRUE))  
  r <- c(r, sum(q81a$`SouthAmerica$COUNTRY` == african_countries[i]
                & q81a$`SouthAmerica$Q81A` == "None", na.rm = TRUE)) 
}

for ( i in 1:length(african_countries)){
  opinions <- c(opinions, (o[i]*8 + p[i]*6 + q[i]*4 + r[i]*2) / (o[i] + p[i] + q[i] + r[i]))
}

#q81b
#create table to store the data of question 81b
q81b <- cbind.data.frame(SouthAmerica$COUNTRY, SouthAmerica$Q81B)

ob <- c()
pb <- c()
qb <- c()
rb <- c()
sb <- c()
opinions_b <- c()

for ( i in 1:length(african_countries)) {
  ob <- c(ob, sum(q81b$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q81b$`SouthAmerica$Q81B` == "Very positive", na.rm = TRUE))
  pb <- c(pb, sum(q81b$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q81b$`SouthAmerica$Q81B` == "Somewhat positive", na.rm = TRUE))  
  qb <- c(qb, sum(q81b$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q81b$`SouthAmerica$Q81B` == "Neither positive nor negative", na.rm = TRUE))  
  rb <- c(rb, sum(q81b$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q81b$`SouthAmerica$Q81B` == "Somewhat negative", na.rm = TRUE)) 
  sb <- c(sb, sum(q81b$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q81b$`SouthAmerica$Q81B` == "Very negative", na.rm = TRUE))
}

for ( i in 1:length(african_countries)){
  opinions_b <- c(opinions_b, (ob[i]*9 + pb[i]*7 + qb[i]*5 + rb[i]*3 + sb[i]*1) 
                  / (ob[i] + pb[i] + qb[i] + rb[i] + sb[i]))
}

#q89e
#create table to store the data of question 89e
q89e <- cbind.data.frame(SouthAmerica$COUNTRY, SouthAmerica$Q89E)
oc <- c()
pc <- c()
qc <- c()
rc <- c()
sc <- c()
opinions_e <- c()

for ( i in 1:length(african_countries)) {
  oc <- c(oc, sum(q89e$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q89e$`SouthAmerica$Q89E` == "Strongly like", na.rm = TRUE))
  pc <- c(pc, sum(q89e$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q89e$`SouthAmerica$Q89E` == "Somewhat  like", na.rm = TRUE))  
  qc <- c(qc, sum(q89e$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q89e$`SouthAmerica$Q89E` == "Would not care", na.rm = TRUE))  
  rc <- c(rc, sum(q89e$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q89e$`SouthAmerica$Q89E` == "Somewhat dislike", na.rm = TRUE)) 
  sc <- c(sc, sum(q89e$`SouthAmerica$COUNTRY` == african_countries[i]
                  & q89e$`SouthAmerica$Q89E` == "Strongly  dislike", na.rm = TRUE))
}


for ( i in 1:length(african_countries)){
  opinions_e <- c(opinions_e, (oc[i]*9 + pc[i]*7 + qc[i]*5 + rc[i]*3 + sc[i]*1) 
                  / (oc[i] + pc[i] + qc[i] + rc[i] + sc[i]))
}

#combine the three question tables together
Africa_opinions <- cbind.data.frame(african_countries, opinions, opinions_b, opinions_e)
install.packages("plyr")
library(plyr)
install.packages("dplyr")
library(dplyr)
names(Africa_opinions)[1] <- "RECIPIENT"

China.investment.3[China.investment.3 == 0] <- NA


Investment_to_popular <-left_join(China.investment.3, Africa_opinions)
options(scipen = 999)
names(Investment_to_popular)[4] <- "Q81A"
names(Investment_to_popular)[5] <- "Q81B"
names(Investment_to_popular)[6] <- "Q89E"
Investment_to_popular$popular_opinion <- (Investment_to_popular$Q81A + Investment_to_popular$Q81B)/2


#GDP
#input another dataset gdp of different regions and subset Africa data
GDP <- read.csv("GDP.csv", header=TRUE, check.names=FALSE)
Region <- read.csv("Region.csv")
names(GDP)[2] <- "Country.Code"
GDP_africa <- left_join(GDP, Region)
GDP_africa <- subset(GDP_africa, is.na(GDP_africa$Region) == FALSE)


country <- unique(GDP_africa$`Country Name`)
country_gdp <- c()
m <- c()
year <- c()

EGYPT <- c( GDP$`2000`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2001`[GDP$`Country Name` == "Egypt, Arab Rep."], 
            GDP$`2002`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2003`[GDP$`Country Name` == "Egypt, Arab Rep."],
            GDP$`2004`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2005`[GDP$`Country Name` == "Egypt, Arab Rep."],
            GDP$`2006`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2007`[GDP$`Country Name` == "Egypt, Arab Rep."],
            GDP$`2008`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2009`[GDP$`Country Name` == "Egypt, Arab Rep."],
            GDP$`2010`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2011`[GDP$`Country Name` == "Egypt, Arab Rep."], 
            GDP$`2012`[GDP$`Country Name` == "Egypt, Arab Rep."], GDP$`2013`[GDP$`Country Name` == "Egypt, Arab Rep."],
            GDP$`2014`[GDP$`Country Name` == "Egypt, Arab Rep."])
MOROCCO <- c(GDP$`2000`[GDP$`Country Name` == "Morocco"], GDP$`2001`[GDP$`Country Name` == "Morocco"], 
             GDP$`2002`[GDP$`Country Name` == "Morocco"], GDP$`2003`[GDP$`Country Name` == "Morocco"],
             GDP$`2004`[GDP$`Country Name` == "Morocco"], GDP$`2005`[GDP$`Country Name` == "Morocco"],
             GDP$`2006`[GDP$`Country Name` == "Morocco"], GDP$`2007`[GDP$`Country Name` == "Morocco"],
             GDP$`2008`[GDP$`Country Name` == "Morocco"], GDP$`2009`[GDP$`Country Name` == "Morocco"],
             GDP$`2010`[GDP$`Country Name` == "Morocco"], GDP$`2011`[GDP$`Country Name` == "Morocco"], 
             GDP$`2012`[GDP$`Country Name` == "Morocco"], GDP$`2013`[GDP$`Country Name` == "Morocco"],
             GDP$`2014`[GDP$`Country Name` == "Morocco"])
TUNISIA <- c(GDP$`2000`[GDP$`Country Name` == "Tunisia"], GDP$`2001`[GDP$`Country Name` == "Tunisia"], 
             GDP$`2002`[GDP$`Country Name` == "Tunisia"], GDP$`2003`[GDP$`Country Name` == "Tunisia"],
             GDP$`2004`[GDP$`Country Name` == "Tunisia"], GDP$`2005`[GDP$`Country Name` == "Tunisia"],
             GDP$`2006`[GDP$`Country Name` == "Tunisia"], GDP$`2007`[GDP$`Country Name` == "Tunisia"],
             GDP$`2008`[GDP$`Country Name` == "Tunisia"], GDP$`2009`[GDP$`Country Name` == "Tunisia"],
             GDP$`2010`[GDP$`Country Name` == "Tunisia"], GDP$`2011`[GDP$`Country Name` == "Tunisia"], 
             GDP$`2012`[GDP$`Country Name` == "Tunisia"], GDP$`2013`[GDP$`Country Name` == "Tunisia"],
             GDP$`2014`[GDP$`Country Name` == "Tunisia"])

gdp3 <- c(EGYPT, MOROCCO, TUNISIA)
years3 <- c(2000:2014, 2000:2014, 2000:2014)
v <- c()

for (i in 1:3 ){
  for (j in 1:length(observed_year)){
    v <- c(v, i)
  }
}

countries3 <- c("Egypt", "Morocco", "Tunisia")
countries3 <- countries3[v]
North_africa <- cbind.data.frame(countries3, years3, gdp3)
names(North_africa)[1] <- "RECIPIENT"
names(North_africa)[2] <- "YEAR"
names(North_africa)[3] <- "country_gdp"

#create a dataset of showing GDP for each African countries from 2000 to 2014
for (i in 1:length(country)){
  m <- c(m, i, i, i, i, i, i, i, i, i, i, i, i, i, i, i)
  country_gdp <- c(country_gdp, GDP_africa$`2000`[ GDP_africa$`Country Name`== country[i]], 
                   GDP_africa$`2001`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2002`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2003`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2004`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2005`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2006`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2007`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2008`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2009`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2010`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2011`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2012`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2013`[ GDP_africa$`Country Name`== country[i]],
                   GDP_africa$`2014`[ GDP_africa$`Country Name`== country[i]] )
  for (j in 1:length(observed_year)){
    year <- c(year, observed_year[j])
  }
}

#combine the 
GDP_co <- country[m]
GDP_africa_2 <- cbind.data.frame(GDP_co, year, country_gdp)
GDP_africa_2 <- rbind.data.frame(GDP_africa_2, North_africa)
names(GDP_africa_2)[1] <- "RECIPIENT"
names(GDP_africa_2)[2] <- "YEAR"
Investment_to_popular <- left_join(Investment_to_popular, GDP_africa_2)


#create a table of demonstrate African countries of their Regime type, GDP, and China's investment 
Regime_type <- read.csv("regime type.csv")
Regime_2 <- subset(Regime_type, Year >= 2000 & Year < 2015 )
names(Regime_2)[2] <- "Country.Code"
names(Regime_2)[4] <- "Regime.Type"
Region <- left_join(Region, Regime_2)
Regime_3 <- cbind.data.frame(Region$TableName, Region$Year, Region$Regime.Type)
names(Regime_3)[1] <- "RECIPIENT"
names(Regime_3)[2] <- "YEAR"
names(Regime_3)[3] <- "Regime_Type"
Investment_to_popular <- left_join(Investment_to_popular, Regime_3)

Investment_to_popular <- subset(Investment_to_popular, is.na(Investment_to_popular$popular_opinion) == FALSE)
Investment_to_popular <- subset(Investment_to_popular, is.na(Investment_to_popular$country_gdp) == FALSE)
Investment_to_popular$investment_by_proportion <- Investment_to_popular$AMOUNT_PER_YEAR / Investment_to_popular$country_gdp



#Regression 
fit <- lm(Investment_to_popular$popular_opinion ~ Investment_to_popular$investment_by_proportion, data=Investment_to_popular)
summary(fit)


#scatter plot
plot(Investment_to_popular$investment_by_proportion, Investment_to_popular$popular_opinion, type ="n", ylim = c(4, 8),
     xlab="China Official Development Assistance by Percentage of Country GDP", ylab="Popular Opinion", xaxt="n",
     main="Africa Popular Opinion by Chinese Investment")
text(Investment_to_popular$investment_by_proportion, Investment_to_popular$popular_opinion, 
     labels = as.character(Investment_to_popular$RECIPIENT), cex=0.5)
axis(1, at=pretty(Investment_to_popular$investment_by_proportion), 
     labels = paste0(pretty(Investment_to_popular$investment_by_proportion)*100, " %"), las= TRUE)
abline(fit)


#multiple regression
fit1 <- lm(Investment_to_popular$popular_opinion ~ Investment_to_popular$investment_by_proportion +
             Investment_to_popular$Regime_Type + Investment_to_popular$Q89E, data=Investment_to_popular)
summary(fit1)
abline(fit1)
