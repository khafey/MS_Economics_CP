#Kyle Hafey 
#Project 3 - Shafran
#Code for data import/cleaning

#Part 1 - Data Collection 
#Start creating loop to import all the data
complete = data.frame()
newdata = data.frame()
cleandata = data.frame()
dffinal = data.frame()
dffinalcomplete = data.frame()
URLYear = 1960
while(URLYear <= 2010){
  newdata = read.fwf(
    file = url(paste('http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf', URLYear, 'gms.txt', sep = '')),   
    widths = c(11,28,3,28,3))
  newdata$Year = URLYear
#Delete Spaces
  newdata[,2] = gsub(" ", "", newdata[,2])
  newdata[,4] = gsub(" ", "", newdata[,4])
  colnames(newdata) = c("Date", "Home", "HomeScore","Away","AwayScore", "Season")
#Drop Ties
  cleandata = data.frame(HomeTeam = as.character(newdata$Home), HomeScore = as.numeric(newdata$HomeScore), AwayTeam = as.character(newdata$Away), AwayScore = as.numeric(newdata$AwayScore), Season = as.character(newdata$Season))
  cleandata = cleandata[!cleandata$HomeScore == cleandata$AwayScore,]
#Make the loop combine data
  complete = rbind(complete,cleandata)
#add one to the counter
  URLYear = URLYear + 1
}
complete$HomeTeam = as.character(complete$HomeTeam)
complete$AwayTeam = as.character(complete$AwayTeam)
complete$Season = as.character(complete$Season)
#View(complete)
#complete has scores for each game without ties and name spaces
#Now, I need to drip division II teams, or teams that played less than 6 games



#Create code to drop Div II Teams
#Start with empty global dataframes
year = complete[-(1:nrow(complete)),]
d1year = complete[-(1:nrow(complete)),]
d1total= complete[-(1:nrow(complete)),]
#Do loop for each year
for(y in 1960:2010){
  #make dataframe for any given year 
  for(i in 1:nrow(complete)){
    if(complete[i,5]==y){
     year=rbind(year,complete[i,])
    }
  }
  #This sorts the data into individual data frames by year/ not a list
  #only include teams that play more than 6 games in cleandata
  for(x in 1:nrow(year)){
    counter1 = length(c(which(year[x,1] == year$HomeTeam), which(year[x,1] == year$AwayTeam)))
    counter2 = length(c(which(year[x,3] == year$HomeTeam), which(year[x,3] == year$AwayTeam)))
    if(counter1 > 6 & counter2 > 6) 
      d1year = rbind(d1year,year[x,])
  }
#combine d1year and d1total and save d1total. 
  d1total = rbind(d1total,d1year)
#Reset variables
  year = complete[-(1:nrow(complete)),]
  d1year = complete[-(1:nrow(complete)),]
  counter1 = c()
  counter2 = c()
#save as cleandata
  cleandata = d1total
}
#cleandata will be all the Division I games that did not end in a tie
View(cleandata)


#Now that I have a clean dataset, I need to start programming the wins, 
#losses, and opponents vector


#create global variables
yearsplit = data.frame()
dfhopeful = data.frame()
dfhope = data.frame()
teams = c()
opponents = c()
wins = c()
losses = c()
allwins = c()
alllosses = c()
allopponents = c()

#Try using loop to split data and then manipulate it
for(y in 1960:2010){
  #make dataframe for any given year 
  for(i in 1:nrow(cleandata)){
    if(cleandata[i,5] == y){
      yearsplit = rbind(yearsplit, cleandata[i,])
      yearsplit$HomeTeam = as.character(yearsplit$HomeTeam)
      yearsplit$AwayTeam = as.character(yearsplit$AwayTeam)
      teams = sort(unique(c(yearsplit$HomeTeam, yearsplit$AwayTeam)))
      teams = as.character(teams)
    }
  }
#Now create final data frame for every year
  for(j in 1:length(teams)){
    opponents = c()
    wins = c()
    losses = c()
    for(k in 1:nrow(yearsplit)){
     if((teams[j] == yearsplit$HomeTeam[k]) & (yearsplit$HomeScore[k] > yearsplit$AwayScore[k]))
       wins = c(wins, 1)
     if((teams[j] == yearsplit$AwayTeam[k]) & (yearsplit$AwayScore[k] > yearsplit$HomeScore[k]))
        wins = c(wins, 1)
     if((teams[j] == yearsplit$HomeTeam[k]) & (yearsplit$HomeScore[k] < yearsplit$AwayScore[k]))
        losses = c(losses, 1)
     if((teams[j] == yearsplit$AwayTeam[k]) & (yearsplit$AwayScore[k] < yearsplit$HomeScore[k]))
       losses = c(losses, 1)
      if(teams[j] == yearsplit$HomeTeam[k])
        opponents = c(opponents, which(teams == yearsplit$AwayTeam[k]))
      if(teams[j] == yearsplit$AwayTeam[k])
        opponents = c(opponents, which(teams == yearsplit$HomeTeam[k]))
    }
    allwins[j] = length(wins)
    alllosses[j] = length(losses)
    allopponents[[j]] = opponents
    season = c(rep(y, length(teams)))
    opponents = c(rep(0, length(teams)))
  }
  #Save data as dfhope, because I hope this actually works...
  dfhopeful = data.frame(Season = season, Teams = teams, Wins = allwins, Losses = alllosses, Opponents = opponents)
  dfhopeful$Opponents = allopponents
  dfhope = rbind(dfhope, dfhopeful)
  #Reset variables
  dfhopeful = data.frame()
  yearsplit = data.frame()
  teams = c()
  allwins = c()
  alllosses = c()
  season = c()
  allopponents = c()
  opponents = c()
}
#View output...
View(dfhope) 
#Dfhope is good to go


#Save data
#I could not save the data as .xls, or .dat because of the opponents vector
#I am saving it as .rdata
#save(dfhope, file = '~/Desktop/Project3.Part1.rdata')


