#Kyle Hafey
#Shafrn - Project 2
#10/14/2015

load("~/Desktop/R/ncaaf_2010.rdata")

#Set up variables
teams = df$teams
wins = df$wins
losses = df$losses
df$total = df$wins + df$losses
total = df$total
df$netwins= df$wins-df$losses
netwins=df$netwins
opponents = df$opponents


#create global variables for the loop
r = c(rep(0,length(df$teams)))
rnew = c(rep(0,length(df$teams)))
effectivewins = c(rep(0,length(df$teams)))
effectivewinsnew = c(rep(0,length(df$teams)))
#View(df)

#start looping
#Create unadjusted ranking 
for (i in 1:120){
  r[i] = (1 + wins[i])/(2 + total[i])
}

#start creating an adjusted effective wins and an adjusted ranking
repeat{
  r = rnew
  for(i in 1:120){
    effectivewins[i] = ((wins[i] - losses[i])/ 2) + sum(r[opponents[[i]]])
    rnew[i] = (1 + effectivewins[i])/(2 + wins[i] + losses[i])
    effectivewinsnew[i] = ((wins[i] -losses[i])/2) + sum(rnew[opponents[[i]]])
  }
  if(max(rnew - r) < .000000001){
    break
  }
}

ranking = rnew
# order and Print the solution
Solution = data.frame(teams, ranking)
Solution = Solution[order(-Solution$ranking),]
View(Solution)
