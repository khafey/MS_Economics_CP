#Project 1 
#Kyle Hafey
#10/4/2015

load("~/Desktop/R/ncaaf_2010.rdata")
df

#Loading in variables from the Data frame
#Manipulating some variables
wins=df[["wins"]]
losses=df[["losses"]]
teams=df[["teams"]]
netwl=wins-losses
total=wins+losses
opponents=df[["opponents"]]
df$games=total


#Part 1
  #Solve for vector b (pg 10)
  #Create a vector of 1s 120 times
a=c(rep(1,120))
  #Solve for the b vector
b=(a+(netwl)/2)
b

#Part 2 
  #Solve for the Colley Matrix C(pg 10)
  #Set up the matrix with all 0's
  #Name the Rows and Columns with the team names for organization
ColleyMatrix=matrix(0,nrow=120,ncol=120)
rownames(ColleyMatrix)=df$teams
colnames(ColleyMatrix)=rownames(ColleyMatrix)
ColleyMatrix

#Manipulate the 120 x 120 matrix to place a -1 in each cell that
  #team i played team j from the data frama "opponents" vector
  # this code will also place a nuber two in the corresponding cell 
  # if team i and j played eachother twice in the season
for(i in 1:120){
    play=df$opponents[[i]]
  for(j in play)
    ColleyMatrix[i,j]=ColleyMatrix[i,j]-1
}

#Manipulate the data frame to put two plus the total number of games
  #that each team played where i and i converge on the diagonal
for(i in 1:120){
  for(j in 1:120)
    g=df$games[i]
    ColleyMatrix[i,i]=g+2
}

#Print the colley matrix and inspect for correctness
View(ColleyMatrix)
View(df)

#Part 3
#Solve Cr=b where c=ColleyMatrix, b is the b=b, and r=rank is the
  #ranking. We will use the command solve to find the solution
rank=solve(ColleyMatrix,b)
#Create solution based on the ranking
solution=data.frame(rank,teams)
#Order the solution by decending order to view the highest rankings
solution=solution[order(-solution$rank),]
#View the solution
View(solution)
