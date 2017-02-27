#Kyle Hafey
#Project 3 - Shafran
#Code for function


load("/Users/kylehafey/Desktop/Project3 /Project3.Part1.rdata")
View(dfhope)


#Name global variables
year = data.frame()



#Create function by using project 1
Colley = function(S) {
  for(i in 1:nrow(dfhope)){
    if(dfhope[i,1] == S){
      year = rbind(year,dfhope[i,])
    }
  }
  
  a = c(rep(1,length(year$Teams)))
  b = (a + (year$Wins - year$Losses)/2)
  b
  
  ColleyMatrix = matrix(0, nrow = length(year$Teams), ncol = length(year$Teams))
  rownames(ColleyMatrix) = year$Teams
  colnames(ColleyMatrix) = rownames(ColleyMatrix)
 
  for(i in 1:length(year$Teams)){
    play = year$Opponents[[i]]
    for(j in play)
      ColleyMatrix[i,j]=ColleyMatrix[i,j]-1
  }
  
  for(i in 1:length(year$Teams)){
    for(j in 1:length(year$Teams))
      g = year$Wins[i] + year$Losses[i]
    ColleyMatrix[i,i] = g + 2
  }
  
  
  rank = solve(ColleyMatrix, b)
  #Create solution based on the ranking
  solution = data.frame(rank,year$Teams)
  #Order the solution by decending order to view the highest rankings
  solution = solution[order(-solution$rank),]
  #View the solution
  View(solution)
  
    
}


Colley(2010)
