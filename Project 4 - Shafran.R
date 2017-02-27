#Kyle Hafey
#Project 4 - Shafran
#12/5/2015


#Shafrans given function
calcindex <- function(x1, y1, x2, y2){
  
  n1 = length(x1)
  n2 = length(x2)
  
  q1e=sum(as.integer((x1<0.5)&(y1<0.5)))
  q2e=sum(as.integer((x1>=0.5)&(y1<0.5)))
  q3e=sum(as.integer((x1<0.5)&(y1>=0.5)))
  q4e=sum(as.integer((x1>=0.5)&(y1>=0.5)))
  
  q1o=sum(as.integer((x2<0.5)&(y2<0.5)))
  q2o=sum(as.integer((x2>=0.5)&(y2<0.5)))
  q3o=sum(as.integer((x2<0.5)&(y2>=0.5)))
  q4o=sum(as.integer((x2>=0.5)&(y2>=0.5)))
  
  r = 0.5*(abs(q1e/n1-q1o/n2)+abs(q2e/n1-q2o/n2)+abs(q3e/n1-q3o/n2)+abs(q4e/n1-q4o/n2))
  
  return(r)
  
}




###########Create Schelling function##############
Schelling = function(N, f, t, s){
  
  #use loop to itterate the number of s simulations the function will do
  itteration = matrix(0, nrow = 2, ncol = s)
  for(k in 1:s){
    
  #Assign location coordinates
  x = runif(N)
  y = runif(N)
  #Assign type to people
  type = runif(N)
  for(i in 1:length(type)){
    if(type[[i]] <= f){
      type[[i]] = 1
    }
    else
      type[[i]] = 0
  }
  #summary(type)
  ##This is not perfect, but as N gets larger this will approach the true percentage of type 1s.
  #Set the original data frame: coordinates and type
  dfconfig = data.frame("Xcoordinates" = x, "Ycoordinates" = y, "Type" = type)
  #Create a while loop to do simulation
  moves = TRUE
  counter = 0
  while(moves){
    counter = counter + 1
    
    #Getting the initial discrimination index of the people if we are in period 1(counter = 1)
    if (counter == 1){
      #Set x1,y1,x2,y2
      x1 = c(dfconfig[which(dfconfig[3] == 1),1])
      y1 = c(dfconfig[which(dfconfig[3] == 1),2])
      x2 = c(dfconfig[which(dfconfig[3] == 0),1])
      y2 = c(dfconfig[which(dfconfig[3] == 0),2])
    }
      #Calculate the initial index
    initialindex = calcindex(x1, y1, x2, y2)
    
    #Create a distance matrix that has the full locations
    fulldistance = data.frame("test" = numeric(length(N)))
    for(i in 1:length(dfconfig$Ycoordinates)){
      distance = c()
      for(j in 1:length(dfconfig$Ycoordinates)){
        d = ((dfconfig[j,1] - dfconfig[i,1])^2 + (dfconfig[j,2] - dfconfig[i,2])^2)^.5
        distance = c(distance, d)
      }
      fulldistance = cbind(fulldistance, distance)
    }
    #clean it up
    fulldistance = fulldistance[2:(N + 1)]
    colnames(fulldistance) = c(1:N)
  
  #calculate who lives near the initial people - I will call this the neighborhoods
    neighborhoods = matrix(0, nrow = N, ncol = N)
    for(i in 1:N){
    neighborhoods[,i] = sort(fulldistance[,i], decreasing = FALSE)
    }
    #Now the challenge is to relate location to the type of person because its different tables
    #Now, I want to relate what type the neighbors are in the groups
    relation = data.frame()
    for (i in 1:N){
      for (j in 1:N){
       relation[i,j] = which(neighborhoods[,j] == fulldistance[i,j])
      }
    }
    #Now, relate these type locations to the ten closest neighbors
    neighborsrelation = data.frame()
    for (i in 1:11){
      for (j in 1:N){
        neighborsrelation[i,j] = which(relation[,j] == i)
      }
    }
   
     #Determine and display what type the neighbors are...
    TYPE = data.frame()
    for(i in 1:11){
      for(j in 1:N){
        TYPE[i,j] = dfconfig$Type[neighborsrelation[i,j]]
      }
    }
  
    #Compare the percentages of same type people in the neighborhood 
    #The first row will be the individuals type, the second will be the percent like that person
    #If its above the threshold they will not move
    TYPE = as.matrix(TYPE)
    TYPEcounter = matrix(0, nrow = 2, ncol = N)
    TYPEcounter[1,] = TYPE[1,]
    for(i in 1:N){
      if(TYPEcounter[1,i] == 1){
        TYPEcounter[2,i] = (sum(TYPE[2:11,i])/10)
      }else{
        TYPEcounter[2,i] = ((10 - sum(TYPE[2:11,i]))/10)
      }  
    }
    TYPEcounter = as.data.frame(TYPEcounter)

    
    #Check who needs to move - either stop (FALSE) or move them(TRUE)
    #TRUE will change their location and send them through the loop again 
    moves = FALSE
    for (i in 1:N){
      if (TYPEcounter[2,i] >= t){
        dfconfig$Xcoordinates[i] = dfconfig$Xcoordinates[i]
        dfconfig$Ycoordinates[i] = dfconfig$Ycoordinates[i]
      }
      else{
        moves = TRUE
        dfconfig$Xcoordinates[i] = runif(1)
        dfconfig$Ycoordinates[i] = runif(1)
      }
    }
  } 
    
  #Calculate the ending index
  x11 = c(dfconfig[which(dfconfig[3] == 1),1])
  y11 = c(dfconfig[which(dfconfig[3] == 1),2])
  x21 = c(dfconfig[which(dfconfig[3] == 0),1])
  y21 = c(dfconfig[which(dfconfig[3] == 0),2])
  
  #Calculate the final discrimination index
  finalindex = calcindex(x11, y11, x21, y21)
  
  #take the mean of the itterations
  itteration[1,k] = initialindex
  itteration[2,k] = finalindex
  meaninitial = mean(itteration[1,])
  meanfinal = mean(itteration[2,])
  }
  #Combine the means in a vector to display as the results
  SOLUTION = c(meaninitial, meanfinal)
  return(SOLUTION)
  

}

Schelling(50, .5, .5, 100)

