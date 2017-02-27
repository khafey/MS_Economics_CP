#Kyle Hafey
#Project 5 Code
#12/9/2015

data = read.csv("~/Desktop/R/pddata.csv")
#View(data)

game = matrix(c(.105, .175, .005, .075), ncol = 2, byrow = FALSE)
rownames(game) = c("A", "B")
colnames(game) = c("A", "B")
#View(game)

#Create formula following the outline on the powerpoint slides
#Initial function
ll_rl = function(h){
Aa = c()
Ab = c()
h = h[1]
d = 1 
for(i in 1:length(data$choice)){
  if(data$round[i] == 1){
    Aa[i] = 0
    Ab[i] = 0
  }else{
    Aa[i] = d * Aa[i - 1] + (1 - data$choice[i - 1]) * data$payoff[i - 1]
    Ab[i] = d * Ab[i - 1] + data$choice[i - 1] * data$payoff[i - 1]
  }
}

Pa = exp(h*Aa) / (exp(h*Aa) + exp(h*Ab))
Pb = (1 - Pa)

A = ifelse(data$choice == 0, 1, 0)
B = ifelse(data$choice == 1, 1, 0)

liklihood = h*A*Aa + h*B*Ab - log(exp(h*Aa) + exp(h*Ab))

return(-sum(liklihood))
}

#use the log liklihood function and the parameter estimate to max the parameter. 
#Test the base function
#result = .8
#resultll = nlminb(result, ll_rl)
#resultll = resultll$par


###################### Question 1 #########################
#Simulation sample function
ll_sample = function(h){
  Aa = c()
  Ab = c()
  h = h[1]
  d = 1 #in next part d = h[2]
  for(i in 1:length(studentpopulation$choice)){
    if(studentpopulation$round[i] == 1){
      Aa[i] = 0
      Ab[i] = 0
    }else{
      Aa[i] = d * Aa[i - 1] + (1 - studentpopulation$choice[i - 1]) * studentpopulation$payoff[i - 1]
      Ab[i] = d * Ab[i - 1] + studentpopulation$choice[i - 1] * studentpopulation$payoff[i - 1]
    }
  }
  Pa = exp(h*Aa) / (exp(h*Aa) + exp(h*Ab))
  Pb = (1 - Pa)
  
  A = ifelse(studentpopulation$choice == 0, 1, 0)
  B = ifelse(studentpopulation$choice == 1, 1, 0)
  
  liklihood = h*A*Aa + h*B*Ab - log(exp(h*Aa) + exp(h*Ab))
  
  return(-sum(liklihood))
}


#Global Variables
studentpopulation = data.frame()
Question1 = c()
sample1 = c()
result = .8
N = 5
#Collect data and run loop
for(j in 1:N){
#start sampling - take a sample of 20 ids
    sample1 = sample(1:20, replace = T)
#View(sample)
    for(k in 1:20){
      student = data[which(sample1[k] == data$id),]
      studentpopulation = rbind(studentpopulation, student)
      }
  result1 = nlminb(result, ll_sample)
  result1 = result1$par
  Question1 = c(Question1, result1) 
  studentpopulation = data.frame()
  sample1 = c()
}

seeror1 = sd(Question1)



################## Question 2 #####################

#Simulation function
#P will be a vector of two elements
ll_sample2 = function(p){
  Aa = c()
  Ab = c()
  for(i in 1:length(studentpopulation2$choice)){
    if(studentpopulation2$round[i] == 1){
      Aa[i] = 0
      Ab[i] = 0
    }else{
      Aa[i] = p[1] * Aa[i - 1] + (1 - studentpopulation2$choice[i - 1]) * studentpopulation2$payoff[i - 1]
      Ab[i] = p[1] * Ab[i - 1] + studentpopulation2$choice[i - 1] * studentpopulation2$payoff[i - 1]
    }
  }
  Pa = exp(p[2]*Aa) / (exp(p[2]*Aa) + exp(p[2]*Ab))
  Pb = (1 - Pa)
  
  A = ifelse(studentpopulation2$choice == 0, 1, 0)
  B = ifelse(studentpopulation2$choice == 1, 1, 0)
  
  liklihood = p[2]*A*Aa + p[2]*B*Ab - log(exp(p[2]*Aa) + exp(p[2]*Ab))
  
  return(-sum(liklihood))
}


set.seed(100)
#Global Variables
studentpopulation2 = data.frame()
Question2 = data.frame()
sample2 = c()
startv = c(.8,.9)
N = 5
#Collect data and run loop
for(j in 1:N){
  #start sampling - take a sample of 20 ids
  sample2 = sample(1:20, replace = T)
  #View(sample)
  for(k in 1:20){
    student2 = data[which(sample2[k] == data$id),]
    studentpopulation2 = rbind(studentpopulation2, student2)
  }
  result2 = nlminb(startv, ll_sample2)
  result2 = result2$par
  Question2 = rbind(Question2, result2) 
  studentpopulatio2 = data.frame()
  sample2 = c()
}

colnames(Question2) = c("D" , "H")
View(Question2)
#display Standard Errors
seeror2H = sd(Question2[,2])
seerror2D = sd(Question2[,1])








#This takes a while to run, but the code is super accurate and works perfectly







