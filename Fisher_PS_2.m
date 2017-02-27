%Fisher Problem Set 2
%Kyle Hafey
%Cole Estrade
%Gloria

%We wrote the code this way so you can run the code and the answers will be
%displayed in the command window. Let us know if you would like us to do
%something different for the next problem sets. 


%Question 1 - Working With Eigenvectors
%Question 1.1 - Distinct Real Eigenvectors

%Code in matrix
A = [1 2 ; 0 3];
%Calculate eigenvalues
eigs(A);
%The corresponding eigenvectors would be (1,0) and (1,1)
%We did this work by hand. 
%Product of eigenvalues is the determinant of matrix
det(A);
check = 3*1;
%Sum of eigenvalues is the trace of matrix
trace(A);
check1 = 3 + 1;

%V is the 2x2 matrix whose columns are the eigenvectors
V = [1 1 ;0 1];
%L stands for lambda which is the diagonal matrix whose elements are the
%corresponding eigenvalues.
L = [1 0 ; 0 3];
%Show that A = V*L*inv(V)
check2 = V*L*inv(V);
%Write A^100
A100 = A^100;

%In order for the system to be bounded in the long run, we must set 
%the initial conditions. 
%V^-1*(c1;c2) = (*,0)
%c2 = 0, c1 can be anything. 
%Initial condition is required for the equilibrium to be on a stable
%saddle path. If not, the system will explode and not be bounded. 


%Question 1.2 - Complex Eigenvectors

%Code the new matrix
A = [1 -1 ; 1 1];
%Code the new eigenvalues - because they contain i, they are complex
%conjugates
E = eigs(A);
%We also computed the eigenvectors. they are (i, 1) and (-i, 1)
%Show that the product of the eigenvalues is the determinant of the matrix
det(A);
check3 = (1 + 1i)*(1 - 1i);
%Show that the sum of the eigenvalues is the determinant of the matrix
trace(A);
check4 = (1 + 1i) + (1-1i);

%The immaginary system exhbits cyclical behavior. We will look at various
%moments to try to lock down the period of its cycle and observe its
%behavior. 
A;
A^2;
A^3;
A^4;
A^5;
A^6;
A^7;
A^8;
A^9;
A^10;
A^50;
A^75;
A^100;
%The period of the cycle is 8
%It appears as though the sin wave will start small and then get larger
%iterations as time goes on. The peak to trough difference will increase
%as the iterations  continue. 

%Here, we will compare the second matrix to the new matrix that utilizes
%properties of trigonometry. 
A = [1 -1; 1 1]
Art = [' rcost' '-rsint' ; ' rsint' ' rcost']
%In order for rcost and rsint to be equal, cost must equal sint. According
%to properties of trigonometry theta will equal pi/4. 
%Now we will solve for theta and for r. 
Theta = pi/4
r = 2/sqrt(2)
%this matrix will take in a vector and rotate it counterclockwise and
%stretch it by r. 

%Now we want to code a matrix that will have a period of 4 and will 
%shrink any vector in R2 halfway to (0,0)in 4 periods. We will use the same
%structure as the example. We define theta as pi/2. 
EX = ['0r    ' '-1r   ' ; '1r    ' '0r    ']
%Now we will solve for r. we found r = (.5)^(.25)
r = .5^(.25)
EX = [ 0 -.8409 ; .8409 0]
%This matrix will look like a spiral that after one full period of 2pi will
%shink in half. 


%Question 2 - Markov Matrices
%Question 2.1 - Computing the Probabilities of Histories
prob = (.6)*(.1)*(.5)*(.1)*(.5)
%If t = 2 periods there are a few options:
%Start 2, end 2
prob1 = .6*.9
%start2, end 5
prob2 = .6*.1
%start 5, end 5
prob3 = .4*.5
%start 5, end 2
prob4 = .4*.5
%The system has a 46%(.2 + .2 + .06) chance of containing a 5. and the 
%sytesm has a 40% chance of returning a 5 in the first stage. 
%So, the probability we are looking for would be .4/.46
PROB = .4/.46

%Question 2.2 - Long-Run Probabilities

%Code in the probability 
pr = [.9 .1 ; .5 .5]
pr^100
prinitial = [.6 .4]
longrunprob = prinitial*pr^100
%The book defines invariant as a case where yt = y0. In an invariant
%case, where you start determines your payoff. This case is not considered
%invariant because you are free to move from case 2 to case 1 and vice
%versa and the payouts are different for each stage.  

%For this matrix example it is impossible to make your initial period
%placement define what the payout is. In order to make this matrix
%invariant the payouts will have to be equal. They can be any combinations
%of equal values.  
payout = [1 ; 1]

%Question 2.3 - Inferring Transition Probabilities

%P is unknown
%Period 1's estimated value is the mean. Period 2's estiamted value would
%be the variance. With this bit of information we can determine the payoffs
%in any period. Moment 1's payoff is (2 , 5). For moment 2's payoff, we
%square the payoffs in moment 1 and get (4 , 25). 
%P*M = H
H = [3.5 14.5 ; 4.1 18.7]
M = [2 4 ; 5 25]
P = H*inv(M)


