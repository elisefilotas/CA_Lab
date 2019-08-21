# Forest fire model of Drossel et Schawbl (1992) 
#
# Landscape with periodic boundary conditions
# Moore Neighborhood
#
# 3 possible states for each cell
#  1: the cell is occupied (by one tree)
#  2: the cell is on fire (burning tree)
#  0: empty cell (burned tree)
#
# Rules
# We visit each cell of the landscape (in random order)
# 1) if a cell is on fire it becomes empty (if 2 then 0)
# 2) if the cell is occupied (1)
#    a) it burns with probability 1 if at least one of its neighbor is on fire (2)
#    b) it burns with probability f if none of its neighbors are on fire (2)
# 3) if the cell is empty (0), a tree grows with probability p (1)
##########################################################################

#------------------------------------------------------------------------------
# Add needed packages and function
#------------------------------------------------------------------------------

library(pracma)
library(ggplot2)
library(Rmisc)
library(reshape2)

# include the function that returns the Moore neighborhood of each cell for periodic boundary cond.
source("Voisins_Moore.R")

#----------------------------------------------------------------------------
# Define parameters
#----------------------------------------------------------------------------
D = 50       #Landscape size is DxD
TT = 500     #Simulation length
p = 0.08     #growth probability 0.00005 0.05
f = 0.0005   #probability of spontaneous fire

#--------------------------------------------------------------------------
#Define the variables and set their initial conditions
#--------------------------------------------------------------------------
a<-runif(D*D)                 #D*D random numbers
A<-matrix(a,D,D)              #Matrix of random numbers
Land <-matrix(0,D,D)          #Create empty landscape
Land[A<0.7]<-1                #Set the proportion of occupied cell to 0.7 
Land[(A>=0.7)&(A<0.75)]<-2     #Set the proporition of burning cell to 0.05

# Create a vector that will count the number of occupied cells at each iteration
N_occ<-matrix(0, TT, 1)

plots<-list() #will contain the image of the landscape at each iteration


#Conserve the image as one element of a list
Land_TT<- list() #will contain a list of the landscapes at each iteration
Land_TT[[1]]<-Land



#--------------------------------------------------------------------------
#RUN SIMULATION
#--------------------------------------------------------------------------



for(t in 1:TT){ #for each iteration
  print(t)
  
  #Create a temporary matrix in which to store the updated landscape
  Land_temp<-Land
  
  
  #Create a random sequence in which each cell will be visited
  #this sequence will change at each iteration t 
  order_x <- randperm(1:D)
  order_y <- randperm(1:D)
  
  for(i in 1:D){  #Loop over x
    x<- order_x[i] #find the x coordonnate of the cell according to the random sequence
    
      for(j in 1:D){ #Loop over y
        y<- order_y[j] #find the y coordonnate of the cell according to the random sequence
      
        # RULE 1: if a cell is on fire it becomes empty (if 2 then 0)
        
        HERE ADD LINES
        
        # RULE 2: if the cell is occupied (1)
        #    a) it burns with probability 1 if at least one of its neighbor is on fire (2)
        #    b) it burns with probability f if none of its neighbors are on fire (2)
        if(Land[x,y]==1){
          
          #Find the neighborhood of (x,y)
          v<-Voisins_Moore(Land,x,y)
          #Determine if at least 1 neighbhors is on  fire
          HERE ADD LINES
          
          else{         #if none are on fire
            HERE ADD LINES
          }
          
        }
          
        #RULE 3: if the cell is empty, grow a tree with proba p
        if(Land[x,y]==0){
          if(runif(1)<=p){
           HERE ADD LINES
          } 
          
        }
        
       
    }#for y
  }# for x
  
  #Update the landscape
  Land<-Land_temp
  #Put the landscape in the list
  Land_TT[[t]]<-Land
  
  #Find the number of occupied cells
  N_occ[t]<-sum(Land[Land==1])
  
  
  
} #for t


#----------------------------------------------------------------------------------------
# Create a fonction that converts the landscape into an image
#----------------------------------------------------------------------------------------
couleur=c("palegoldenrod","seagreen4", "red") #colors to illustrate the landscape

# Converts the current grid (matrix) to a ggplot2 image
# This part is taken from : http://johnramey.net/blog/2011/06/05/conways-game-of-life-in-r-with-ggplot2-and-animation/
Land_to_plot <- function(grid) {
  grid <- melt(grid)   #reshape into single column
  p <- ggplot(grid, aes(x=Var1, y=Var2, fill = value))
  p <- p + geom_tile()+scale_fill_gradientn(colours = couleur) 
  p <- p+ theme(legend.position = "none", aspect.ratio=1, axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),axis.text.y=element_blank(),
                axis.ticks.y=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
  p<-p + scale_x_discrete(expand=c(0,0))+ scale_y_discrete(expand=c(0,0))
}

#-----------------------------------------------------------------------------------------
# Apply the function to each landscape of the list
#-----------------------------------------------------------------------------------------
Land_plot <- lapply(Land_TT, Land_to_plot)

plotname = sprintf("Fire.gif")


saveGIF(lapply(Land_plot, print), movie.name=plotname)

# #Plot the time dynamics of occupied cells
# x11()
# plot(1:TT,N_occ/(D*D), pch = 20,col = "seagreen4", xlab='Iterations', ylab='Proportion of occupied patches',  ylim=c(0,0.6), xlim=c(0,TT))



