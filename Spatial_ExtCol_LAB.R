#CA model of single species extinction-colonization dynamics
#Periodic boundary conditions
##Colonization is density-dependent:
# the probability that an empty cell (x,y) becomes occupied is c*N where N is the number of occupied cells in the neighborhood V of (x,y).
#
# Elise Filotas
# 2019/08/15
##################################################################################

#------------------------------------------------------------------------------
# Add needed packages and function
#------------------------------------------------------------------------------

#packages
library(animation)
library(ggplot2)
library(reshape2)
library(Rmisc)
library(pracma)
library(magick)
# include the function that returns the Moore neighborhood of each cell for periodic boundary cond.
source("Voisins_Moore.R")

#----------------------------------------------------------------------------
# Define parameters
#----------------------------------------------------------------------------

P = 100 #Duration of the simulation
D = 10 #Dimension of the grid is DxD
e = 0.2 # Extinction probability
c = 0.05 # Colonization probability
i = 1 #fraction of initial occupancy
couleur=c("palegoldenrod","seagreen4")


#--------------------------------------------------------------------------
#Define the variables and set their initial conditions
#--------------------------------------------------------------------------
t = 1             #Initialize the iteration

X = matrix(0,D,D) #Landscape, initially empty
p<-runif(D*D) #create DxD random numbers
X[p<=i]<-1   #Populate the landscape


O<-matrix(0,1,P) #Vector containing the total number of occupied cells at each iteration
O[1]<-sum(sum(X))



X_P<- list() #will contain a list of the landscapes at each iteration
X_P[[1]]<-X



#--------------------------------------------------------------------------
#RUN SIMULATION
#--------------------------------------------------------------------------

for(t in 2:P){ #For each iteration
  
  
  
 
}

#----------------------------------------------------------------------------------------
# Create a fonction that converts the landscape into an image
#----------------------------------------------------------------------------------------

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
X_plot <- lapply(X_P, Land_to_plot)

plotname = sprintf("Col-Ext_CA.gif")


saveGIF(lapply(X_plot, print), movie.name=plotname)
