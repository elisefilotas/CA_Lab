# Schelling segregation Model
# Landscape with dimension DxD + Moore neighborhood+ periodic boundary conditions
#
# Elise Filotas 04/03/2013
#########################################################################################

#Create a function that depends on 2 parameters:
# T_R = the tolerance threshold of the red people
# T_G = the tolerance threshold of the green people

Schelling <-function(T_R, T_G){
#T_R<-2
#T_G<-4

#------------------------------------------------------------------------------
# Add needed packages and function
#------------------------------------------------------------------------------
library(animation)
library(ggplot2)
library(reshape2)
library(Rmisc)
library(pracma)
library(magick)

source("Voisins_Moore.R")

#--------------------------------------------------------------------------------
# Define the parameters
#--------------------------------------------------------------------------------
D = 50              #Landscape size
prop_empty = 0.2    #Need a certain amount of empty cell
prop_R = 0.4        #proportion of red people
prop_G = 0.4        #proportion of green people
TT = 30             #length of simulation




#--------------------------------------------------------------------------------
# Initialize the landscape
#--------------------------------------------------------------------------------

#Create a matrix of random numbers [0,1]
V<-runif(D*D)
A<-matrix(V,D,D)


#Create the landscape
M<-matrix(0,D,D)                         
# Distribute red people randomly in the landscape they will have value 1
M[(A>prop_empty)&(A<=prop_R+prop_empty)]<-1 
# Distribute green people randomly in the landscape they will have value 2
M[(A>prop_R+prop_empty)&(A<=1)]<-2   


M_TT<- list() #will contain a list of the landscapes at each iteration
M_TT[[1]]<-M

#-------------------------------------------------------------------------------
# Run simulation
#--------------------------------------------------------------------------------

for(t in 2:TT){       #For each iteration
  print(t)
  
  
  #Create a random sequence in which each cell will be visited
  #this sequence will change at each iteration t 
  order_x <- randperm(1:D)
  order_y <- randperm(1:D)
  
  for(i in 1:D){      #For each cell
    
    x<- order_x[i] #find the x coordonnate of the cell according to the random sequence
    
    for(j in 1:D){
      y<- order_y[j] #find the y coordonnate of the cell according to the random sequence
      
      
      
      # Apply transition rule only to non-empty cells
      if(M[x,y]!=0){
        
        # Find the neighborhood of cell (x,y)
        V = Voisins_Moore(M,x,y)
        
        # Find the number of neighbors that are the same colour as cell (x,y)
        H <- which(V==M[x,y], arr.ind=TRUE)
        Nb<-(dim(H)[1]-1)   #Remove the individual in cell (x,y)
        
        #
        if (M[x,y]==1){ #if the individual is red, use the red tolerance threshold 
        T<-T_R
      }
        if (M[x,y]==2){ #if the individual is green, use the green tolerance threshold 
        T<-T_G
      }
        
        # THe individual in (x,y) is "happy" if Nb is superior or equal to the threshold
        # if it Nb is inferior it moves!
        if(Nb<T){
          # Find the coordinates of empty cells on the landscape
          Empty <- which(M==0, arr.ind=TRUE)
          # Pick a cell at random
          r<-round(runif(1)*dim(Empty)[1])
          x_new <- Empty[r,1]  #coordinates of the new cell
          y_new <- Empty[r,2]
          # Move
          M[x_new,y_new]=M[x,y]  #the new cell takes the value of the treated cell
          M[x,y] = 0             #the treated cell becomes empty
        }
        
        
      }##if
      
    }##for x   
  }##for y

   
   #Put the landscape in the list
   M_TT[[t]]<-M
   
}## for t

#----------------------------------------------------------------------------------------
# Create a fonction that converts the landscape into an image
#----------------------------------------------------------------------------------------
couleur<-c( "white",  "red", "Green")
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
M_plot <- lapply(M_TT, Land_to_plot)

plotname = sprintf("Schelling_%d_%d.gif",T_R,T_G)


saveGIF(lapply(M_plot, print), movie.name=plotname)

}