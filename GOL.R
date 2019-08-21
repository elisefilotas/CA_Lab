# Game of Life
# Landscape with dimension DxD + Moore neighborhood+ periodic boundary conditions
#
# Elise Filotas 04/03/2013
#########################################################################################


#------------------------------------------------------------------------------
# Add needed packages and function
#------------------------------------------------------------------------------
library("animation")
library('ggplot2')
library(reshape2)
library(Rmisc)

source("Voisins_Moore.R")

#----------------------------------------------------------------------------
# Define parameters
#----------------------------------------------------------------------------
D = 50       #Dimension of the landscape is DxD
TT = 30      #Simulation length


#--------------------------------------------------------------------------
#Define the variables and set their initial conditions
#--------------------------------------------------------------------------
Land_TT<- list() #will contain a list of the landscapes at each iteration
k = 1

# Random Landscape
a<-runif(D*D)
A<-matrix(a,D,D)
Land <-matrix(0,D,D)
Land[A>0.5]<-1    #Chose initial density of live cells

# Create a periodic pattern (Beacon) - period=2
# filename<-("Beacon.gif")
# Land<-matrix(0,D,D)
# Land[23:24,23:24]<-1
# Land[25:26,25:26]<-1

# Create a periodic pattern (Pulsar) - periode=3
# filename<-("Pulsar.gif")
# Land<-matrix(0,D,D)
# Land[21,23:25]<-1
# Land[26,23:25]<-1
# Land[28,23:25]<-1
# Land[33,23:25]<-1
# Land[21,29:31]<-1
# Land[26,29:31]<-1
# Land[28,29:31]<-1
# Land[33,29:31]<-1
# Land[23:25,21]<-1
# Land[23:25,26]<-1
# Land[23:25,28]<-1
# Land[23:25,33]<-1
# Land[29:31,21]<-1
# Land[29:31,26]<-1
# Land[29:31,28]<-1
# Land[29:31,33]<-1

# Create a glider 
# filename<-("glider.gif")
# Land<-matrix(0,D,D)
# Land[1,2]<-1
# Land[2,3]<-1
# Land[3,1:3]<-1

# Load the "gun"
# filename<-("gun.gif")
# Land<-matrix(0,D,D)
# gun<- read.table("gun.txt")
# for(x in 1:12){
#   for(y in 1:37){
#     Land[x+5,y+5]<-gun[x,y]
#   }
# }
# Land<-t(Land)


#--------------------------------------------------------------------------
#RUN SIMULATION
#--------------------------------------------------------------------------
Land_TT[[1]]<-Land

for(t in 2:TT){
  
  #Create a temporary matrix in which to store the updated landscape
  Land_temp<-Land
  
  for(x in 1:D){        #for each iteration
    for(y in 1:D){
      
      
      
      #Find the neighborhood of (x,y)
      v = Voisins_Moore(Land,x,y)
      
      #find the live neighbors
      v_live <- which(v==1, arr.ind=TRUE)
      
      #how many?
      n_live <- dim(v_live)[1]
      
      #Transition rules
      
      #Default transition is that the cell dies
      Land_temp[x,y]<-0
      
      
      if(Land[x,y]==0){#if the cell is dead
        
                if(n_live == 3){#if there are 3 live neighbors
                Land_temp[x,y]<-1  #the dead cell becomes alive
                }#if 0
      }#if 0
      
      
      if(Land[x,y]==1){#if the cell is alive
            
               n_live <- n_live-1 #remove the cell itself from the number of live neighbors
        
               if((n_live==2)|(n_live==3)){ #if the number of live neighbors is 2 or 3
               Land_temp[x,y]<-1 #The cell stays alive
               }#if 2 ou 3
      
      }#if 1
        
      
    } #for y       
} #for x

#update the landscape
Land<-Land_temp

#Put the landscape in the list
Land_TT[[t]]<-Land

}#for t




# Converts the current grid (matrix) to a ggplot2 image
# This part is taken from : http://johnramey.net/blog/2011/06/05/conways-game-of-life-in-r-with-ggplot2-and-animation/
Land_to_plot <- function(grid) {
  grid <- melt(grid)   #reshape into single column
  grid$value <- factor(ifelse(grid$value, "Alive", "Dead"))
  p <- ggplot(grid, aes(x=Var1, y=Var2, z = value, color = value))
  p <- p + geom_tile(aes(fill = value))
  p <- p+ theme(legend.position = "none", aspect.ratio=1, axis.text.x=element_blank(),
                       axis.ticks.x=element_blank(),axis.text.y=element_blank(),
                        axis.ticks.y=element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank())
  p<-p + scale_x_discrete(expand=c(0,0))+ scale_y_discrete(expand=c(0,0))
  p  + scale_fill_manual(values = c("Dead" = "white", "Alive" = "black"))
}





Land_plot <- lapply(Land_TT, Land_to_plot)
saveGIF(lapply(Land_plot, print),movie.name=filename)





