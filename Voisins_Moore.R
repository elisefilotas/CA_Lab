# Finds the state of the cells in the Moore neighborhood of (x,y)
# in the landscape A with periodic boundary conditions
# 
# Elise Filotas 02/03/2013
###########################################################################

Voisins_Moore<-function(A,x,y){
  
# Find the dimension of the matrix A
  m<-dim(A)[1];      
  n<-dim(A)[2];

# Create an empty vector
  v1<-c()                  

# Find the neighbhors
# Note %% is the modulo function : a%%b = the rest following the division b/a
# Example: if (x,y)=(1,1) and m=n=5  
# then for i=-1 and j=-1, we have:
# ((x-1+i)%%m)+1 = ((1-1-1)%%5)+1 = (-1%%5)+1=4+1=5
  
  for(i in -1:1)
    for(j in -1:1)
    {
      cx<-((x-1+i)%%m)+1
      cy<-((y-1+j)%%n)+1
      v1<- c(v1, A[cx,cy]);
    }

#Put the vector into matrix format
  v2<-t(matrix(v1,3,3))
  
  return(v2)
}

