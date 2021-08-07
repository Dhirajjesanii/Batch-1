
#Q1 import dataset and give the column name with perticuler txt file.

decathlon = read.csv("decathlon.csv",header = F)
colnames(decathlon) = c("Athlets",	
                        "100m",	
                        "Long.jump",	
                        "Shot.put",	
                        "High.jump",	
                        "400m",	
                        "110m.hurdle",	
                        "Discus",	
                        "Pole.vault",	
                        "Javeline",	
                        "1500m",	
                        "Rank",	
                        "Points",	
                        "Competition")


#Q2 Matrix and scaling

mat = matrix(1:25,ncol = 5)
scal = scale(mat,center = T)

#Q3 Apply  k-means algorithm on numerical caolumn

#(removing non-numerical columns)
remove = decathlon[-c(1,14)]
kmen = kmeans(remove,centers = 5)
kmen$cluster

#Q4 create function to print cluster which created as above question.

print_clus = function(x)
{
    view = print(kmen)  
}
print_clus(kmen$cluster)


#Q5 Print cluster boot

library(fpc)
best.p <- 5
cboot <- clusterboot(kmen$cluster, clustermethod=kmeansCBI,
                     runs = 100,iter.max = 100,
                     krange = best.p, seed = 15555)
