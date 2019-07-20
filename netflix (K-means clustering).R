movie_metadata <- read.csv("./datafiles/movie_metadata.csv");
dim(movie_metadata);
movie <- data.matrix(movie_metadata)

#removing null values
movie <- na.omit(movie)

#se;ecting sample 500 values
smple <- movie[sample(nrow(movie),500),]

smple_sort <-smple[,c(9,23)]

smple_matrix <-data.matrix(smple_sort)

#elbow curve 
wss <- (nrow(smple_matrix-1)*sum(apply(smple_matrix, 2, var)))
for (i in 2:15) wss[i] <-sum (kmeans(smple_matrix,centers = i)$withinss)
plot (1:15 ,wss, type= "b", xlab = "number of clusters", ylab = "within sum of squawre")


# k-means clustering





