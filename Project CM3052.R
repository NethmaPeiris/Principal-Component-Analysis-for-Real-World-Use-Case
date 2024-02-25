# Project CM3052
# DNR Peiris
# D/DBA/21/0026

# First Setting a working directory 
setwd("D:/KDU/Semester 06/Assignments/Multivariant/backup code")

#Loading nessacry libraries
library(readxl)
library(skimr)
library(corrplot)
library(reshape2)
library(ggplot2)
library(factoextra)
library(NbClust)

#loading data set
df = data.frame(read_xlsx("CM 3052_project data.xlsx"))
df
head(df,5)
tail(df,5)

# Exploratory Data Analytics

  #1.  perform skim to display summary statistics
  skim(df)

  #2. checking the null values of the data set
  null_val <- sum(is.null(df))
  null_val

  #3. Correlation matrix calculation
  cor_matrix <- cor(df)
  corrplot(cor_matrix, method = "color", tl.col = "black",tl.srt = 45, type = "full")
  
  #4. Scatter plot matrix
  pairs(df[-1], main = "Scatterplot Matrix", col="red")
  
  
#Analysis 
  # Objective 1: Summarizing the chemical components into sub groups
    # Perform Principal Component Analysis (PCA)
    pca_result <- prcomp(df[, c("Be", "Cr", "Fe", "Ni", "Cu", "As", "Cd", "Ba", "Tl", "Pb", "U")], scale = TRUE)
    pca_result
    
  # Objective 2:  Well water samples can be cluster into homogeneous groups according the structure of the mixture components.
    #determine the no of clusters 
    fviz_nbclust(df[-1],kmeans,method = "wss")
    
    #2nd method to obtain no of optimal clusters
    NbClust(data = df[-1], diss = NULL, distance = "euclidean", min.nc = 2, max.nc = 15, 
            method = "ward.D2", index = "all", alphaBeale = 0.1)
    
    #optimal no of clusters =3
    d_df <- dist(df[-1])
    d_df
    wardse <- hclust(d_df,"ward.D2")
    
    # dendrogram  
    plot(wardse)
    plot(wardse, hang = -1)
    rect.hclust(wardse, k=3 , border="blue")
    rect.hclust(wardse, k=3 , border= 2:5)
    
    #data frame with sample no and cluster no
    clusters <- cutree(wardse,k=3)
    df2 <- data.frame(df$well.water.sample_No,clusters)
    head(df2)
         
  
    #Objective 3:
    # Chemical mixtures in well water samples are in line with the standard accepted values in well water
    standard_values <- c(Be = 4, Cr = 100, Fe = 300, Ni = 20 , Cu = 1300, As = 10, Cd =5, Ba = 2000, Tl = 0.5, Pb = 15, U = 30)
    standard_values
    
    mean_values <- colMeans(df[, c("Be", "Cr", "Fe", "Ni", "Cu", "As", "Cd", "Ba", "Tl", "Pb", "U")])
    mean_values
   
    
    # Compare mean values of your data with standard values
    n=92 #no of samples
    p=11 #no of variables
    ex1 <- cbind(df[-1])
    S <- cov(ex1)  #calculate the co-variance matrix
    S
    
    x_bar = matrix(mean_values,c(11,1))
    x_bar
    
    mu_note =matrix(standard_values,c(11,1))
    mu_note
    
    #Test statistics 
    T2_cal <- n*t(x_bar-mu_note)%*%solve(S)%*% (x_bar-mu_note)
    T2_cal
    
    Table_value =(n-1)*p/(n-p)*qf(0.95,p,n-p)
    Table_value
    
    
    
    

    
