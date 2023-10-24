library(DT)
library(factoextra)
library(ggplot2)
library(readxl)

#Memanggil Data
data<-read.delim("clipboard")
data
View(data)
corr<-cor(data, use = "complete.obs")
corr
#Penentuan Banyak KU
#Cara 2:Nilai Eigen
n_eig<-eigen(corr)
n_eig

#Standarisasi data
data_standardized <- scale(x=data)
data_standardized

#Matriks Kovarians
data_covariance <- cov(data_standardized)
data_covariance
#correlation
data_cor <- cor(data_standardized)
data_cor

#Nilai eigen matriks kovarians
data_eigen <- eigen(data_covariance)
data_eigen
data_eigen <- eigen(data_cor)
data_eigen

#Cara 1: Ragam
data_pca <- prcomp(x = data, scale=TRUE, center = TRUE)
names(data_pca)
summary(data_pca)

data_pca$rotation

#Cara 3 : Scree Plot
fviz_eig(data_pca, addlabels = TRUE, ylim=c(0,80))
round(data_pca$rotation[,1:3],2)

#Pengujian Multikolinearitas
library(PerformanceAnalytics)
chart.Correlation(data[,1:5])
uji_bart <-function(x)
{
  method <-"Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x))
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <-p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail = FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value, method=method, data.name=data.name), class="htest"))
}

uji_bart(data)

#pengujian kembali data multikolinearitas
###Analisis PCA###
library(factoextra)
pca_A <- prcomp(data, scale. = TRUE)
pca_A
round(pca_A$rotation,2)

#bayaknya jumlah faktor dilihat dari nilai eigen value > 1 atau nilai varians > 80% 
round(pca_A$sdev^2,2) 
fviz_eig(pca_A, addlabels = TRUE, ylim = c(0, 80))
round(pca_A$rotation[,1:3],2)

##data hasil PCA
pca_fix=pca_A$x[,1:3]
pca_fix
View(pca_fix)
new_Data=as.data.frame(pca_fix)
new_Data
uji_bart(new_Data)
