###Kütüpahaneler
install.packages('MASS')
install.packages('corrplot')
library(MASS)
library(corrplot)

##2. Sentetik veri seti oluşturma

set.seed(123)
n<-100

satis_sayisi<-rnorm(n,mean=200,sd=30)
gunluk_musteri<-rnorm(n,mean=300,sd=20)

##Bağımlı değişken oluşturma
satis_sayisi<-0.5*gunluk_musteri+rnorm(n,mean=10,sd=5)

##data frame oluşturma
data<-data.frame(gunluk_musteri,satis_sayisi)
head(data)
str(data)
summary(data)

## veri setine genel hatları
View(data)

## kullandığımız verilen mod ve medyan değerleri
mean(data$gunluk_musteri)
median(data$gunluk_musteri)
hist(data$gunluk_musteri)

mean(data$satis_sayisi)
median(data$satis_sayisi)
summary(data)
hist(data$satis_sayisi)

#Eksik veri temizleme
data<-na.omit(data)

##eksik veri temizleme ve kontrolü
is_na_gunluk_musteri<-is.na(gunluk_musteri)
is_na_satis_sayisi<-is.na(satis_sayisi)
colSums(is.na(data))
##outlier hesaplama
z_scores<-scale(data$gunluk_musteri)
outliers<-scale(z_scores)>3

##Kolerasyon hesapla(müşteri artıkça satış artıyormu)
cor(data$gunluk_musteri,data$satis_sayisi)
cor_matrix<-cor(data)
corrplot(cor_matrix,method='color',addCoef.col = 'black')

##regresyon kurma
model<-lm(satis_sayisi~gunluk_musteri,data=data)
summary(model)

##
boxplot(data)
sort(data$gunluk_musteri)
##install.packages("corrplot")
library(corrplot)
cor_matrix<-cor(data)
corrplot(cor_matrix,method = 'color',
         addCoef.col = 'black',
         tl.col='black',
         number.cex = 0.7)



####normal dağılım kontrolü
hist(data$gunluk_musteri)
qqnorm(data$gunluk_musteri)
qqline(data$gunluk_musteri)

hist(data$satis_sayisi)
qqnorm(data$satis_sayisi)
qqline(data$satis_sayisi)

str(data)
#####---Aykırı değer yöntemi
Q1<-quantile(data$gunluk_musteri,0.25)
Q3<-quantile(data$gunluk_musteri,0.75)
##IQR hesaplama
IQR<-Q3-Q1
lower_bound<-Q1-1.5*IQR
upper_bound<-Q3 +1.5*IQR


aykiri<-data$gunluk_musteri[data$gunluk_musteri<lower_bound|
 data$gunluk_musteri >upper_bound]                                                                                          
length(aykiri)                                                                                             
                           
                              
                              
                              
