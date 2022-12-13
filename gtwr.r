#Set directory data, sudah diatur jadi dibawah tidak perlu tulis alamat lagi
setwd("E:/NSC")

#Install Package GTWR dan Pemetaan Spasial
library(GWmodel)
library(spdep)
library(rgdal)
library(spdep)
library(raster)
library(lmtest)
library(tigris)

#Input Data
datagtwr=read.csv("jawaa.csv", sep=";") 
head(datagtwr)

#Correlation
install.packages("corrplot")
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
mcor <- cor(datagtwr[6:16])
corrplot(mcor, method="color", col=col(200),  
         type="upper", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=0 #Text label color and rotation
)

#Spatial Exploration
indo <-readOGR(dsn='Admin2Kabupaten/idn_admbnda_adm2_bps_20200401.shp',
               layer='idn_admbnda_adm2_bps_20200401')
datagtwr.sort <- datagtwr[1:118,]
datagtwr.sort <- datagtwr.sort[order(datagtwr.sort$Nama.Wilayah),]
jawa <- geo_join(spatial_data=indo, 
                 data_frame=datagtwr.sort, by_sp="ADM2_PCODE", 
                 by_df="ADM2_PCODE", how = "inner")

jawa$kemiskinan2019<-datagtwr[1:118,6]
jawa$kemiskinan2020<-datagtwr[119:236,6]
jawa$kemiskinan2021<-datagtwr[237:354,6]

colfunc<-colorRampPalette(c("#379237","green","yellow","orange","red"))
color<-colfunc(5)
spplot(jawa,c("kemiskinan2019","kemiskinan2020","kemiskinan2021"), layout = c(1,3),
       as.table = TRUE,col.regions=color,cuts=4)

#uji asumsi keragaman spasial
reg=lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=datagtwr)
summary(reg)
bptest(reg)

#Transformation to spasial data
data.sp.gtwr=datagtwr
coordinates(data.sp.gtwr)=4:5 #lat long
class(data.sp.gtwr)
head(data.sp.gtwr)

## GTWR ##

#Bandwidth
band.gtwr=bw.gtwr(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=data.sp.gtwr,obs.tv =data.sp.gtwr$Tahun,
                  longlat = F,kernel = "gaussian",approach = "CV",adaptive = T)

#Estimation Model
model2=gtwr(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10,data=data.sp.gtwr,obs.tv =data.sp.gtwr$Tahun,
            st.bw = band.gtwr, kernel="gaussian", longlat = F,adaptive = T)
model2
model2$SDF

#thitung
thitung=as.data.frame(model2$SDF[16:20])

#beta hat
beta=as.data.frame(model2$SDF[,1:11])
View(beta)

beta$Nama.Wilayah<-datagtwr$Nama.Wilayah
beta$Tahun<-datagtwr$Tahun
boxplot(beta$X1 ~ beta$Tahun,xlab="Tahun",ylab="X1")
boxplot(beta$X9 ~ beta$Tahun,xlab="Tahun",ylab="X9")

jawa$X1_2021<-beta$X1[237:354]
spplot(jawa,c("X1_2021"), layout = c(1,1),
       as.table = TRUE,col.regions=color,cuts=4,main="Rata-rata Lama Sekolah Penduduk 15+")
jawa$X9_2021<-beta$X9[237:354]
spplot(jawa,c("X9_2021"), layout = c(1,1),
       as.table = TRUE,col.regions=color,cuts=4,main="Tingkat Partisipasi Angkatan Kerja")

#Y and Yhat
Y=as.data.frame(model2$SDF[,12:13])
jawa@data<-cbind(GTWR.kemiskinan2019=Y[1:118,2],
                 GTWR.kemiskinan2020=Y[119:236,2],
                 GTWR.kemiskinan2021=Y[237:354,2],
                 jawa@data)
spplot(jawa,c("kemiskinan2019","GTWR.kemiskinan2019","kemiskinan2020","GTWR.kemiskinan2020",
              "kemiskinan2021","GTWR.kemiskinan2021"),
       layout = c(2,3),
       as.table = TRUE, col.regions=color,cuts=4)

#Correlation
## GTWR
cor(Y[,1:2])
## Linear Reg
reglin<-datagtwr[6:16]
yhatreglin=data.frame(reglin, yhat = fitted(reg))
cor(x=yhatreglin$Y, y=yhatreglin$yhat)

#Asumsi error
error <- datagtwr$Y-model2$SDF$

## kenormalan
shapiro.test(error)
qqnorm(error)

## autokorelasi
RunsTest(error)

# heterogenitas
bptest(error)