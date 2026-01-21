# Load library
library(TSA) #For EACF function
library(tseries)
library(forecast) #For forecasting data
library(lmtest)
library(dplyr) #Manipulate Data
library(zoo) #Untuk ubah dataframe ke time series data
library(xts) # For time series manipulation
library(vrtest) # For time series variance ratio test

#Load data tiap bulan dari Januari 2021 sampai Mei 2024
#Data Tahun 2021
januari2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202101_NSW1.csv")
februari2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202102_NSW1.csv")
maret2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202103_NSW1.csv")
april2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202104_NSW1.csv")
mei2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202105_NSW1.csv")
juni2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202106_NSW1.csv")
juli2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202107_NSW1.csv")
agustus2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202108_NSW1.csv")
september2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202109_NSW1.csv")
oktober2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202110_NSW1.csv")
november2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202111_NSW1.csv")
desember2021<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202112_NSW1.csv")

#Data Tahun 2022
januari2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202201_NSW1.csv")
februari2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202202_NSW1.csv")
maret2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202203_NSW1.csv")
april2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202204_NSW1.csv")
mei2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202205_NSW1.csv")
juni2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202206_NSW1.csv")
juli2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202207_NSW1.csv")
agustus2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202208_NSW1.csv")
september2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202209_NSW1.csv")
oktober2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202210_NSW1.csv")
november2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202211_NSW1.csv")
desember2022<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202212_NSW1.csv")

#Data Tahun 2023
januari2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202301_NSW1.csv")
februari2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202302_NSW1.csv")
maret2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202303_NSW1.csv")
april2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202304_NSW1.csv")
mei2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202305_NSW1.csv")
juni2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202306_NSW1.csv")
juli2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202307_NSW1.csv")
agustus2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202308_NSW1.csv")
september2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202309_NSW1.csv")
oktober2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202310_NSW1.csv")
november2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202311_NSW1.csv")
desember2023<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202312_NSW1.csv")

#Data Tahun 2024
januari2024<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202401_NSW1.csv")
februari2024<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202402_NSW1.csv")
maret2024<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202403_NSW1.csv")
april2024<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202404_NSW1.csv")
mei2024<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\Data NSW Electricity\\PRICE_AND_DEMAND_202405_NSW1.csv")


#Gabungkan data menjadi 1 dataframe
data<-rbind(januari2021,februari2021,maret2021,april2021,mei2021,juni2021,juli2021,agustus2021,september2021,oktober2021,november2021,desember2021,
            januari2022,februari2022,maret2022,april2022,mei2022,juni2022,juli2022,agustus2022,september2022,oktober2022,november2022,desember2022,
            januari2023,februari2023,maret2023,april2023,mei2023,juni2023,juli2023,agustus2023,september2023,oktober2023,november2023,desember2023
           )

sapply(data,class) #Cek tipe data tiap kolom
dim(data) #Cek dimensi data
sapply(data, function(x) sum(is.na(x))) #Cek missing value pada data


# Cek apakah ada duplikat
duplicated(data)
# Hitung duplikat data
sum(duplicated(data))
## Tidak ada duplikat data

data<-data[, !(colnames(data) %in% c("REGION","PERIODTYPE"))] #Hapus kolom REGION dan PERIODTYPE


# Convert DATE column to Date type
data$SETTLEMENTDATE <- as.POSIXct(data$SETTLEMENTDATE, format = "%Y/%m/%d %H:%M:%S") #Ubah ke date tipe data sesuai dataset
View(data)


data_timeseries<-xts(data$TOTALDEMAND,data$SETTLEMENTDATE)
class(data_timeseries)
data_timeseries
# Menyimpan data frame ke dalam file CSV
write.csv(data, file = "data_timeseries_NSW.csv", row.names = FALSE)


summary(data_timeseries)
#Plot data
plot(data_timeseries, main="Plot Jumlah Demand Electricity di Daerah NSW (Januari 2021 - Desember 2023)",ylab="Total Demand", xlab = "Date-Time", type='l')
decompose(data_timeseries)

# Cek Kestasioneran data dengan uji ADF (Augmented Dickey-Fuller) dan Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
adf_test<-adf.test(data_timeseries, k = 0)
#Phillips-Perron Test : pp.test(data_timeseries)
kpss_test<-kpss.test(data_timeseries)
print("ADF Test :")
print(adf_test)

print("KPSS Test :")
print(kpss_test)

#Auto.VR(data_timeseries)
#Sebab hasil automatic variance ratio test jauh dari 1 maka data non stasioner
#Maka data perlu dilakukan differencing


# Model Spesification
eacf(data_timeseries)
acf(data_timeseries)
pacf(data_timeseries)
resid = armasubsets(y = data_timeseries, nar = 8, nma = 8, y.name = 'data_timeseries', ar.method = 'ols')
plot(resid)

data_timeseries_diff <- diff(data_timeseries, differences = 1)
data_timeseries_diff <- na.omit(data_timeseries_diff)
dim(data_timeseries_diff)

eacf(data_timeseries_diff)
acf(data_timeseries_diff)
pacf(data_timeseries_diff)

plot(data_timeseries_diff, main="Plot Jumlah Demand Electricity di Daerah NSW (Januari 2022 - Mei 2024)",ylab="Total Demand", xlab = "Date-Time", type='l')
#Auto.VR(data_timeseries_diff)
adf_test2<-adf.test(data_timeseries_diff, k = 0)
#Phillips-Perron Test : pp.test(data_timeseries)
kpss_test2<-kpss.test(data_timeseries_diff)
print("ADF Test :")
print(adf_test2)

print("KPSS Test :")
print(kpss_test2)

model<-auto.arima(data_timeseries_diff)
summary(model)
coef(model)


