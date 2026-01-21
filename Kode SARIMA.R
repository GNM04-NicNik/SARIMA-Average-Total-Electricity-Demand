# Load library
library(TSA) # For EACF function
library(tseries) # For time series analysis
library(forecast) # For forecasting data
library(lmtest) # For Coefficient of model
library(dplyr) # Manipulate Data
library(zoo) # Untuk ubah dataframe ke time series data
library(xts) # For time series manipulation
library(vrtest) # For time series variance ratio test



df<-read.csv("C:\\Nicholas\\Universitas Indonesia\\Kuliah\\Semester 4\\Metper\\Final Project\\data_timeseries_NSW_monthly.csv")
df

#Pre-Processing
sapply(df,class) #Cek tipe df tiap kolom
dim(df) #Cek dimensi df
sapply(df, function(x) sum(is.na(x))) #Cek missing value pada df


# Cek apakah ada duplikat
duplicated(df)
# Hitung duplikat df
sum(duplicated(df))
View(df)
## Tidak ada duplikat df

summary(df)

#Drop Kolom Indeks
df<-df[, !(colnames(df) %in% c("Index"))] 
View(df)

# Append "-01" to each date string to represent the first day of the month
df$Year<- paste0(df$Year, "/01")

# Convert to Date using the correct format
df$Year <- as.Date(df$Year, format = "%Y/%m/%d")
df

data_timeseries<-ts(df$TOTAL.DEMAND, frequency = 1) #Ini data belum dalam  bentuk period 12 jadi perlu dispesifikasikan pada pemodelan

class(data_timeseries)
data_timeseries
summary(data_timeseries)

# Plot Data
plot(data_timeseries, main="Plot Rata-Rata Demand Electricity di Daerah NSW (Januari 1999 - Desember 2023)",ylab="Total Demand", xlab = "Date-Time", type='l')
dat<-xts(df$TOTAL.DEMAND, order.by=df$Year);dat #Hanya untuk plot
plot(dat, main="Plot Rata-Rata Demand Electricity di Daerah NSW (Januari 1999 - Desember 2023)",ylab="Total Demand", xlab = "Date-Time", type='l')

periodogram(data_timeseries)
ggtsdisplay(data_timeseries)
BoxCox.lambda(data_timeseries)
#Data hampir Perfect Linear Transformation karena hasil BoxCox.lambda sangat deket ke -1. Jadi data tidak perlu ditransformasikan dengan log

# Uji stasioneritas dataset
adf_test<-adf.test(data_timeseries, k = 0)
kpss_test<-kpss.test(data_timeseries)
print("ADF Test :")
print(adf_test)

print("KPSS Test :")
print(kpss_test)

# Hasil test menunjukkan bahwa tidak terdapat unit root (Uji ADF menolak H0), tetapi masih terdapat tren non stasioner (Uji KPSS menolak H0)
# Oleh sebab itu akan dicek menggunakan Automatic Variance Ratio Test

Auto.VR(data_timeseries)
# Hasil Autovariansi rasio menghasilkan nilai 119.4412. Angka ini masih jauh dari 1
# Oleh sebab itu data masih heteroskedastis dan data belum stasioner. Maka perlu dilakukan differencing
# Hasil PACF dan ACF menunjukkan terdapat signifikansi pada lag 12 dan 24. Oleh sebab itu perlu dilakukan differencing seasonal juga dengna lag = 12.

# Differencing pertama
# Data differencing 1 kali 
data_tsdiff1<-diff(diff(data_timeseries, lag=12,difference=1),difference=1)
ggtsdisplay(data_tsdiff1)

#Cek kestasioneran data setelah differencing pertama
adf.test(data_tsdiff1)
kpss.test(data_tsdiff1)

#Terlihat bahwa data sudah stasioner setelah melakukan differencing. Kedua test menghasilkan keputusan bahwa data sudah stasioner
#Akan dicek lebih lengkap dengan Variansi rasio tes
Auto.VR(data_tsdiff1)

# Nilai menjadi negatif dan dekat ke angka -1 sehingga dapat disimpulkan bahwa data sudah hampir sepenuhnya stasioner

#Cek model yang memungkinkan untuk ARIMA
eacf(data_tsdiff1)

# Model Pertama
model1<-arima(data_timeseries, order=c(0,1,2), seasonal=list(order = c(1, 1, 1), period = 12))
summary(model1)
coef(model1)

# Model Diagnostic Model Pertama
autoplot(model1)
#Titik-Titik pada gambar Inverse AR Root sudah berada di dalam lingkaran dan sudah rapih. Namun, titik-titik pada gambar Inverse MA roots masih terdapat titik yang sejajar atau berdempetan dengan titik yang lain.
coeftest(model1)
# Hasil uji Z untuk tiap koefisien menunjukkan hasil yang hampir signifikan semua kecuali koefisien atau parameter AR pada seasonal
checkresiduals(model1)
# Hasil menunjukkan bahwa residual sudah hampir semua berupa white noise. Tidak ada random walk lagi.
# Namun, hasil Ljung-Box Test model masih menolak H0 sehingga asumsi masih dilanggar
ggtsdisplay(model1$residuals)
tsdiag(model1)
# Plot p-values for Ljung-Box Statistic menunjukkan titik-titik berada di atas garis signifikan untuk semua lag. 
# Beberapa titik sangat dekat dengan garis, tetapi masih terhitung signifikan.
Box.test(resid(model1), type = "Ljung")
# Hasil Ljung-Box Test menunjukkan bahwa hipotesis null tidak berhasil ditolak
# Oleh sebab itu residual saling independen 
shapiro.test(resid(model1))
qqnorm(residuals(model1))
qqline(residuals(model1))
hist(residuals(model1), xlab = 'Residuals')

# P-value lebih kecil dari 0,05 maka H0 ditolak sehingga residual tidak mengikuti asumsi normalitas.
# Hasil uji normalitas dengan shapiro test menunjukkan bahwa residual dari model masih belum mengikuti distribusi Normal

#one-sample t-test for mean 0
resid.model1 = model1$residuals
t.test(resid.model1, mu = 0, alternative = "two.sided")
plot(model1$residuals, ylab = "Residual")

#Akan dicoba lihat hasil peramalan untuk 5 titik kedepan (Januari-Mei)
Ramal <- predict(model1, n.ahead = 5)
values = Ramal$pred;values

nilai.asli<-c(7616.089782,7887.902067,7286.726556,6937.607252,7749.012011)

#Plot Data Time Series dan Forecastnya
plot(data_timeseries,main="Plot Hasil Forecast untuk 5 Titik Waktu Kedepan",ylab='Rata-Rata Total Demand',type="l",pch=20,col="black")
points(x=(301:305),y=values,type="o",pch=20,col="green")#Untuk Forecast
lines(x=(301:305),y=values,pch=19, col="green") 
points(x=(301:305),y=nilai.asli,type="o",pch=20,col="purple")#Untuk Data Asli 
lines(x=(301:305),y=nilai.asli,pch=19, col="purple")  #Untuk Data Asli


#Sebab belum semua asumsi terpenuhi maka akan dicoba differencing pada seasonal dengan orde 2 pada seasonal
data_tsdiff2<-diff(diff(data_timeseries, lag=12,difference=2),difference=1)
ggtsdisplay(data_tsdiff2)

#ACF dan PACF menunjukkan sudah berkurang lag yang berada di atas garis signifikan.
#Terdapat Tails down pada PACF yang memungkinkan AR tidak diperlukan pada model
#Garis pada lag 3 signifikan di ACF sehingga memungkinkan orde 3 pada MA
#Lag 12 satu-satunya lag seasonal yang signifikan sehingga memungkinkan orde 1 untuk AR dan MA pada model seasonal

#Cek kestasioneran data setelah differencing pertama
adf.test(data_tsdiff2)
kpss.test(data_tsdiff2)

#Terlihat bahwa data sudah stasioner setelah melakukan differencing. Kedua test menghasilkan keputusan bahwa data sudah stasioner
#Akan dicek lebih lengkap dengan Variansi rasio tes
Auto.VR(data_tsdiff2)

# Nilai tetap negatif dan semakin dekat ke angka -1 sehingga akan disimpulkan bahwa data sudah terhitung stasioner.

#Cek model ARIMA yang memungkinkan
eacf(data_tsdiff2)

# Model Kedua
model2<-arima(data_timeseries, order=c(0,1,3), seasonal=list(order = c(1, 2, 1), period = 12))
summary(model2)
coef(model2)

# Model Diagnostic
autoplot(model2)
coeftest(model2)
# Hasil uji Z untuk tiap koefisien menunjukkan hasil yang signifikan semua
checkresiduals(model2)
# Ljung Box Test tidak berhasil menolak H0
# Hasil menunjukkan bahwa residual sudah hampir semua berupa white noise. Tidak ada random walk lagi.
# Namun, lag 9 dan lag 24 signifikan sehingga kemungkinan perlu dilakukan differencing lagi.
ggtsdisplay(model2$residuals)
tsdiag(model2)
#p-values for Ljung-Box Statistic menghasilkan hasil yang sangat bagus
# Semua titik berada di atas garis signifikan, tetapi lag 9 dan 10 masih dekat dengan garis signifikan

Box.test(resid(model2), type = "Ljung")
# Hasil Ljung-Box Test menunjukkan bahwa hipotesis null tidak berhasil ditolak
# Oleh sebab itu residual saling independen 
shapiro.test(resid(model2))
qqnorm(residuals(model2))
qqline(residuals(model2))
hist(residuals(model2), xlab = 'Residuals')

# Hasil uji normalitas dengan shapiro test berhasil menolak H0
# Oleh sebab itu residual dari model belum distribusi Normal. Maka model belum memenuhi asumsi normalitas

#one-sample t-test for mean 0
resid.model2 = model2$residuals
t.test(resid.model2, mu = 0, alternative = "two.sided")
#Hasil t test untuk menguji mean residual juga menunjukkan bahwa data tidak cukup untuk menolak H0
#Oleh sebab itu, hasil t test menyimpulkan residual sudah memiliki mean sama dengan 0 dengan taraf signifikansi 5%

plot(model2$residuals, ylab = "Residual")
#Hasil plot sudah menunjukkan bahwa residual tidak mengikuti proses random walk maka model sudah bagus

#Hasil diagnostik model menunjukkan hasil yang baik kecuali asumsi normalitas residual. Oleh sebab itu, perlu dicari model lain yang lebih baik
# Akan dicoba dengan mendifferencingkan ARIMA biasa dengan orde 2 dan seasonal dengan orde 1


# Forecast untuk 5 titik kedepan (Januari-Mei)
Ramal2 <- predict(model2,level=c(95), n.ahead = 5)
values2 = Ramal2$pred;values2

#Plot Data Time Series dan Forecastnya
plot(data_timeseries,main="Plot Hasil Forecast untuk 5 Titik Waktu Kedepan",ylab='Rata-Rata Total Demand',type="l",pch=20,col="black")
points(x=(301:305),y=values2,type="o",pch=20,col="green")#Untuk Forecast
lines(x=(301:305),y=values2,pch=19, col="green") 
points(x=(301:305),y=nilai.asli,type="o",pch=20,col="purple")#Untuk Data Asli 
lines(x=(301:305),y=nilai.asli,pch=19, col="purple")  #Untuk Data Asli




# Model Ketiga
data_tsdiff3<-diff(diff(data_timeseries, lag=12, difference=1),difference=2)
ggtsdisplay(data_tsdiff3)

#Cek kestasioneran data setelah differencing pertama
adf.test(data_tsdiff3)
kpss.test(data_tsdiff3)

#Terlihat bahwa data sudah stasioner setelah melakukan differencing. Kedua test menghasilkan keputusan bahwa data sudah stasioner
#Akan dicek lebih lengkap dengan Variansi rasio tes
Auto.VR(data_tsdiff3)

# Nilai tetap negatif dan semakin dekat ke angka -1 sehingga akan disimpulkan bahwa data sudah terhitung stasioner.

#Cek model ARIMA yang memungkinkan
eacf(data_tsdiff3)

# Model Ketiga
model3<-arima(data_timeseries, order=c(0,2,2), seasonal=list(order = c(1, 1, 1), period = 12))
summary(model3)
coef(model3)

# Model Diagnostic
autoplot(model3)
coeftest(model3)
# Hasil uji Z untuk tiap koefisien menunjukkan hasil yang signifikan semua
checkresiduals(model3)
# Hasil menunjukkan bahwa residual belum semua berupa white noise. Tidak ada random walk lagi.
ggtsdisplay(model3$residuals)
tsdiag(model3)
Box.test(residuals(model3), type = "Ljung")
# Hasil Ljung-Box Test menunjukkan bahwa hipotesis null tidak berhasil ditolak
# Oleh sebab itu residual saling independen 
shapiro.test(resid(model3))
qqnorm(residuals(model3))
qqline(residuals(model3))
hist(residuals(model3), xlab = 'Residuals')

# Hasil uji normalitas dengan shapiro test tidak berhasil menolak H0
# Oleh sebab itu residual dari model sudah distribusi Normal. Maka model sudah memenuhi asumsi normalitas

#one-sample t-test for mean 0
resid.model3 = model3$residuals
t.test(resid.model3, mu = 0, alternative = "two.sided")
#Hasil t test untuk menguji mean residual juga menunjukkan bahwa data tidak cukup untuk menolak H0
#Oleh sebab itu, hasil t test menyimpulkan residual sudah memiliki mean sama dengan 0 dengan taraf signifikansi 5%

plot(model3$residuals, ylab = "Residual")
#Hasil plot sudah menunjukkan bahwa residual tidak mengikuti proses random walk maka model sudah bagus


# Forecast untuk 5 titik kedepan (Januari-Mei)
Ramal3 <- predict(model3,level=c(95), n.ahead = 5)
values3 = Ramal3$pred;values3

#Plot Data Time Series dan Forecastnya
plot(data_timeseries,main="Plot Hasil Forecast untuk 5 Titik Waktu Kedepan",ylab='Rata-Rata Total Demand',type="l",pch=20,col="black")
points(x=(301:305),y=values3,type="o",pch=20,col="green")#Untuk Forecast
lines(x=(301:305),y=values3,pch=19, col="green") 
points(x=(301:305),y=nilai.asli,type="o",pch=20,col="purple")#Untuk Data Asli 
lines(x=(301:305),y=nilai.asli,pch=19, col="purple")  #Untuk Data Asli


# Model Keempat
#Sebab data belum memenuhi asumsi juga, akan dinaikan orde pada MA
model4<-arima(data_timeseries, order=c(0,2,3), seasonal=list(order = c(1, 1, 1), period = 12))
summary(model4)
coef(model4)

# Model Diagnostic
autoplot(model4)
coeftest(model4)
# Hasil uji Z untuk tiap koefisien menunjukkan hasil yang signifikan semua
checkresiduals(model4)
# Hasil menunjukkan bahwa residual sudah hampir semua berupa white noise. Tidak ada random walk lagi.
ggtsdisplay(model4$residuals)
tsdiag(model4)
Box.test(residuals(model4), type = "Ljung")
# Hasil Ljung-Box Test menunjukkan bahwa hipotesis null tidak berhasil ditolak
# Oleh sebab itu residual saling independen 
shapiro.test(resid(model4))
qqnorm(residuals(model4))
qqline(residuals(model4))
hist(residuals(model4), xlab = 'Residuals')

# Hasil uji normalitas dengan shapiro test tidak berhasil menolak H0
# Oleh sebab itu residual dari model sudah distribusi Normal. Maka model sudah memenuhi asumsi normalitas

#one-sample t-test for mean 0
resid.model4 = model4$residuals
t.test(resid.model4, mu = 0, alternative = "two.sided")
#Hasil t test untuk menguji mean residual juga menunjukkan bahwa data tidak cukup untuk menolak H0
#Oleh sebab itu, hasil t test menyimpulkan residual sudah memiliki mean sama dengan 0 dengan taraf signifikansi 5%

plot(model4$residuals, ylab = "Residual")
#Hasil plot sudah menunjukkan bahwa residual tidak mengikuti proses random walk maka model sudah bagus



# Forecast untuk 5 titik kedepan (Januari-Mei)
Ramal4 <- predict(model4,level=c(95), n.ahead = 5)
values4 = Ramal4$pred;values4


#Plot Data Time Series dan Forecastnya
plot(data_timeseries,main="Plot Hasil Forecast untuk 5 Titik Waktu Kedepan",ylab='Rata-Rata Total Demand',xlab="Waktu",type="l",pch=20,col="black")
points(x=(301:305),y=values4,type="o",pch=20,col="green")#Untuk Forecast
lines(x=(301:305),y=values4,pch=19, col="green") 
points(x=(301:305),y=nilai.asli,type="o",pch=20,col="purple")#Untuk Data Asli 
lines(x=(301:305),y=nilai.asli,pch=19, col="purple")  #Untuk Data Asli
legend("topright", legend = c("Actual Values", "Forecast"), 
       col = c("purple", "green"), lty = c(1, 1),cex=1)
autoplot(cbind(data_timeseries, values3))

#Deteksi Outlier pada Model
detectAO(model4)
detectIO(model4)

qqnorm(residuals(model4))
qqline(residuals(model4))
model_io<-arima(data_timeseries, order=c(0,2,3), seasonal=list(order = c(1, 1, 1), period = 12), io=c(241))

# Perbandingan Nilai Forecast Model 1, 2 , 3, dan 4
abs(nilai.asli-values)
abs(nilai.asli-values2)
abs(nilai.asli-values3)
abs(nilai.asli-values4)

#Walau hasil dari model 1 dan 2 baik, tetapi asumsi tidak terpenuhi semua
#Maka Model terbaik dalam memodelkan data adalah model keempat yaitu SARIMA(0,2,3)(1,1,1)[12]
#Karena semua diagnostik dari model keempat menunjukkan hasil yang baik dan tidak terdapat asumsi yang ditolak


