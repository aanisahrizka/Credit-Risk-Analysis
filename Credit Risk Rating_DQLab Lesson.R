library("openxlsx")
library("C50")
library("reshape2")

#Preparing Data
dataCreditRating <- read.xlsx(xlsxFile = "https://storage.googleapis.com/dqlab-dataset/credit_scoring_dqlab.xlsx")
str(dataCreditRating)
dataCreditRating$risk_rating<-as.factor(dataCreditRating$risk_rating) 
input_columns <- c("durasi_pinjaman_bulan", "jumlah_tanggungan")
datafeed <- dataCreditRating[ , input_columns ]
str(datafeed)

#Dataset for training and testing
set.seed(100)
indeks_training_set <- sample(900, 800)
input_training_set<-datafeed[indeks_training_set,]
class_training_set<-dataCreditRating[indeks_training_set,]$risk_rating
input_testing_set<-datafeed[-indeks_training_set,]

str(input_training_set)
str(class_training_set)
str(input_testing_set)

#model summary
risk_rating_model <- C5.0(input_training_set, class_training_set) 
summary(risk_rating_model)
plot(risk_rating_model)

risk_rating_model1 <- C5.0(input_training_set, class_training_set, 
                           control = C5.0Control(label="Risk Rating"))
summary(risk_rating_model1)

#Predict the testing set
predict(risk_rating_model, input_testing_set)
input_testing_set$risk_rating <- dataCreditRating[-indeks_training_set,]$risk_rating
input_testing_set$hasil_prediksi <- predict(risk_rating_model, input_testing_set)
print(input_testing_set)

#Confusion Matrix
dcast(hasil_prediksi ~ risk_rating, data=input_testing_set)

input_testing_set$risk_rating==input_testing_set$hasil_prediksi
input_testing_set[input_testing_set$risk_rating==input_testing_set$hasil_prediksi,]
nrow(input_testing_set[input_testing_set$risk_rating==input_testing_set$hasil_prediksi,]) #data yang benar ada 75

nrow(input_testing_set[input_testing_set$risk_rating!=input_testing_set$hasil_prediksi,])

#Creating new data
aplikasi_baru <- data.frame(jumlah_tanggungan = 6, durasi_pinjaman_bulan = 12)
print(aplikasi_baru)

#Predicting
predict(risk_rating_model, aplikasi_baru)
