# 1. Выполнить учебный импорт любых таблиц данных из csv-файла и xls-таблицы.
df <- read.csv("Любимый жанр музыки.csv", sep=";", header=T,
             fileEncoding="cp1251")

df[15, ] <- df[16, ]
df <- df[-c(16), ]

library(readxl)
test_data <- read_excel("test.xls", sheet = "test"); test_data


# 2. Выполнить дескриптивный анализ данных из ЛР №2.
num_idx <- which(sapply(df, is.numeric))

hist(df$Джаз, main="Гистограмма по столбцу Джаз", xlab="Оценки", 
     ylab="Частота", col = rainbow(6))
boxplot(df[, num_idx], main='Коробчатые диаграммы жанров музыки',
        xlab='Жанры', ylab='Оценки', col=rainbow(11))
summary(df)

# 3. Выполнить сортировку наборов данных по выбранному признаку.
df_sorted <- df[order(df$Рок, decreasing = T), ]; View(df_sorted)

# 4.	Сформировать отдельные наборы данных по одинаковому признаку
#(например, составить subdataset, из студентов, отдавших предпочтение по 
# шкале > 7 определенной книге), вывести результат,  выполнить подсчет 
# размерностей новых таблиц, снова выполнить их анализ –
#гистограмма, боксплот, серединные меры
syntwave_more_than_5 <- subset(df, Синтвейв > 5); syntwave_more_than_5
dim(syntwave_more_than_5)
attach(syntwave_more_than_5)

hist(Синтвейв, main="Гистограмма по столбцу синтвейв, оценки > 5", xlab="Оценки", 
     ylab="Частота", col = rainbow(4))
boxplot(Синтвейв, main='Коробчатая диаграмма жанра синтвейв',
        xlab='Синтвейв', ylab='Оценки', col=rainbow(1))

mean(Синтвейв)
median(Синтвейв)

detach(syntwave_more_than_5)