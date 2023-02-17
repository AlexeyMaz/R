df<-read.csv("Любимый жанр музыки.csv", sep=";", header=T,
                   fileEncoding="cp1251")
View(df)

num_idx <- which(sapply(df, is.numeric))

# №1. max, min, mean по всем столбцам
sapply(na.omit(df[, num_idx]), max)
sapply(na.omit(df[, num_idx]), min)
sapply(na.omit(df[, num_idx]), mean)

# по возрастанию: 3 10 5 8 9 6 2 4 1 7
# №3. Рейтинг
idxes <- order(sapply(na.omit(df[, num_idx]), mean), decreasing=T)
idxes <- sapply(idxes, function (x) x <- colnames(df)[x + 2])
data.frame('Рейтинг жанров'=idxes)
