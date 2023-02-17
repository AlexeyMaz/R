df<-read.csv("Любимый жанр музыки.csv", sep=";", header=T,
                   fileEncoding="cp1251")

df[15, ] <- df[16, ]
df <- df[-c(16), ]
View(df)

num_idx <- which(sapply(df, is.numeric))

# №1. max, min, mean по всем столбцам
sapply(na.omit(df[, num_idx]), max)
sapply(na.omit(df[, num_idx]), min)
means <- sapply(na.omit(df[, num_idx]), mean)
means

# №3. Рейтинг жанров по убыванию
idxes <- order(means, decreasing=T)
idxes <- sapply(idxes, function (x) x <- colnames(df)[x + 2])
data.frame('Рейтинг_жанров'=idxes)

# №4. Столбчатая диаграмма оценок
library(ggplot2)

# genre <- readline(prompt = 'Диаграмму оценок какого жанра хотите посмотреть? ')
# # сделать первую букву заглавной
# genre <- gsub(' ', '', 
#               paste(toupper(substr(genre, 1, 1)), substr(genre, 2, nchar(genre))))


# оставить только фамилию
# surnames <- df[, 2]
# for (i in 1 : nrow(df))
# {
#   first_space <- as.vector(gregexpr(' ', df[i, 2]))[[1]][1]
#   first_space
#   surnames[i] <- substr(df[i, 2], 1,
#                      first_space - 1)
# }

# диаграмма
pl_df <- data.frame(x=colnames(df[, num_idx]), y=means)
# скип строк с NA
pl_df <- pl_df[complete.cases(pl_df), ]
perf <-ggplot(pl_df, aes(x, y, fill = x)) +
  geom_bar(stat="identity", show.legend = F)
perf

# genre_str <- paste("Оценки жанра", tolower(genre))
ggp <- perf + labs(x="Жанр",y="Оценка", title='Средняя оценка жанров музыки')
ggp