df<-read.csv("Любимый жанр музыки.csv", sep=";", header=T,
                   fileEncoding="cp1251")

df[15, ] <- df[16, ]
df <- df[-c(16), ]
View(df)

num_idx <- which(sapply(df, is.numeric))

# №1. max, min, mean по всем столбцам
maxes <- sapply(na.omit(df[, num_idx]), max); maxes
mins <- sapply(na.omit(df[, num_idx]), min); mins
means <- sapply(na.omit(df[, num_idx]), mean); means

# №2. Подсчитать количество людей, отдавших предпочтение >7 и <3
more_than_7 <- sapply(df[, num_idx], function(x) sum(x > 7 & !is.na(x))); more_than_7
less_than_3 <- sapply(df[, num_idx], function(x) sum(x < 3 & !is.na(x))); less_than_3

# №3. Рейтинг жанров по убыванию
votes <- sort(means, decreasing = T); votes
data.frame('Рейтинг_жанров'=votes)

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
pl_df <- data.frame(x=colnames(df[, num_idx]), y=means); pl_df
# скип строк с NA
# pl_df <- pl_df[complete.cases(pl_df), ]
perf <-ggplot(pl_df, aes(x=reorder(pl_df$x, -pl_df$y), y, fill = x)) +
  geom_bar(stat="identity", show.legend = F); perf

# genre_str <- paste("Оценки жанра", tolower(genre))
ggp <- perf + labs(x="Жанр",y="Оценка", title='Средняя оценка жанров музыки'); ggp