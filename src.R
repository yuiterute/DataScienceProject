# Библиотеки
library('tidyverse')
library('openintro')
library('ggplot2')
library('readr')
library('scales')
library('reshape2')
library('viridis')
library('broom')
library('Metrics')
library('dplyr')
library('scales')
library('DataExplorer')
library('rpart')
library('rpart.plot')
library('randomForest')

# Загрузка и предварительная обработка датасета. Датасет: https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023 (на момент загрузки файла данные были с января по август включительно).
my_data <- read_csv ("D:/lessons/R/spotify-2023.csv") # Путь до файла с датасетом. 
# Переименовывание колонок. Оригинальные колонки содержат специальные символы, ломающие код
names(my_data)[1] <- "track_name"
names(my_data)[2] <- "artists_name"
names(my_data)[18] <- "danceability"
names(my_data)[19] <- "valence"
names(my_data)[20] <- "energy"
names(my_data)[21] <- "acoustic"
names(my_data)[22] <- "instrumental"
names(my_data)[23] <- "live"
names(my_data)[24] <- "speech"
# Удаляем колонку Key, так как она содержит много значений N/A
my_data <- subset(my_data, select = -key)

# Убедимся, что все переменные, используемые в будущих моделях, числовые
my_data <- my_data %>%
  mutate(
    in_apple_playlists = as.numeric(in_apple_playlists),
    in_deezer_playlists = as.numeric(in_deezer_playlists),
    in_spotify_playlists = as.numeric(in_spotify_playlists),
    streams = as.numeric(streams)
    )

# Удаляем прочие значения N/A
my_data <- na.omit(my_data)

# Ознакомление со структурой датасета.  Сводка статистических показателей для каждой переменной
head(my_data)
summary(my_data)
glimpse(my_data)
my_data_report <- create_report(my_data) # Создать отчёт с графиками и проч. (долго)

################## ГРАФИКИ ##################

# Диаграмма рассеяния по месту в чартах, количеству плейлистов Spotify и общему кол-ву прослушиваний.

filtered_data <- my_data %>% filter(in_spotify_charts != 0, in_spotify_playlists != 0, streams !=0) # Значения 0 не учитываются.

ggplot(filtered_data, aes(x=streams, y=in_spotify_playlists)) +
  geom_point(aes(color=factor(in_spotify_charts)), alpha=0.7) +
  scale_color_viridis(discrete = TRUE, name="Место в чартах Spotify") +
  labs(title="Диаграмма рассеяния", x="Количество прослушиваний", y="Количество плейлистов в Spotify") +
  theme_minimal() +
  scale_x_continuous(labels = comma_format()) +
  theme(
    panel.background = element_rect(fill = "#212121"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7"),
    axis.text = element_text(color = "#C7C7C7"),
    axis.title = element_text(color = "#C7C7C7"),
    plot.title = element_text(color = "#C7C7C7"),
    panel.grid.minor.x = element_line(color = "#212121", size = 1),
    panel.grid.minor.y = element_line(color = "#212121", size = 1),
    panel.grid.major = element_line(color = "black", size = 1), 
    panel.grid.minor = element_line(color = "black", size = 1),
    legend.key.width = unit(0.4, "mm"),
    legend.key.height = unit(0.4, "mm"),
    legend.margin = unit(c(30, 10, 10, 10), "mm")
  )

# Хитмэп корреляции аудио-характеристик

audio_features <- my_data %>%
  select(danceability, valence, energy, acoustic, instrumental, live, speech)
corr_matrix <- cor(audio_features, use="complete.obs")
melted_corr_matrix <- melt(corr_matrix) # Преобразование матрицы корреляции в формат 'long' с помощью melt из reshape2

ggplot(melted_corr_matrix, aes(Var1, Var2, fill=value)) +
  geom_tile(color = NA) +
  geom_text(aes(label=round(value, 2)), color="white") +
  scale_fill_gradient2(low="#26E61C", mid = "#212121", high="#770A9E", midpoint=0, limit=c(-1,1), space="Lab", name="Корреляция") +
  theme_minimal() +
  labs(title="Корреляция аудио-характеристик") +
  theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1),
        axis.text.y = element_text(size=11)) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7"),
    axis.text = element_text(color = "#C7C7C7"),
    axis.title = element_blank(),  
    plot.title = element_text(color = "#C7C7C7"),
    panel.grid.minor.x = element_line(color = "black", size = 1),
    panel.grid.minor.y = element_line(color = "black", size = 1),
    panel.grid.major = element_line(color = "black", size = 1), 
    panel.grid.minor = element_line(color = "black", size = 1),
  )

# Хитмэп корреляции прослушиваний, чартов и плейлистов

streams_features <- my_data %>% select(streams, in_spotify_playlists, in_apple_playlists, in_deezer_playlists, in_spotify_charts, in_apple_charts, in_deezer_charts, in_shazam_charts)
corr_matrix <- cor(streams_features, use="complete.obs")
melted_corr_matrix <- melt(corr_matrix)

ggplot(melted_corr_matrix, aes(Var1, Var2, fill=value)) +
  geom_tile(color = NA) +
  geom_text(aes(label=round(value, 2)), color="white") +
  scale_fill_gradient2(low="#26E61C", mid = "#212121", high="#770A9E", midpoint=0, limit=c(-1,1), space="Lab", name="Корреляция") +
  theme_minimal() +
  labs(title="Корреляция прослушиваний, чартов и плейлистов") +
  theme(axis.text.x = element_text(angle=45, vjust=1, size=11, hjust=1),
        axis.text.y = element_text(size=11)) +
  theme(
    panel.background = element_rect(fill = "black"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7"),
    axis.text = element_text(color = "#C7C7C7"),
    axis.title = element_blank(),
    plot.title = element_text(color = "#C7C7C7"),
    panel.grid.minor.x = element_line(color = "black", size = 1),
    panel.grid.minor.y = element_line(color = "black", size = 1),
    panel.grid.major = element_line(color = "black", size = 1),
    panel.grid.minor = element_line(color = "black", size = 1),
  )

# Барплот: распределение песен по годам релиза

ggplot(data = my_data, aes(x = released_year)) +
  geom_bar(fill = "#26E61C", color = NA) +
  labs(title = "Распределение песен по годам релиза",
       x = "Год релиза",
       y = "Количество песен") +
  theme(
    panel.background = element_rect(fill = "#212121"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7", size = 17),  # Размер текста
    axis.text = element_text(color = "#C7C7C7", size = 13),  # Размер текста на осях
    axis.title = element_text(color = "#C7C7C7", size = 17),  # Размер текста подписей осей
    plot.title = element_text(color = "#C7C7C7", size = 19),  # Размер текста названия графика
    panel.grid.minor.x = element_line(color = "#212121", size = 1),
    panel.grid.minor.y = element_line(color = "#212121", size = 1),
    panel.grid.major = element_line(color = "black", size = 1),
    panel.grid.minor = element_line(color = "black", size = 1)
  )

# Боксплот: распределение по месяцам релиза

ggplot(data = my_data, aes(x = factor(released_month), y = as.numeric(streams))) +
  geom_boxplot(fill = "#770A9E", color = "#26E61C") +
  labs(title = "Распределение стримов по месяцам релиза",
       x = "Месяц релиза",
       y = "Количество стримов") +
  scale_y_continuous(labels=comma_format()) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#212121"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7", size = 17),
    axis.text = element_text(color = "#C7C7C7", size = 13),
    axis.title = element_text(color = "#C7C7C7", size = 17),
    plot.title = element_text(color = "#C7C7C7", size = 19),
    panel.grid.minor.x = element_line(color = "#212121", size = 1),
    panel.grid.minor.y = element_line(color = "#212121", size = 1),
    panel.grid.major = element_line(color = "black", size = 1),
    panel.grid.minor = element_line(color = "black", size = 1)
  )

# Круговая диаграмма: распределение ладов

mode_summary <- my_data %>%
  group_by(mode) %>%
  summarize(count = n()) # Создаем сводную таблицу по режимам (mode) и подсчитываем количество песен в каждом режиме

p <- ggplot(data = mode_summary, aes(x = "", y = count, fill = mode)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("Major" = "#770A9E", "Minor" = "#26E61C")) +
  labs(title = "Распределение ладов", size = 17) +
  theme_void() +
  theme_minimal() + theme(
    panel.background = element_rect(fill = "#212121"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7"),
    axis.text = element_text(color = "#212121"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "#C7C7C7", size = 19),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 17),
    panel.grid.minor.x = element_line(color = "#212121", size = 1),
    panel.grid.minor.y = element_line(color = "#212121", size = 1),
    panel.grid.major = element_line(color = "#212121", size = 1), 
    panel.grid.minor = element_line(color = "#212121", size = 1)
  )

p + geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black", size = 7) +
  scale_fill_manual(values = c("Major" = "#770A9E", "Minor" = "#26E61C"),
                    labels = c(paste("Major:", mode_summary$count[mode_summary$mode == "Major"]),
                               paste("Minor:", mode_summary$count[mode_summary$mode == "Minor"])), 
                    name = "Лады") # Добавляем цифры на диаграмму и в легенду

################## МОДЕЛИ ##################

# Разделение данных на обучающую и тестовую выборки для будущих моделей
set.seed(123)  # для воспроизводимости результатов
sample_indices <- sample(1:nrow(my_data), 0.7 * nrow(my_data))
train_data <- my_data[sample_indices, ]
test_data <- my_data[-sample_indices, ]

################## Линейная регрессия ##################

# Предсказывает streams по in_apple_playlists, in_deezer_playlists и in_spotify_playlists

# Построение модели на обучающей выборке
model <- lm(streams ~ in_apple_playlists + in_deezer_playlists + in_spotify_playlists, data = train_data)
summary(model)

# Подсчёт MSE, RMSE
mse <- mse(test_data$streams, predictions)
rmse <- sqrt(mse)
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

# Получение предсказаний и остатков на тестовой выборке

# Создание графика предсказаний
predictions <- predict(model, newdata = test_data)

ggplot() +
  geom_point(aes(x = test_data$streams, y = predictions), color = "#770A9E") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#26E61C", linewidth = 1) +
  labs(x = "Действительные значения (кол-во прослушиваний)", y = "Предсказано (кол-во прослушиваний)", title = "Предсказания и действительные значения") +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(labels = comma_format()) +
  theme_minimal() +   theme(
    panel.background = element_rect(fill = "#212121"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7"),
    axis.text = element_text(color = "#C7C7C7"),
    axis.title = element_text(color = "#C7C7C7"),
    plot.title = element_text(color = "#C7C7C7"),
    panel.grid.minor.x = element_line(color = "#212121", linewidth = 1),
    panel.grid.minor.y = element_line(color = "#212121", linewidth = 1),
    panel.grid.major = element_line(color = "black", linewidth = 1),
    panel.grid.minor = element_line(color = "black", linewidth = 1)
  )

# Создание графика остатков
residuals <- residuals(model, newdata = test_data)

ggplot() +
  geom_point(aes(x = predictions, y = test_data$streams - predictions), color = "#770A9E") +
  geom_hline(yintercept = 0, color = "#26E61C", size = 1) +
  labs(x = "Предсказано (кол-во прослушиваний)", y = "Отклонение (остатки)", title = "Отклонение предсказаний") +
  scale_y_continuous(labels = comma_format()) +
  scale_x_continuous(labels = comma_format()) +
  theme_minimal() + theme(
    panel.background = element_rect(fill = "#212121"),
    plot.background = element_rect(fill = "black"),
    text = element_text(color = "#C7C7C7"),
    axis.text = element_text(color = "#C7C7C7"),
    axis.title = element_text(color = "#C7C7C7"),
    plot.title = element_text(color = "#C7C7C7"),
    panel.grid.minor.x = element_line(color = "#212121", size = 1),
    panel.grid.minor.y = element_line(color = "#212121", size = 1),
    panel.grid.major = element_line(color = "black", size = 1),
    panel.grid.minor = element_line(color = "black", size = 1)
  )

################## Древо решений ##################

# Построение одного дерева решений
model_tree <- rpart(streams ~ in_apple_playlists + in_deezer_playlists + in_spotify_playlists, data = train_data)

# Получение предсказаний на тестовой выборке
predictions_tree <- predict(model_tree, newdata = test_data)

# Вычисление MSE и RMSE
mse_tree <- mse(test_data$streams, predictions_tree)
rmse_tree <- sqrt(mse_tree)
cat("Decision Tree - MSE:", mse_tree, "\n")
cat("Decision Tree - RMSE:", rmse_tree, "\n")

# График древа
rpart.plot(model_tree, type = 0, branch.lty = 3, box.col = c("#A06DB3"), split.col = "black")

################## Случайный лес ##################

# Построение модели случайного леса
model_rf <- randomForest(streams ~ in_apple_playlists + in_deezer_playlists + in_spotify_playlists, data = train_data)

# Получение предсказаний на тестовой выборке
predictions_rf <- predict(model_rf, newdata = test_data)

# Вычисление MSE и RMSE
mse_rf <- mse(test_data$streams, predictions_rf)
rmse_rf <- sqrt(mse_rf)
cat("Random Forest - MSE:", mse_rf, "\n")
cat("Random Forest - RMSE:", rmse_rf, "\n")

