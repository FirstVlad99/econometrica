library(readxl)
library(forecast)
library(ggplot2)

data <- read_excel("data.xls")

# преобразование чисел (если есть запятая)
data$Value <- as.numeric(gsub(",", ".", data$Value))

# временной ряд
ts_data <- ts(
  data$Value,
  start = c(1976, 1),
  frequency = 12
)


#  ГРАФИК РЯДА
autoplot(ts_data) +
  labs(
    title = "Строительств жилья для одной семьи",
    subtitle = "Месячные данные, c 1976",
    x = "Год",
    y = "Строительств"
  )

# ACF
ggAcf(ts_data) +
  labs(
    title = "Автокорреляционная функция (ACF)",
    subtitle = "Проверка зависимости от прошлых значений"
  )

# PACF
ggPacf(ts_data) +
  labs(
    title = "Частная автокорреляционная функция (PACF)",
    subtitle = "Определение порядка AR модели"
  )

# ARIMA модель
model <- auto.arima(ts_data)
summary(model)

#  Диагностика остатков
checkresiduals(model)

# ПРОГНОЗ 3 ШАГА
forecast_model <- forecast(model, h = 3)

forecast_model

# ГРАФИК ПРОГНОЗА
autoplot(forecast_model) +
  geom_line(colour = "blue")
  ggtitle("Строительство жилья для одной семьи") +
  labs(
    subtitle = "Прогноз на 3 месяца вперед",
    x = "Год",
    y = "Значение"
  )
