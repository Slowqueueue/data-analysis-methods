data = read.csv('C:/Users/nikit/Desktop/daily_weather_data.csv')
# Загрузка данных из файла .csv
data <- data[c(310688:312432),]
# Выбираем данные для города Вашингтон
library(splines)
# Подключение библиотеки «splines»
data = na.omit(data)
# Очистка данных от пропущенных значений
attach(data)
# Прикрепеление базы data, для дальнейшего лёгкого использования её столбцов
ste <- 20
# Заданное количество степеней свободы (меняется на 5, 10, 20) по заданию
plot(data.frame(tavg, pres), col="orange")
# Построение диаграммы рассеияния
fit.poly <- lm(pres ~ poly (tavg, ste))
# Подгонка (fit) полиномиальной модели
fit.bs <- lm(pres ~ bs(tavg, df = ste))
#  Подгонка (fit) b-сплайна
fit.ns <- lm(pres ~ ns(tavg, df = ste))
# Подгонка (fit)  естественного сплайна
fit.sp <- smooth.spline(pres ~ tavg, df = ste)
# Подгонка (fit)  сглаживающий сплайн
lines(tavg, predict(fit.poly, data.frame(tavg=tavg)), col=1, lwd=2)
lines(tavg, predict(fit.bs, data.frame(tavg=tavg)), col=2, lwd=2)
lines(tavg, predict(fit.ns, data.frame(tavg=tavg)), col=3, lwd=2)
lines(fit.sp, col=4, lwd=2)
# добавляем линии подгонки к графику