#Ильичева П.И.
#ЗАДАНИЕ: создайте модель множественной линейной регрессии потоков углекислого газа за весенний период 2013 года по данным измерений методом турбулентной пульсации
#установка (и проверка) рабочей директории
setwd("D:/polina/Ilicheva/2")
getwd()
#очистим рабочую память
rm(list=ls())
#загрузим пакеты
library("tidyverse")
library("readr")
library("stringr")
library("dplyr")
library("ggplot2")
library("readr")
library("tidyr")
library("tibble")
#сохраним список
alldata = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"),  comment=c("["))
alldata$co2_flux
#удалим ненужную пустую первую строку
alldata = alldata[-1,]
alldata
#посмотрим на сами переменные и для этого воспользуемся функцией glimpse(), которая более наглядно представляет каждую отдельную переменную, 
#опуская при этом представление строчек данных
glimpse(alldata)
#переменная roll содержит только NA, следовательно, будет мешать при анализе, необходимо убрать с помощью функции select()
alldata = select(alldata, -(roll))
#преобразуем в факторы (factor) переменные типа char
alldata = alldata %>% mutate_if(is.character, factor)
#используем функцию str_replace_all, что позволит использовать более простой синтаксис для замены ненужных нам символов
names(alldata) =  str_replace_all(names(alldata), "[!]","_emph_")
#заменим специальные символы в названии стобцов на допустимые для переменных имена
names(alldata) = names(alldata) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
#проверим сами переменные
glimpse(alldata)
#уберем NA
alldata = drop_na(alldata)
#обозначим весенний период
alldata = filter(alldata,DOY >= 60 & DOY < 151)
#выберем все переменные типа numeric
sapply(alldata,is.numeric)
#получим таблицу, состоящую из данных numeric
alldata_numeric = alldata[,sapply(alldata,is.numeric) ]
#получим таблицу, содержащую остальные колонки
alldata_non_numeric = alldata[,!sapply(alldata,is.numeric) ]
#создадим обучающую и тестирующую непересекающиеся выборки
row_numbers = 1:length(alldata_numeric$co2_flux)
teach = sample(row_numbers, floor(length(alldata_numeric$co2_flux)*.7))
test = row_numbers[-teach]
#обучающая выборка
teaching_tbl = alldata_numeric[teach,]
#тестирующая выборка
testing_tbl = alldata_numeric[test,]
#создадим модель 1, добавив в нее все переменные с помощью "(.)" и используя обучающую выборку
mod1 = lm(co2_flux ~ (.), data = teaching_tbl)
#получим информацию о моделе и коэффициенты
summary(mod1)
#проанализируем переменные по значимости
anova(mod1)
#выведем графики
plot(mod1)
#создадим модель 2, добавив в неё значимые переменные из результатов функции anova() (со значимостью до 0.01)
mod2 = lm(co2_flux ~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux + h2o_flux 
          + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density  
          + h2o_mixing_ratio + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume + water_vapor_density +e + es + specific_humidity + Tdew + u_unrot + v_unrot
          + w_unrot + u_rot + v_rot + w_rot + max_speed + wind_dir + yaw + pitch + u_star_ + TKE + L + `_z_minus_d__div_L` + bowen_ratio + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ + x_90_perc_
          + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux + u_spikes + w_spikes + v_var + co2_signal_strength_7200 + co2_signal_strength_7200, data = teaching_tbl)
#получим информацию о полученной модели и коэффициенты
summary(mod2)
#проанализируем переменные по значимости
anova(mod2)
#сравним с предыдущей моделью
anova(mod2, mod1)
#создадим модель 3, добавив в неё значимые переменные из результатов функции anova() (со значимостью до 0.001)
mod3 = lm(co2_flux ~ DOY + Tau + qc_Tau + rand_err_Tau + H + qc_H + rand_err_H + LE + qc_LE + rand_err_LE + qc_co2_flux + rand_err_co2_flux + h2o_flux
          + H_strg + co2_v_minus_adv + h2o_v_minus_adv + co2_molar_density + co2_mole_fraction + co2_mixing_ratio + h2o_molar_density
          + sonic_temperature + air_temperature + air_pressure + air_density + air_heat_capacity + air_molar_volume + water_vapor_density +e + es + specific_humidity + Tdew + u_unrot + v_unrot
          + w_unrot + u_rot + v_rot + w_rot + max_speed + wind_dir + yaw + pitch + TKE + `_z_minus_d__div_L` + bowen_ratio + T_star_ + x_peak + x_offset + x_10_perc_ + x_30_perc_ + x_50_perc_ + x_70_perc_ 
          + un_Tau + Tau_scf + un_H + H_scf + un_LE + LE_scf + un_co2_flux + un_h2o_flux, data = teaching_tbl)
#получим информацио о модели и коэффициенты
summary(mod3)
#проанализируем переменные по значимости
anova(mod3)
#сравним с предыдущей моделью
anova(mod3, mod2)
#выведем графики
plot(mod3)
#проведем корреляционный анализ переменных
cor_td = cor(alldata_numeric)
cor_td
#преобразуем матрицу в таблицу, выберем интересующий столбец и возьмем только имена переменных со значением коэффициента детерминации больше 0,1
cor_td = cor(drop_na(alldata_numeric))
cor_td
cor_td = cor(drop_na(alldata_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
#выберем из таблицы только участвующие в линейной модели переменные 
cor_teaching_tbl = select(teaching_tbl, co2_flux, DOY, Tau, qc_Tau, rand_err_Tau, H, qc_H, rand_err_H, LE, qc_LE, rand_err_LE, qc_co2_flux, rand_err_co2_flux, h2o_flux,
                          H_strg, co2_v_minus_adv, h2o_v_minus_adv, co2_molar_density, co2_mole_fraction, co2_mixing_ratio, h2o_molar_density,
                          sonic_temperature, air_temperature, air_pressure, air_density, air_heat_capacity, air_molar_volume, water_vapor_density, e, es, specific_humidity, Tdew, u_unrot, v_unrot,
                          w_unrot, u_rot, v_rot, w_rot, max_speed, wind_dir, yaw, pitch, TKE, `_z_minus_d__div_L`, bowen_ratio, T_star_, x_peak, x_offset, x_10_perc_, x_30_perc_, x_50_perc_, x_70_perc_, 
                          un_Tau, Tau_scf, un_H, H_scf, un_LE, LE_scf, un_co2_flux, un_h2o_flux)
#получаем таблицу коэффициентов корреляции
cor_td = cor(cor_teaching_tbl) %>% as.data.frame
#строим графики по полученной модели
#проверка модели
#построим точки co2_flux от co2_flux на значениях обучающей выборки и наложим предсказанные значения по модели 3 на обучающей выборке сверху в виде линии
#в идеале линия проходит через все точки. А так как у нас график co2_flux от самого себя, то он должен идти под углом 45 градусов
qplot(co2_flux , co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod3, teaching_tbl)))
#проделаем тоже самое на тестирующей выборке
qplot(co2_flux , co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
#модель зависит от множества переменных, возможно вывести много различных графиков зависимостей co2_flux от учитываемых в модели параметров
#необходимо, чтобы предсказанная линия проходила через все точки, или как можно ближе к ним на тестирующей выборке
#выведем для примера несколько графиков зависимостей co2_flux от параметров на основе тестирующей выборки
qplot(DOY, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(Tau, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))
qplot(co2_flux, co2_flux, data = testing_tbl) + geom_line(aes(y = predict(mod3, testing_tbl)))

