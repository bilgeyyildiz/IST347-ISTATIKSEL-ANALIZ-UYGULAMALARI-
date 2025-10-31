# Veriyi yükleme (TXT dosyası okuma)
data <- read.table("C:/Users/bilge/OneDrive/Masaüstü/df_grup_24.txt", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# Veri setinin genel boyutunu kontrol etme (Gözlem ve değişken sayısı)
dim(data) 

# İlk 6 satırı gösterme
head(data)

# Veri setinin yapısını görüntüleme 
str(data)

# Eksik değerlerin sayısını her sütun için hesaplama
na_counts <- sapply(data, function(x) sum(is.na(x)))
na_counts


# Değişkenlerin özet istatistikleri 
summary(data)

# Sadece sayısal değişkenleri seçme
numeric_data <- data[sapply(data, is.numeric)]

# Varyansları ve standart sapmaları hesaplama
variances <- sapply(numeric_data, var)
std_devs <- sapply(numeric_data, sd)

# Sonuçları bir tablo olarak birleştirme
results <- data.frame(
  Variable = names(numeric_data),
  Variance = variances,
  Std_Deviation = std_devs
)
results

# Çarpıklık  Hesaplama ve Görselleştirme

library(moments)  # Çarpıklık hesaplamak için
library(ggplot2)  # Görselleştirme için

# Çarpıklık değerlerini hesaplama
skewness_values <- sapply(numerica_data, skewness, na.rm = TRUE)

# Çarpıklık değerlerini bir veri çerçevesine dönüştürme
skewness_df <- data.frame(
  Variable = names(skewness_values),
  Skewness = skewness_values
)

skewness_df

# Çubuk grafiği ve çarpıklık çizgisini çizme
ggplot(skewness_df, aes(x = Variable, y = Skewness)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") + # Çubuk grafiği
  geom_line(aes(group = 1), color = "red", size = 1.2) + # Çarpıklık çizgisi
  geom_point(color = "red", size = 3) + # Çizgi üzerindeki noktalar
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Sıfır çarpıklık çizgisi
  labs(title = "Skewness of Numerical Variables", 
       x = "Variables", 
       y = "Skewness") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#Basıklık Hesaplama ve Görselleştirme
library(moments)  # Basıklık hesaplamak için
library(ggplot2)  # Görselleştirme için

# Basıklık (kurtosis) değerlerini hesaplama
kurtosis_values <- sapply(numeric_data, kurtosis, na.rm = TRUE)

# Basıklık değerlerini bir veri çerçevesine dönüştürme
kurtosis_df <- data.frame(
  Variable = names(kurtosis_values),
  Kurtosis = kurtosis_values
)

kurtosis_df

# Çubuk grafiği ve basıklık çizgisini çizme
ggplot(kurtosis_df, aes(x = Variable, y = Kurtosis)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") + # Çubuk grafiği
  geom_line(aes(group = 1), color = "blue", size = 1.2) + # Basıklık çizgisi
  geom_point(color = "blue", size = 3) + # Çizgi üzerindeki noktalar
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + # Sıfır basıklık çizgisi
  labs(title = "Kurtosis of Numerical Variables", 
       x = "Variables", 
       y = "Kurtosis") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

#1- Maaş ve Gelir Analizi

#I)Maaşlar mesleklere göre nasıl farklılık gösteriyor?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 

# Mesleklere göre maaşların özet istatistikleri
salary_by_job <- data %>%
  group_by(job_title) %>%
  summarise(
    Mean_Salary = mean(salary, na.rm = TRUE),
    Median_Salary = median(salary, na.rm = TRUE),
    Min_Salary = min(salary, na.rm = TRUE),
    Max_Salary = max(salary, na.rm = TRUE),
    Std_Dev_Salary = sd(salary, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Mean_Salary))

# Özet tabloyu yazdırma
salary_by_job

# Mesleklere göre ortalama maaşları gösteren çubuk grafiği

ggplot(salary_by_job, aes(x = reorder(job_title, Mean_Salary), y = Mean_Salary, fill = Mean_Salary)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "İş Unvanlarına Göre Ortalama Maaşlar",
    x = "İş Unvanı",
    y = "Ortalama Maaş (USD)",
    fill = "Ortalama Maaş"
  )

#Dağılımın Normallik Testi  

# Maaş değişkeninin normal dağılıma uygunluğunu test etme

# Anderson-Darling testi (Shapiro-wilk, KS testi  )
ad_test <- ad.test(data$salary)
# Test sonucunu yazdırma
ad_test

#p<0.05 olduğu için normal dağılım sergilemez paramterik olmayan testler kullanılır 

# Levene testi: İş unvanları arasında maaş varyanslarının homojenliğini kontrol etme
levene_test <- leveneTest(salary ~ job_title, data = data)
levene_test

#Levene testinde p<0.05 olduğu için ANOVA varsyaımı sağlanmaz ve Kruksal-Walis testi kullanılır. 

# Kruskal-Wallis testi: mesleklere göre maaş farkları 
kruskal_test <- kruskal.test(salary ~ job_title, data = data)
kruskal_test

# kruksal walis testi p<0.95 çıktı Dunn's Post-Hoc Testi: gruplar arasındaki farkı karşılaştırır.  

dunn_test <- dunnTest(salary ~ job_title, data = data, method = "bonferroni")
dunn_test


#II)Maaşlar, farklı para birimlerinde nasıl bir dağılım göstermektedir?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 

# Maaşların farklı para birimlerine göre dağılımının analizi
salary_distribution_by_currency <- data %>%
  group_by(salary_currency) %>%
  summarise(
    Count = n(),
    Mean_Salary = mean(salary, na.rm = TRUE),
    Std_Dev = sd(salary, na.rm = TRUE),
    Min_Salary = min(salary, na.rm = TRUE),
    Q1 = quantile(salary, 0.25, na.rm = TRUE),
    Median = median(salary, na.rm = TRUE),
    Q3 = quantile(salary, 0.75, na.rm = TRUE),
    Max_Salary = max(salary, na.rm = TRUE)
  )

# Sonuçları yazdırma
salary_distribution_by_currency

# Boxplot ile maaş dağılımını görselleştirme
ggplot(data, aes(x = salary_currency, y = salary, fill = salary_currency)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Maaşların Para Birimlerine Göre Dağılımı",
    x = "Para Birimi",
    y = "Maaş",
    fill = "Para Birimi"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Barplot ile ortalama maaşları görselleştirme
ggplot(salary_distribution_by_currency, aes(x = salary_currency, y = Mean_Salary, fill = salary_currency)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Para Birimlerine Göre Ortalama Maaşlar",
    x = "Para Birimi",
    y = "Ortalama Maaş"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Anderson-Darling testi (Shapiro-wilk, KS testi  )
ad_test <- ad.test(data$salary)
# Test sonucunu yazdırma
ad_test

# Levene Testi: Para birimlerine göre maaş varyanslarının homojenliği
levene_test <- leveneTest(salary ~ salary_currency, data = data)
levene_test
  
# Kruskal-Wallis testi: Para birimlerine göre maaş farkları
kruskal_test <- kruskal.test(salary ~ salary_currency, data = data)
kruskal_test
  
#Gruplar arasındaki farklıklar 

dunn_test <- dunnTest(salary ~ salary_currency, data = data, method = "bonferroni")
dunn_test

#III)Deneyim seviyesi arttıkça maaş artış oranı nasıl değişiyor?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 

# Deneyim seviyesine göre maaşların özet istatistikleri
salary_by_experience <- data %>%
  group_by(experience_level) %>%
  summarise(
    Mean_Salary = mean(salary_in_usd, na.rm = TRUE),
    Median_Salary = median(salary_in_usd, na.rm = TRUE),
    Min_Salary = min(salary_in_usd, na.rm = TRUE),
    Max_Salary = max(salary_in_usd, na.rm = TRUE),
    Std_Dev_Salary = sd(salary_in_usd, na.rm = TRUE),
    Count = n()
  )

# Maaş artış oranını hesaplama (bir önceki seviyeye göre artış)
salary_by_experience <- salary_by_experience %>%
  mutate(
    Salary_Increase = (Mean_Salary - lag(Mean_Salary)) / lag(Mean_Salary) * 100
  )

# Özet tabloyu yazdırma
salary_by_experience

# Deneyim seviyesine göre maaşların çubuk grafiği
ggplot(salary_by_experience, aes(x = experience_level, y = Mean_Salary, fill = experience_level)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Deneyim Seviyesine Göre Ortalama Maaşlar",
    x = "Deneyim Seviyesi",
    y = "Ortalama Maaş (USD)",
    fill = "Deneyim Seviyesi"
  )

# Maaş artış oranını görselleştirme (Çizgi Grafiği)
ggplot(salary_by_experience, aes(x = experience_level, y = Salary_Increase, group = 1)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(
    title = "Deneyim Seviyesi Artışına Göre Maaş Artış Oranı",
    x = "Deneyim Seviyesi",
    y = "Maaş Artış Oranı (%)"
  )

# Anderson-Darling testi (Shapiro-wilk, KS testi  )
ad_test <- ad.test(data$salary)
# Test sonucunu yazdırma
ad_test

# Levene Testi: Maaşların deneyim seviyelerine göre varyans homojenliği
levene_test <- leveneTest(salary ~ experience_level, data = data)
levene_test
  
# Kruskal-Wallis testi: Deneyim seviyesine göre maaş farkları
kruskal_test <- kruskal.test(salary ~ experience_level, data = data)
kruskal_test
  
#Gruplar arasındaki farkların büyüklükleri 
dunn_test <- dunnTest(salary ~ experience_level , data = data, method = "bonferroni")
dunn_test


#IV)Şirket lokasyonlarına göre maaşlar nasıl farklılık gösteriyor?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 
  
# Şirket lokasyonlarına göre maaşların özet istatistikleri
  salary_by_location <- data %>%
    group_by(company_location) %>%
    summarise(
      Mean_Salary = mean(salary_in_usd, na.rm = TRUE),
      Median_Salary = median(salary_in_usd, na.rm = TRUE),
      Min_Salary = min(salary_in_usd, na.rm = TRUE),
      Max_Salary = max(salary_in_usd, na.rm = TRUE),
      Std_Dev_Salary = sd(salary_in_usd, na.rm = TRUE),
      Count = n()
    ) %>%
    arrange(desc(Mean_Salary))
  
# Özet tabloyu yazdırma
salary_by_location
  
# Şirket lokasyonlarına göre maaşların çubuk grafiği
ggplot(salary_by_location, aes(x = reorder(company_location, Mean_Salary), y = Mean_Salary, fill = Mean_Salary)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "Şirket Lokasyonlarına Göre Ortalama Maaşlar",
    x = "Şirket Lokasyonu",
    y = "Ortalama Maaş (USD)",
    fill = "Ortalama Maaş"
  )
  
# Şirket lokasyonlarına göre maaşların kutu grafiği
ggplot(data, aes(x = company_location, y = salary_in_usd, fill = company_location)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "Şirket Lokasyonlarına Göre Maaş Dağılımı",
    x = "Şirket Lokasyonu",
    y = "Maaş (USD)",
    fill = "Şirket Lokasyonu"
  )
# Anderson-Darling testi (Shapiro-wilk, KS testi  )
ad_test <- ad.test(data$salary)
ad_test
  
# Levene Testi: Maaşların lokasyonlara göre varyans homojenliği
levene_test <- leveneTest(salary ~ company_location, data = data)
levene_test
    
# Kruskal-Wallis testi: Şirket lokasyonlarına göre maaş farkları
kruskal_test <- kruskal.test(salary ~ company_location, data = data)
kruskal_test
  
#Gruplar arasındaki farkların büyüklükleri 
dunn_test <- dunnTest(salary ~ company_location , data = data, method = "bonferroni")
dunn_test

#V)	Çalışma türü (tam zamanlı, yarı zamanlı vb.) maaş düzeyini nasıl etkiliyor?
    
# Gerekli kütüphaneleri yükleme
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme

    
# Çalışma türüne göre maaşların özet istatistikleri
salary_by_employment <- data %>%
  group_by(employment_type) %>%
    summarise(
    Mean_Salary = mean(salary, na.rm = TRUE),
    Median_Salary = median(salary, na.rm = TRUE),
    Min_Salary = min(salary, na.rm = TRUE),
    Max_Salary = max(salary, na.rm = TRUE),
    Std_Dev_Salary = sd(salary, na.rm = TRUE),
    Count = n()
  ) %>%
    arrange(desc(Mean_Salary))
    
# Özet tabloyu yazdırma
salary_by_employment
    
# Çalışma türüne göre maaşların çubuk grafiği
ggplot(salary_by_employment, aes(x = reorder(employment_type, Mean_Salary), y = Mean_Salary, fill = Mean_Salary)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Çalışma Türüne Göre Ortalama Maaşlar",
    x = "Çalışma Türü",
    y = "Ortalama Maaş ",
    fill = "Ortalama Maaş"
  )
    
# Çalışma türüne göre maaşların kutu grafiği
ggplot(data, aes(x = employment_type, y = salary, fill = employment_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Çalışma Türüne Göre Maaş Dağılımı",
    x = "Çalışma Türü",
    y = "Maaş",
    fill = "Çalışma Türü"
  )
    
# Levene Testi: Çalışma türlerine göre maaş varyanslarının eşitliği
levene_test <- leveneTest(salary ~ employment_type, data = data)
levene_test

# ANOVA testi: Çalışma türlerinin maaşlara etkisi
anova_result <- aov(salary ~ employment_type, data = data)
summary(anova_result)
    
    
#VI) Maaşlar yıllara göre nasıl farklılık gösteriyor?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 

    
# Yıllara göre maaşların özet istatistikleri
salary_by_year <- data %>%
  group_by(work_year) %>%
  summarise(
  Mean_Salary = mean(salary, na.rm = TRUE),
  Median_Salary = median(salary, na.rm = TRUE),
  Min_Salary = min(salary, na.rm = TRUE),
  Max_Salary = max(salary, na.rm = TRUE),
  Std_Dev_Salary = sd(salary, na.rm = TRUE),
  Count = n()
  )
    
# Özet tabloyu yazdırma
salary_by_year
    
# Yıllara göre maaşların çizgi grafiği
ggplot(salary_by_year, aes(x = work_year, y = Mean_Salary, group = 1)) +
  geom_line(size = 1, color = "blue") +
  geom_point(size = 3, color = "red") +
  theme_minimal() +
  labs(
    title = "Yıllara Göre Ortalama Maaşlar",
    x = "Yıl",
    y = "Ortalama Maaş "
  )
    
# Yıllara göre maaşların kutu grafiği
ggplot(data, aes(x = as.factor(work_year), y = salary, fill = as.factor(work_year))) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Yıllara Göre Maaş Dağılımı",
    x = "Yıl",
    y = "Maaş",
    fill = "Yıl"
  )
    
# Anderson-Darling testi (Shapiro-wilk, KS testi  )
ad_test <- ad.test(data$salary)
ad_test

# Levene Testi: Maaşların yıllara göre varyans homojenliği
levene_test <- leveneTest(salary ~ as.factor(work_year), data = data)
levene_test
    
   
# Kruskal-Wallis testi: Yıllara göre maaş farkları
kruskal_test <- kruskal.test(salary ~ as.factor(work_year), data = data)
kruskal_test
      
#Gruplar arasındaki farkların büyüklükleri 
dunn_test <- dunnTest(salary ~ work_year , data = data, method = "bonferroni")
dunn_test

#Buradaki 2 değişken de sayısal olduğun için korelasyon ve resgresyon yapabiliriz 
 #Korelasyon 

# Gerekli kütüphaneleri yükleme
library(ggplot2)

# Korelasyon  hesaplama
correlation <- cor(data$work_year, data$salary, method = "spearman")
correlation 

# Regresyon Modeli Kurulumu
regression_model <- lm(salary ~ work_year, data = data)
# Modelin Özeti
summary(regression_model)

# Regresyon  ve Korelsayon Grafiği
library(ggplot2)

ggplot(data, aes(x = work_year, y = salary)) +
  geom_point(color = "blue", size = 3) + # Veri noktaları
  geom_smooth(method = "lm", color = "red", se = TRUE) + # Regresyon çizgisi
  theme_minimal() +
  labs(
    title = "Çalışma Yılı ile Maaş Arasındaki Regresyon ve Korelasyon",
    x = "Çalışma Yılı",
    y = "Maaş"
  )


#B)Uzaktan Çalışma ve Çalışma Modelleri

#I) Uzaktan çalışmaya en yatkın Meslekler hangileridir?
# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 
    
# Mesleklere göre uzaktan çalışma oranlarının özet istatistikleri
remote_by_job <- data %>%
  group_by(job_title) %>%
  summarise(
    Mean_Remote = mean(remote_ratio, na.rm = TRUE),
    Median_Remote = median(remote_ratio, na.rm = TRUE),
    Min_Remote = min(remote_ratio, na.rm = TRUE),
    Max_Remote = max(remote_ratio, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Mean_Remote))
# Özet tabloyu yazdırma
remote_by_job
    
# Mesleklere göre uzaktan çalışma oranlarının çubuk grafiği 
ggplot(remote_by_job, aes(x = reorder(job_title, Mean_Remote), y = Mean_Remote, fill = Mean_Remote)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Tek renk gradyan
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "MEsleklere Göre Ortalama Uzaktan Çalışma Oranları",
    x = "Meslekler ",
    y = "Ortalama Uzaktan Çalışma Oranı (%)",
    fill = "Uzaktan Çalışma Oranı"
  )

# Anderson-Darling testi (Shapiro-wilk, KS testi  )
ad_test <- ad.test(data$remote_ratio)
ad_test
    
# Levene Testi: Mesleklere uzaktan çalışma oranlarının varyans homojenliği
levene_test <- leveneTest(remote_ratio ~ job_title, data = data)
levene_test

# Kruskal-Wallis testi: Mesleklere göre uzaktan çalışma oranı farkları
kruskal_test <- kruskal.test(remote_ratio ~ job_title, data = data)
kruskal_test

#Gruplar arasındaki farkların büyüklükleri 
dunn_test <- dunnTest(remote_ratio ~ job_title , data = data, method = "bonferroni")
dunn_test  
  

#II) Uzaktan çalışma oranı, coğrafi bölgelere göre nasıl farklılık gösteriyor?

# Gerekli kütüphaneleri yükleme
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme

# Coğrafi bölgelere göre uzaktan çalışma oranının özet istatistikleri
location_remote_summary <- data %>%
  group_by(company_location) %>%
  summarise(
    Mean_Remote = mean(remote_ratio, na.rm = TRUE),
    Median_Remote = median(remote_ratio, na.rm = TRUE),
    Std_Dev_Remote = sd(remote_ratio, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Mean_Remote))  # Uzaktan çalışma oranına göre sıralama

# Özet tabloyu yazdırma
location_remote_summary

# Coğrafi bölgelere göre uzaktan çalışma oranı için çubuk grafiği
ggplot(location_remote_summary, aes(x = reorder(company_location, Mean_Remote), y = Mean_Remote, fill = Mean_Remote)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) +
  labs(
    title = "Coğrafi Bölgelere Göre Uzaktan Çalışma Oranı",
    x = "Coğrafi Bölge",
    y = "Ortalama Uzaktan Çalışma Oranı (%)",
    fill = "Ortalama Oran"
  )

# Levene Testi: Coğrafi bölgelere göre uzaktan çalışma oranlarının varyans homojenliği
levene_test <- leveneTest(remote_ratio ~ company_location, data = data)
levene_test
  
# ANOVA testi: Coğrafi bölgelerin uzaktan çalışma oranlarına etkisi
anova_result <- aov(remote_ratio ~ company_location, data = data)
summary(anova_result)

# Tukey HSD testi
tukey_result <- TukeyHSD(anova_result)
tukey_result


#III) Şirket boyutu uzaktan çalışma oranlarını nasıl etkiliyor? 

# Gerekli kütüphaneleri yükleme
library(dplyr)    # Veri manipülasyonu için
library(ggplot2)  # Görselleştirme için

# Şirket boyutlarına göre uzaktan çalışma oranının özet istatistikleri
size_remote_summary <- data %>%
  group_by(company_size) %>%
  summarise(
    Mean_Remote = mean(remote_ratio, na.rm = TRUE),
    Median_Remote = median(remote_ratio, na.rm = TRUE),
    Std_Dev_Remote = sd(remote_ratio, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Mean_Remote))  # Uzaktan çalışma oranına göre sıralama

# Özet tabloyu yazdırma
size_remote_summary

# Şirket boyutlarına göre uzaktan çalışma oranı için çubuk grafiği
ggplot(size_remote_summary, aes(x = reorder(company_size, Mean_Remote), y = Mean_Remote, fill = Mean_Remote)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Şirket Boyutuna Göre Ortalama Uzaktan Çalışma Oranı",
    x = "Şirket Boyutu",
    y = "Ortalama Uzaktan Çalışma Oranı (%)",
    fill = "Ortalama Oran"
  )

#Uzaktan Çalışma Oranını Belirli Kategorilere Ayırma
data$remote_ratio_category <- ifelse(data$remote_ratio == 0, "0",
                                     ifelse(data$remote_ratio == 50, "50",
                                      ifelse(data$remote_ratio == 100, "100", NA))
                                     )
# Çapraz tablo oluşturma
cross_tab <- table(data$company_size, data$remote_ratio_category)
# Çapraz tabloyu yazdırma
cross_tab

# Çapraz tabloyu veri çerçevesine dönüştürme
cross_tab_df <- as.data.frame(cross_tab)
# Görselleştirme
ggplot(cross_tab_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 4) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Şirket Boyutları ve Uzaktan Çalışma Oranları",
    x = "Şirket Boyutu",
    y = "Uzaktan Çalışma Oranı",
    fill = "Gözlem Sayısı"
  ) +
  theme_minimal()

#Khi-Kare Testi
chisq_test <- chisq.test(cross_tab)
chisq_test

# S ve M şirket boyutlarını, 50 ve 100 uzaktan çalışma oranlarına göre filtreleme
filtered_data <- data %>%
filter(company_size %in% c("L", "M") & remote_ratio %in% c(0, 100))

# Çapraz tabloyu oluşturma
fisher_cross_tab <- table(filtered_data$company_size, filtered_data$remote_ratio)

# Çapraz tabloyu yazdırma
fisher_cross_tab

#Fisher's Exact Test Uygulama
fisher_test <- fisher.test(fisher_cross_tab)
fisher_test


#C) Şirket ve Lokasyon Analizleri:
#I)	Şirket boyutunun maaşlar üzerindeki etkisi lokasyona göre farklı mı?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 

# Şirket boyutu ve lokasyona göre maaşların özet istatistikleri
size_location_salary_summary <- data %>%
  group_by(company_location, company_size) %>%
  summarise(
    Mean_Salary = mean(salary_in_usd, na.rm = TRUE),
    Median_Salary = median(salary_in_usd, na.rm = TRUE),
    Std_Dev_Salary = sd(salary_in_usd, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(desc(Mean_Salary))

# Özet tabloyu yazdırma
size_location_salary_summary

# Şirket boyutu ve lokasyona göre maaşların çubuk grafiği
ggplot(size_location_salary_summary, aes(x = company_size, y = Mean_Salary, fill = company_size)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ company_location) +
  theme_minimal() +
  labs(
    title = "Şirket Boyutunun Maaşlar Üzerindeki Etkisi Lokasyona Göre",
    x = "Şirket Boyutu",
    y = "Ortalama Maaş (USD)",
    fill = "Şirket Boyutu"
  )

#Regresyon Modeli Oluşturma:

regression_model <- lm(salary ~ company_size * company_location, data = data)
regression_model

#II.) Maaşlar yıllar içinde hangi bölgelerde daha hızlı artıyor?

# Gerekli kütüphaneleri yükleme
library(nortest)  #Andersan Darling testi için 
library(car)      #Levene Testi 
library(dplyr)    # Veri manipülasyonu
library(ggplot2)  # Görselleştirme
library(FSA)      #Dunn Testi için 


# Yıllar ve bölgeler bazında maaşların özet istatistikleri
yearly_salary_by_region <- data %>%
  group_by(work_year, employee_residence) %>%
  summarise(
    Mean_Salary = mean(salary_in_usd, na.rm = TRUE),
    Median_Salary = median(salary_in_usd, na.rm = TRUE),
    Count = n()
  ) %>%
  arrange(employee_residence, work_year)

# Özet tabloyu yazdırma
yearly_salary_by_region

# Maaş trendini görselleştirme (Çizgi Grafiği)
ggplot(yearly_salary_by_region, aes(x = work_year, y = Mean_Salary, color = employee_residence, group = employee_residence)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(
    title = "Yıllar İçinde Bölgelere Göre Maaş Trendleri",
    x = "Yıl",
    y = "Ortalama Maaş (USD)",
    color = "Bölge"
  )

# Yıllık maaş artış oranını hesaplama
yearly_growth <- yearly_salary_by_region %>%
  group_by(employee_residence) %>%
  mutate(
    Growth_Rate = (Mean_Salary - lag(Mean_Salary)) / lag(Mean_Salary) * 100
  ) %>%
  filter(!is.na(Growth_Rate))  # NA değerleri kaldırma

# Maaş artış oranı tablosunu yazdırma
yearly_growth

#Maaş artış oranını görselleştirme (Bar Grafiği)
ggplot(yearly_growth, aes(x = work_year, y = Growth_Rate, fill = employee_residence)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Yıllık Maaş Artış Oranı Bölgelere Göre",
    x = "Yıl",
    y = "Maaş Artış Oranı (%)",
    fill = "Bölge"
  )

# Maaş artış oranı verisini hazırlama
growth_rate <- yearly_growth$Growth_Rate
# Anderson-Darling testi uygulama
ad_test <- ad.test(growth_rate)
# Test sonucunu yazdırma
ad_test

# work_year değişkenini faktöre dönüştürme
data$work_year <- as.factor(data$work_year)
# Levene Testi uygulama
levene_test <- leveneTest(salary ~ company_location * work_year, data = data)
levene_test

# Kruskal-Wallis testi: Bölgelere göre yıllık maaş farkları
kruskal_test <- kruskal.test(salary ~ interaction(company_location, work_year), data = data)
kruskal_test
  
#Grauplar arası farklıklar için Dunn Testi
dunn_test <- dunnTest(salary ~ interaction(company_location, work_year), data = data, method = "bonferroni")
dunn_test

#6. Korelasyon 

# Gerekli paketi kurma ve yükleme
library(corrplot)
library(ggcorrplot)

# Veri setinden sayısal değişkenleri seçme
numeric_data <- data %>%
  select_if(is.numeric)

# Korelasyon matrisi oluşturma
correlation_matrix <- cor(numeric_data, use = "complete.obs")  # NA değerler kaldırılır

# Korelasyon matrisini yazdırma
correlation_matrix

# Korelasyon matrisini görselleştirme
ggcorrplot(correlation_matrix, 
           method = "circle", 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           colors = c("red", "white", "blue"), 
           title = "Korelasyon Matrisi", 
           ggtheme = theme_minimal()
           )


#7.) Regresyon 
#Regresyon Modelleri:
library(ggplot2)  #  görselleştirme için
#A. Basit Doğrusal Regresyon

# Basit doğrusal regresyon modeli
model_simple <- lm(salary_in_usd ~ remote_ratio, data = data)

# Model özetini yazdırma
summary(model_simple)

model_simple

# Artık değerleri ve tahmin edilen değerleri hesaplama
data$model_fitted <- fitted(model_simple)
data$model_residuals <- resid(model_simple)

# Artık grafiği (Tahmin edilen değerler vs Artıklar)
ggplot(data, aes(x = model_fitted, y = model_residuals)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Tahmin Edilen Değerler vs Artıklar",
    x = "Tahmin Edilen Değerler",
    y = "Artıklar"
  )

# Artık yoğunluk grafiği
ggplot(data, aes(x = model_residuals)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Artıkların Yoğunluk Grafiği",
    x = "Artıklar",
    y = "Yoğunluk"
  )

#B. Çoklu Doğrusal Regresyon

# Çoklu doğrusal regresyon modeli
model_multiple <- lm(salary_in_usd ~ remote_ratio + experience_level + company_size, data = data)

# Model özetini yazdırma
summary(model_multiple)

# Artık değerleri ve tahmin edilen değerleri hesaplama
data$model_fitted <- fitted(model_multiple)
data$model_residuals <- resid(model_multiple)

# Artık grafiği
ggplot(data, aes(x = model_fitted, y = model_residuals)) +
  geom_point(color = "steelblue", alpha = 0.6, size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(
    title = "Tahmin Edilen Değerler vs Artıklar",
    x = "Tahmin Edilen Değerler",
    y = "Artıklar"
  )
# Artık yoğunluk grafiği
ggplot(data, aes(x = model_residuals)) +
  geom_density(fill = "lightblue", alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Artıkların Yoğunluk Grafiği",
    x = "Artıklar",
    y = "Yoğunluk"
  )





