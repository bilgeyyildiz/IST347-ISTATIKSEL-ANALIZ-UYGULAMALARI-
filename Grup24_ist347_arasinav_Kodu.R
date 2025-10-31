#Veriyi yüklemeye yarar. (İmport Dataset kısmından da ekleyebilriz.)
arasinav <- read.table("C:/users/bilge/Downloads/arasinav.txt", header = TRUE, sep = "", stringsAsFactors = FALSE)

# Veriyi çalışma ortamına ekleme
attach(arasinav)

#kullandıgımız veri setindeki gözlemlerden 2000 gözlemi seçerken aynı değerlere sahip gözlemler de seçilebilir.(Rasgelelik içeren işlemlerde)
set.seed(123)

#2000 gözlem seçimi
orneklem <- arasinav[sample(nrow(arasinav), 2000), ]

# Seçilen veriyi dataframe dönüştürme ve kaydetme 
write.table(orneklem, "orneklem.txt", row.names = FALSE, sep = "\t")

# Çalışma ortamından kaldırma
detach(arasinav)

#Bölüm9: Genel İstatistikler.
#B.9.11: Bİr Örnek Oranın Test Edilmesi:

# City_Tier değiişkenindeki Tier_1 kategorisinin sayısını verir
tier_1_count <- sum(orneklem$City_Tier == "Tier_1")

#örneklemedi değişkenin sayısını verir.
n <- nrow(orneklem)  

??prop.test

#oran testi
prop_test_result <- prop.test(tier_1_count, n, p = 0.5, alternative = "two.sided")

#test sonucu
prop_test_result

#B.9.14: Koşular için Test

# Paketi yükleme
install.packages("tseries")

# Paketi çalıştırma
library(tseries)

# Vektör dönüşümü: 'Student' 1, diğerleri 0 olacak şekilde
binary_data <- as.numeric(orneklem$Occupation == "Student")

# Veri yapısını kontrol et
str(binary_data)  # Veri dizisinin yapısını görüntüle
table(binary_data)  # 0 ve 1'lerin sayısını kontrol et

# Koşu Testi yardım dokümanını incele
??runs.test

# Rasgelelik Testi
test_result <- runs.test(as.factor(binary_data))

# Test sonucunu yazdırma
test_result

#9.15: İki Örneğin Ortalamalarının Karşılaştırılması

# Grupları ayır
tier_1_income <- orneklem$Income[orneklem$City_Tier == "Tier_1"]
tier_2_income <- orneklem$Income[orneklem$City_Tier == "Tier_2"]

# Bağımsız t-testi
t_test_result <- t.test(tier_1_income, tier_2_income)

# Sonuçları yazdır
t_test_result

#BAĞIMLI ÖRNEKLEM

# İki harcama kategorisi: "Groceries" ve "Transport"
groceries <- orneklem$Groceries
transport <- orneklem$Transport

# Bağımlı gruplar t-testi
t_test_paired <- t.test(groceries, transport, paired = TRUE)

# Sonuçları yazdırma
t_test_paired



#10: GRAFİKLER:

#10.2.:Başlık ve Etikler Ekleme

#Paket yükleme
install.packages("ggplot2")  
#Paketi Çalıştır
library(ggplot2)

??ggplot2

# Dağılım grafiği oluşturma ve başlık ile eksen etiketlerini ekleme
ggplot(orneklem, aes(x = Age, y = Income)) +
  geom_point(color = "blue") +  # Renklednirme 
  labs(
    title = "Yaşa Göre Gelir Dağılımı",
    x = "Yaş",
    y = "Gelir (USD)"
  )

#10.10: Çubuk Grafiği Oluşturma:

#PAketleri Yükleme
install.packages("dplyr")

??dplyr

#Paketleri Çalıştırma:
library(dplyr)
library(ggplot2)

# Meslek gruplarına göre ortalama eğitim harcaması hesaplama
occupation_education <- orneklem %>%
  group_by(Occupation) %>%
  summarise(mean_education = mean(Education, na.rm = TRUE)) %>%
  arrange(mean_education)

# Çubuk grafik oluşturma
ggplot(occupation_education, aes(x = Occupation, y = mean_education)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(
    title = "Meslek Gruplarına Göre Ortalama Eğitim Harcaması",
    x = "Meslek",
    y = "Ortalama Eğitim Harcaması (USD)"
  )

#10.20.:Histogram Grafiği ve Yoğunluk:

#kütüphaneyi çalıştır
library(ggplot2)

# histogram ve yoğunluk grafiğini 'Groceries' sütunu için çizelim
ggplot(orneklem, aes(x = Groceries)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "green") +
  geom_density(color = "darkblue") +
  labs(title = "Histogram ve Yoğunluk Tahmini - Market Harcamaları", 
       x = "Market Harcamaları", 
       y = "Yoğunluk"
       ) 


#10.22.: Diğer Q-Q Grafikleri Oluşturma:

#paket yükleme:
install.packages("MASS")

# Kütüphaneleri Çağır
library(ggplot2)
library(MASS)

# Veriyi temizle (NA ve negatif değerleri kaldır)
orneklem <- orneklem[!is.na(orneklem$Eating_Out) & orneklem$Eating_Out > 0, ]

# Eating_Out için serbestlik derecesini tahmin et
est_df <- as.list(MASS::fitdistr(orneklem$Eating_Out, "t")$estimate)[["df"]]

# Q-Q grafiğini oluştur
ggplot(orneklem) +
  aes(sample = Eating_Out) +
  geom_qq(distribution = qt, dparams = est_df, color = "blue", size = 2) + # Noktaları mavi
  stat_qq_line(distribution = qt, dparams = est_df, color = "red", linewidth = 1) + # Çizgiyi kırmızı
  labs(
    title = "Student's Dağılımı Q-Q Grafiği - Eating Out",
    x = "Teorik Kantiller",
    y = "Gözlenen Değerler"
  )


#BÖLÜM11:Doğrusal Regresyon ve ANOVA

#11.12.:Dönüştürülen Verilerde Gerileme

# Negatif veya sıfır değerlerden kaçınmak için sadece pozitif verileri seçelim
data <- orneklem[orneklem$Healthcare > 0 & orneklem$Entertainment > 0, ]

# log-log modeli oluştur
model <- lm(log(Healthcare) ~ log(Entertainment), data = orneklem)

# Modelin özetini incele
summary(model)

#grafik için gerekli paket çalıştırılır. 
library(ggplot2)

# Veriyi ve modeli görselleştirme
ggplot(data, aes(x = log(Entertainment), y = log(Healthcare))) +
  geom_point(color = "blue") + # Veri noktaları
  geom_smooth(method = "lm", color = "red") + # Regresyon çizgisi
  labs(
    title = "Log-Log Regresyon: Healthcare ve Entertainment",
    x = "Log(Entertainment)",
    y = "Log(Healthcare)"
  ) 
  


#11.21.: Tek Yönlü ANOVA Gerçekleştirme:

# Tek yönlü ANOVA testi 
anova_result <- oneway.test(Rent ~ Occupation, data = orneklem)

# Test sonucunu yazdır (Eşit Varyans varsayımı olmayan) 
anova_result

# Eşit varyans varsayımıyla testi tekrar gerçekleştirelim
anova_result_equal_var <- oneway.test(Rent ~ Occupation, data = orneklem, var.equal = TRUE)

# Eşit varyans varsayımı ile sonuç
anova_result_equal_var


#11.23.:Grup Ortalamaları Arasındaki Farkları Bulma

# ANOVA modeli oluştur
aov_model <- aov(Rent ~ Occupation, data = orneklem)

# TukeyHSD analizi
tukey_result <- TukeyHSD(aov_model)

# Sonuçları yazdır
tukey_result

# TukeyHSD sonuçlarını görselleştirme
plot(tukey_result, las = 1, col = "blue")


#11.24.: Sağlam ANOVA Performansı (Kruskal-Wallis Testi)


# Kruskal-Wallis testi
kruskal_result <- kruskal.test(Transport ~ City_Tier, data = orneklem)

# Test sonucunu yazdır
kruskal_result


# Boxplot ile grupların dağılımını görselleştirme
boxplot(Transport ~ City_Tier, data = data, 
        main = "Şehir Seviyelerine Göre Ulaşım Harcamaları",
        xlab = "City Tier", ylab = "Transport Harcamaları", 
        col = c("skyblue", "purple4", "lightpink"))
