#veri setinin sayfaya dahil edilmesi
veri = read.table(file.choose(), header = TRUE, sep=",")

#ke??ifsel veri analizi
head(veri, 3)
tail(veri, 3)
dim(veri)
anyNA(veri)
str(veri)
summary(veri)
attributes(veri)
View(veri)
names(veri)
min(veri$Age)
max(veri$Age)

#pivot tablo yapiliyor
library(dplyr)
library(tidyr)
veri %>%
  group_by(Gender, Showed_up) %>%
  summarise(mean_Age = mean(Age, na.rm = TRUE)) %>%
  spread(key = Showed_up, value = mean_Age)

#istatistiksel ??zetin daha detayli g??sterimi
library(psych)
describe(veri)



veri %>%
  group_by(Gender) %>%
  summarise(
    count = n(),
    mean = mean(Showed_up, na.rm = TRUE),
    sd = sd(Showed_up, na.rm = TRUE),
    min = min(Showed_up, na.rm = TRUE),
    max = max(Showed_up, na.rm = TRUE),
    median = median(Showed_up, na.rm = TRUE),
    q25 = quantile(Showed_up, 0.25, na.rm = TRUE),
    q75 = quantile(Showed_up, 0.75, na.rm = TRUE)
  ) %>%
  t()


#tekrar eden satir olup olmadigini kontrol edelim
sum(duplicated(veri))



# ??apraz tablolar?? olu??turma ve sonu??lar?? yazd??rma
diabetes <- table(veri$Showed_up, veri$Diabetes)
hipertension <- table(veri$Showed_up, veri$Hipertension)
alcohol <- table(veri$Showed_up, veri$Alcoholism)
handcap <- table(veri$Showed_up, veri$Handcap)

# Toplamlar?? (margin) ekleme
diabetes <- addmargins(diabetes)
hipertension <- addmargins(hipertension)
alcohol <- addmargins(alcohol)
handcap <- addmargins(handcap)

# Sonu??lar?? yazd??rma
print(diabetes)
cat("\n\n")
print(hipertension)
cat("\n\n")
print(alcohol)
cat("\n\n")
print(handcap)




library(ggplot2)
library(gridExtra)
veriler <- data.frame(
  Showed_up = sample(c("Yes", "No"), 100, replace = TRUE),
  Diabetes = sample(c(TRUE, FALSE), 100, replace = TRUE),
  Hipertension = sample(c(TRUE, FALSE), 100, replace = TRUE),
  Alcoholism = sample(c(TRUE, FALSE), 100, replace = TRUE),
  Handcap = sample(c(TRUE, FALSE), 100, replace = TRUE)
)

# ??apraz tablolardan pie chart i??in verilerin haz??rlanmas??
diabetes_table <- table(veriler$Showed_up, veriler$Diabetes)
hipertension_table <- table(veriler$Showed_up, veriler$Hipertension)
alcohol_table <- table(veriler$Showed_up, veriler$Alcoholism)
handcap_table <- table(veriler$Showed_up, veriler$Handcap)

# Pastalar i??in fonksiyon
pie_chart <- function(data, labels, title, colors) {
  ggplot(data.frame(data), aes(x = "", y = Freq, fill = Var2)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    scale_fill_manual(values = colors) +
    theme_void() +
    theme(legend.title = element_blank()) +
    labs(title = title, fill = labels) +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_text(aes(label = scales::percent(Freq / sum(Freq))), position = position_stack(vjust = 0.5))
}

# Grafikler
p1 <- pie_chart(diabetes_table, c("Randevuya Kat??ld??", "Randevuya Kat??lmad??"), "Diyabeti Olanlar??n Randevu Kat??l??m??", c("#4CAF50", "#FF6347"))
p2 <- pie_chart(hipertension_table, c("Randevuya Kat??ld??", "Randevuya Kat??lmad??"), "Hipertansiyonu Olanlar??n Randevu Kat??l??m??", c("#6495ED", "#FF6347"))
p3 <- pie_chart(alcohol_table, c("Randevuya Kat??ld??", "Randevuya Kat??lmad??"), "Alkol Sorunu Olanlar??n Randevu Kat??l??m??", c("#FFD700", "#FF6347"))
p4 <- pie_chart(handcap_table, c("Randevuya Kat??ld??", "Randevuya Kat??lmad??"), "Engel Durumu Olanlar??n Randevu Kat??l??m??", c("#898121", "#FF6347"))

# Grafiklerin d??zenlenmesi ve g??sterilmesi
grid.arrange(p1, p2, p3, p4, ncol = 2)







verim <- data.frame(
  Showed_up = sample(c("Yes", "No"), 100, replace = TRUE),
  SMS_received = sample(0:3, 100, replace = TRUE)
)

# 'Showed_up' ile gruplayarak 'SMS_received' s??tununun toplam??n?? al??yoruz
sms_data <- verim %>%
  group_by(Showed_up) %>%
  summarise(Total_SMS = sum(SMS_received))

# ??ubuk grafi??i olu??turuyoruz
ggplot(sms_data, aes(x = Showed_up, y = Total_SMS, fill = Showed_up)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#4CAF50", "#FF6347")) +
  labs(
    title = "Randevu Kat??l??m Durumuna G??re Al??nan SMS Say??s??",
    x = "Randevu Kat??l??m Durumu",
    y = "Toplam SMS Say??s??"
  ) +
  scale_x_discrete(labels = c("No" = "Kat??lmad??", "Yes" = "Kat??ld??")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )





neighbourhood_data <- veri %>%
  group_by(Neighbourhood) %>%
  summarise(Total_Showed_up = sum(Showed_up)) %>%
  arrange(desc(Total_Showed_up))  # En y??ksek kat??l??m?? s??ral??yoruz

# ??lk 10 mahalleyi se??iyoruz
top_neighbourhoods <- head(neighbourhood_data, 10)

# Bar grafi??i ??iziyoruz
ggplot(top_neighbourhoods, aes(x = reorder(Neighbourhood, Total_Showed_up), y = Total_Showed_up, fill = Total_Showed_up)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_gradient(low = "#FF6347", high = "#4CAF50") + # Renk skalas??n?? s??rekli olarak ayarl??yoruz
  labs(
    title = "En Y??ksek Randevu Kat??l??m??na Sahip ??lk 10 Mahalle",
    x = "Mahalle",
    y = "Toplam Randevu Kat??l??m Say??s??"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )






neighbourhood_data <- veri %>%
  group_by(Neighbourhood) %>%
  summarise(Total_Showed_up = sum(Showed_up)) %>%
  arrange(Total_Showed_up)  # En d??????k kat??l??m?? s??ral??yoruz

# En d??????k kat??l??m?? olan ilk 10 mahalleyi se??iyoruz
bottom_neighbourhoods <- head(neighbourhood_data, 10)

# Bar grafi??i ??iziyoruz
ggplot(bottom_neighbourhoods, aes(x = reorder(Neighbourhood, Total_Showed_up), y = Total_Showed_up, fill = Total_Showed_up)) +
  geom_bar(stat = "identity", show.legend = FALSE, fill = "#FF6347") +  # Renk k??rm??z??
  labs(
    title = "En D??????k Randevu Kat??l??m??na Sahip ??lk 10 Mahalle",
    x = "Mahalle",
    y = "Toplam Randevu Kat??l??m Say??s??"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  ) +
  ylim(0, max(bottom_neighbourhoods$Total_Showed_up) + 5)  # Y eksenini s??n??rl??yoruz





# 1. Grafik - Hipertansiyon ile ya?? ve randevu kat??l??m??
p1 <- ggplot(veri, aes(x = Showed_up, y = Age, color = factor(Hipertension))) +
  geom_line() +
  labs(title = "Hipertansiyonu Olanlar??n Ya?? ve Randevu Kat??l??m??", x = "Randevu Kat??l??m Durumu", y = "Ya??") +
  theme_minimal()

# 2. Grafik - Diyabet ile ya?? ve randevu kat??l??m??
p2 <- ggplot(veri, aes(x = Showed_up, y = Age, color = factor(Diabetes))) +
  geom_line() +
  labs(title = "Diyabeti Olanlar??n Ya?? ve Randevu Kat??l??m??", x = "Randevu Kat??l??m Durumu", y = "Ya??") +
  theme_minimal()

# 3. Grafik - Alkol sorunu olanlar ile ya?? ve randevu kat??l??m??
p3 <- ggplot(veri, aes(x = Showed_up, y = Age, color = factor(Alcoholism))) +
  geom_line() +
  labs(title = "Alkol Sorunu Olanlar??n Ya?? ve Randevu Kat??l??m??", x = "Randevu Kat??l??m Durumu", y = "Ya??") +
  theme_minimal()

# 4. Grafik - Engel durumu olanlar ile ya?? ve randevu kat??l??m??
p4 <- ggplot(veri, aes(x = Showed_up, y = Age, color = factor(Handcap))) +
  geom_line() +
  labs(title = "Engel Durumu Olanlar??n Ya?? ve Randevu Kat??l??m??", x = "Randevu Kat??l??m Durumu", y = "Ya??") +
  theme_minimal()

# Grafiklerin alt alta s??ralanmas??n?? sa??lamak i??in gridExtra paketini kullanabiliriz
library(gridExtra)

# Grafiklerin birle??tirilmesi
grid.arrange(p1, p2, p3, p4, ncol = 2)




#H??POTEZ TESTLER??
#1. H??POTEZ
#Randevuya gelip gelmeme ile ya?? aras??ndaki ili??ki; 
#Hipotez: Ya?? de??i??keni ile randevu durumu aras??nda bir ili??ki durumu. 
#H0: Ya?? de??i??keni ile randevu durumu aras??nda anlaml?? bir ili??ki yoktur. 
#H1: Ya?? de??i??keni ile randevu durumu aras??nda anlaml?? bir ili??ki vard??r. 
#Test: Kolmogorov-Smirnov Testi / Mann-Whitney U Testi

age_showed_up <- veri$Age[veri$Showed_up == 1]
age_no_show <- veri$Age[veri$Showed_up == 0]


ks_test_showed_up <- ks.test(age_showed_up, "pnorm", mean(age_showed_up), sd(age_showed_up))
ks_test_no_show <- ks.test(age_no_show, "pnorm", mean(age_no_show), sd(age_no_show))

cat("Kolmogorov-Smirnov Testi (Showed Up): p-value =", ks_test_showed_up$p.value, "\n")
cat("Kolmogorov-Smirnov Testi (No Show): p-value =", ks_test_no_show$p.value, "\n")

#Kolmogorov-Smirnov Testi sonu??lar??na g??re, hem randevuya gelenler 
#(Showed Up) hem de randevuya gelmeyenler (No Show) gruplar??n??n ya?? da????l??mlar?? 
#i??in elde edilen p-de??erleri (1.016394e-237 ve 6.559586e-76) son derece k??????kt??r. 
#Bu, her iki grup i??in de normal da????l??ma uymad??klar?? anlam??na gelir, 
#????nk?? p-de??eri 0.05'ten ??ok daha k??????k. Dolay??s??yla, her iki grup i??in de ya??lar??n 
#normal da????l??mdan anlaml?? derecede farkl?? oldu??u s??ylenebilir.

install.packages("nortest")
library(nortest)
ad.test(age_showed_up)
ad.test(age_no_show)



# Mann-Whitney U Testi (Wilcoxon Rank Sum Test) uygulama
mann_whitney_result <- wilcox.test(age_showed_up, age_no_show)

# Sonu??lar?? yazd??rma
cat("Mann-Whitney U Test: Test Statistic =", mann_whitney_result$statistic, 
    ", p-value =", mann_whitney_result$p.value, "\n")


#Mann-Whitney U testi sonu??lar??na g??re, randevuya gelenler ve gelmeyenler 
#aras??ndaki ya??lar aras??nda anlaml?? bir fark bulunmaktad??r (p-value < 0.05). 
#Test istatisti??i (U = 1,015,591,109) olduk??a y??ksek bir de??ere sahip, bu da gruplar 
#aras??ndaki fark??n b??y??k oldu??unu g??steriyor. Bu sonu??, randevuya gelme durumu ile 
#ya??lar aras??nda istatistiksel olarak anlaml?? bir ili??ki oldu??unu ortaya koymaktad??r.
#H0 hipotezi reddedilmi??tir.




#2. H??POTEZ
#Cinsiyet ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Cinsiyet ile randevuya gelip gelmeme aras??ndaki ili??ki.
#H0: Hastan??n cinsiyeti ile randevu durumu aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastan??n cinyiset durumu ile randevuya gelme yada gelmeme aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi / Logistik Regresyon




# Frekans tablosu olu??turma
contingency_table <- table(veri$Gender, veri$Showed_up)
# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

cat("Ki-Kare De??eri: ", chi2_test$statistic, "\n")
cat("P-de??eri: ", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi: ", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu: \n")
print(chi2_test$expected)

# Sonu??lar?? yorumlama
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Cinsiyet ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Cinsiyet ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}




#3.Hipotez Testi
#Mahalle ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Randevuya gelip gelmeme durumu ile mahalle bilgisi aras??ndaki ili??ki.
#H0: Hastan??n randevu durumu ile mahalle bilgisi aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastan??n randevu durumu ile mahalle bilgisi aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi
library(MASS)
contingency_table <- table(veri$Neighbourhood, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Mahalle bilgisi ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Mahalle bilgisi ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}


#4. Hipotez Testi
#Hipertansiyon ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Hastan??n hipertansiyon olmas?? ile randevu durumu ili??kisi.
#H0: Hastan??n randevuya gelip gelmeme durumu ile hipertansiyon hastas?? olma aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastan??n randevuya gelip gelmeme durumu ile hipertansiyon hastas?? olma aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi

contingency_table <- table(veri$Hipertension, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Hipertansiyon ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Hipertansiyon ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}



#5. Hipotez testi
#Diyabet ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Hastan??n diyabet olmas?? ile randevu durumu ili??kisi.
#H0: Hastan??n randevuya gelip gelmeme durumu ile diyabet hastas?? olma aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastan??n randevuya gelip gelmeme durumu ile diyabet hastas?? olma aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi

contingency_table <- table(veri$Diabetes, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Diyabet ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Diyabet ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}



#6. Hipotez Testi
#Alkol Ba????ml??l?????? ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Hastan??n alkol ba????ml??s?? olmas?? ile randevu durumu ili??kisi.
#H0: Hastan??n randevuya gelip gelmeme durumu ile alkol ba????ml??s?? olma aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastan??n randevuya gelip gelmeme durumu ile alkol ba????ml??s?? olma aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi / Logistik Regresyon

contingency_table <- table(veri$Alcoholism, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Alkol ba????ml??l?????? ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Alkol ba????ml??l?????? ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}



#7. Hipotez
#Engel durumu ile randevu gelip gelmeme ili??kisi;
#Hipotez: Hastan??n engel durumunun bulunmas?? ile randevuya gelip gelmeme aras??ndaki ili??ki.
#H0: Hastan??n engel durumu ile randevuya gelip gelmeme aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastan??n engel durumu ile randevuya gelip gelmeme aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi

contingency_table <- table(veri$Handcap, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Hastan??n engel durumu ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Hastan??n engel durumu ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}



#8. Hipotez
#Hastan??n sa??l??k yard??m?? almas?? ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Hastan??n randevu durumu ile sa??l??k yard??m?? ili??kisi.
#H0: Hastan??n sa??l??k yard??m?? almas?? ile randevuya gelip gelmemesi aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hast??n??n sa??l??k yard??m?? almas?? ile randevuya gelip gelmemesi aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi

contingency_table <- table(veri$Scholarship, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Hastan??n sa??l??k bursu almas?? durumu ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Hastan??n sa??l??k bursu almas?? durumu ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}


#9.Hipotez
#SMS hat??rlatmas?? ile randevu durumu aras??ndaki ili??ki;
#Hipotez: Hastaya randevu ??ncesinde gelen SMS hat??rlatmas?? ile randevuya gelip gelmeme aras??ndaki ili??ki.
#H1: Hastaya gelen SMS hat??rlatmas?? ile randevu durumu aras??nda anlaml?? bir ili??ki yoktur.
#H1: Hastaya gelen SMS hat??rlatmas?? ile randevu durumu aras??nda anlaml?? bir ili??ki vard??r.
#Test: Ki-Kare Testi

contingency_table <- table(veri$SMS_received, veri$Showed_up)

# Ki-Kare testi
chi2_test <- chisq.test(contingency_table)

# Sonu??lar?? yazd??rma
cat("Ki-Kare De??eri:", chi2_test$statistic, "\n")
cat("P-de??eri:", chi2_test$p.value, "\n")
cat("Serbestlik Derecesi:", chi2_test$parameter, "\n")
cat("Beklenen De??erler Tablosu:\n")
print(chi2_test$expected)

# Hipotez testi sonucu
if (chi2_test$p.value < 0.05) {
  cat("H0 hipotezi reddedilir: Hastan??n SMS almas?? durumu ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki vard??r.\n")
} else {
  cat("H0 hipotezi kabul edilir: Hastan??n SMS almas?? durumu ile randevuya gelme durumu aras??nda anlaml?? bir ili??ki yoktur.\n")
}



#10. Hipotez
#Hastan??n randevu ald?????? g??n ile randevu g??n?? aras??ndaki fark??n randevu durumu ile ili??kisi;
#Hipotez: Hastan??n hastane randevusu ald?????? g??n ile hastaneye gidece??i g??n aras??ndaki fark ile randevuya gelip gelmeme durumu ??zerindeki ili??ki.
#H0: Hastan??n randevu ald?????? g??n ile randevu g??n?? aras??ndaki fark??n randevu durumu ??zerinde bir etkisi yoktur.
#H1: Hastan??n randevu ald?????? g??n ile randevu g??n?? aras??ndaki fark??n randevu durumu ile anlaml?? bir ili??kisi vard??r.
#Test: T-Testi


library(tidyverse)
veri$Date.diff <- as.numeric(veri$Date.diff)

# G??sterim gruplar??n?? olu??turma
group_1 <- veri %>% filter(Showed_up == 1) %>% pull(Date.diff)
group_2 <- veri %>% filter(Showed_up == 0) %>% pull(Date.diff)

# T-Testi
t_test_result <- t.test(group_1, group_2)

# Sonu??lar?? yazd??rma
cat("T-Testi Sonucu:\n")
cat("Test istatisti??i:", t_test_result$statistic, "\n")
cat("P-de??eri:", t_test_result$p.value, "\n")

# Hipotez testi sonucu
alpha <- 0.05
if (t_test_result$p.value < alpha) {
  cat("H0 reddedildi: Hastan??n randevu ald?????? g??n ile randevu g??n?? aras??ndaki fark, randevuya gidip gitmeme durumuyla anlaml?? bir ili??kiye sahiptir.\n")
} else {
  cat("H0 kabul edildi: Hastan??n randevu ald?????? g??n ile randevu g??n?? aras??ndaki fark, randevuya gidip gitmeme durumu ??zerinde anlaml?? bir etkiye sahip de??ildir.\n")
}



#MODEL KURMAK ??????N VER?? HAZIRLAMA
library(dplyr)
veri <- veri %>% dplyr::select(-PatientId, -AppointmentID)

veri <- veri %>%
  mutate(Gender = recode(Gender, "F" = 0, "M" = 1))


library(lubridate)
veri$ScheduledDay <- ymd(veri$ScheduledDay)  
veri$AppointmentDay <- ymd(veri$AppointmentDay)


veri$DaysUntilAppointment <- as.numeric(difftime(veri$AppointmentDay, veri$ScheduledDay, units = "days"))


veri <- veri %>%
  mutate(Neighbourhood = as.numeric(factor(Neighbourhood)))


veri <- veri %>%
  mutate(across(c(Scholarship, Hipertension, Diabetes, Alcoholism, Handcap, SMS_received, Showed_up), as.integer))

veri <- veri %>%
  select(-ScheduledDay, -AppointmentDay)

y <- veri[["Showed_up"]]
x <- veri[, !(names(veri) %in% "Showed_up")]

print(levels(as.factor(y)))



#Logictik Regression Model Kurulmas??
# Paketleri y??kleme
install.packages("caret")
install.packages("pROC")
library(caret)
library(pROC)


# Veri b??lme
set.seed(42)
train_index <- createDataPartition(veri$Showed_up, p = 0.7, list = FALSE)
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test <- x[-train_index, ]
y_test <- y[-train_index]

# ??zellikleri ??l??eklendirme
x_train_scaled <- scale(x_train)
x_test_scaled <- scale(x_test, center = attr(x_train_scaled, "scaled:center"), scale = attr(x_train_scaled, "scaled:scale"))

# Lojistik regresyon modeli - Hiperparametre ayar??
set.seed(42)
tune_grid <- expand.grid(.intercept = TRUE)  # Lojistik regresyonda basit ayarlar
model <- train(
  x = x_train_scaled,
  y = as.factor(y_train),
  method = "glm",
  family = "binomial",
  trControl = trainControl(
    method = "repeatedcv",  # ??apraz do??rulama y??ntemi
    number = 10,           # 10 katl?? ??apraz do??rulama
    repeats = 3,           # ??apraz do??rulamay?? 3 kez tekrarla
    savePredictions = "final",  # Nihai tahminleri sakla
    classProbs = TRUE       # S??n??f olas??l??klar??n?? hesapla
  ),
  tuneGrid = tune_grid  # Hiperparametreleri burada belirtiyoruz
)

# Test verisiyle tahmin
predictions <- predict(model, x_test_scaled)
probs <- predict(model, x_test_scaled, type = "prob")  # Olas??l??k tahminleri

# Performans de??erlendirme
conf_matrix <- confusionMatrix(predictions, as.factor(y_test))
print(conf_matrix)

# AUC hesaplama
roc_curve <- roc(as.numeric(y_test), as.numeric(probs[, 2]))  # Olas??l??klar??n ikinci s??n??f??
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# ROC e??risi ??izimi
plot(roc_curve, col = "blue", main = "ROC Curve")






#XGBoost Regression
install.packages("xgboost")
install.packages("caret")
library(xgboost)
library(caret)


set.seed(42)
train_index <- createDataPartition(veri$Showed_up, p = 0.7, list = FALSE)
x_train <- as.matrix(x[train_index, ])  # ??zellikler matrisi
y_train <- as.numeric(as.factor(y[train_index])) - 1  # Hedef de??i??ken (0 veya 1 format??nda)

x_test <- as.matrix(x[-train_index, ])
y_test <- as.numeric(as.factor(y[-train_index])) - 1


set.seed(42)

# XGBoost modelini olu??turuyoruz
xgb_model <- xgboost(
  data = x_train,
  label = y_train,
  nrounds = 100,       # A??a?? say??s??
  max_depth = 6,       # A??a?? derinli??i
  eta = 0.3,           # ????renme oran??
  objective = "binary:logistic",  # Binary s??n??fland??rma
  eval_metric = "logloss"         # De??erlendirme metrikleri
)

# Model ??zeti
print(xgb_model)



# Test seti ??zerinde tahminler
xgb_predictions <- predict(xgb_model, x_test)
xgb_predictions <- ifelse(xgb_predictions > 0.5, 1, 0)  # 0.5'ten b??y??kse 1, k??????kse 0

# Kar??????kl??k Matrisi ve Performans De??erlendirme
confusionMatrix(factor(xgb_predictions), factor(y_test))

# ROC ve AUC De??eri
install.packages("pROC")
library(pROC)
roc_curve <- roc(y_test, xgb_predictions)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))


# Gerekli Paketleri Y??kleyin
install.packages("caret")
install.packages("xgboost")
install.packages("pROC")
library(caret)
library(xgboost)
library(pROC)

# Outcome de??i??kenini fakt??re ??eviriyoruz (s??n??fland??rma i??in)
y_train <- as.factor(y_train)
y_test <- as.factor(y_test)

# XGBoost i??in hiperparametre grid'ini olu??turma
grid <- expand.grid(
  nrounds = c(50, 100, 150),       # A??a?? say??s??
  max_depth = c(3, 6, 9),           # A??a?? derinli??i
  eta = c(0.1, 0.3, 0.5),          # ????renme oran??
  gamma = c(0, 0.1),               # Reg??larizasyon parametresi
  colsample_bytree = c(0.7, 1),     # ??zellik ??rnekleme oran??
  min_child_weight = c(1, 3),       # Minimum ??ocuk a????rl??????
  subsample = c(0.7, 1)             # Alt ??rnekleme oran?? (eklenen parametre)
)

# XGBoost modeli i??in train fonksiyonu
set.seed(42)
xgb_train <- train(
  x = x_train,                # E??itim verisi
  y = y_train,                # Etiketler
  method = "xgbTree",         # XGBoost s??n??fland??rma metodu
  trControl = trainControl(method = "cv", number = 5),  # 5 katl?? ??apraz do??rulama
  tuneGrid = grid             # Hiperparametreler i??in grid
)

# En iyi modelin hiperparametrelerini yazd??r
print(xgb_train$bestTune)

# Model ile tahmin yapma
xgb_best_model <- xgb_train$finalModel
xgb_best_predictions <- predict(xgb_best_model, x_test)

# ROC ve AUC hesaplama
roc_curve_best <- roc(y_test, as.numeric(xgb_best_predictions))  # ROC i??in say??sal de??erler kullan??l??yor
auc_value_best <- auc(roc_curve_best)
print(paste("Optimized XGBoost AUC:", auc_value_best))


















