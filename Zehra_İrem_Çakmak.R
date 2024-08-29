#Zehra İrem ÇAKMAK
#Yönetim Bilişim Sistemleri NÖ
#No:18697589722
#Çevre Aktivistlerinin Van Gogh'un Resmiyle Eylem Yapmalarına İnsanların Verdiği Tepkiler

# Paketleri aktifleştiriyorum.
library(textdata)#Metin maddeciliği (text mining) uygulamaları için kullanılır.
library(tuber)#YouTube Data API'ye erişim sağlar.
library(httpuv)#Web uygulamaları ve sunucu tarafı programlama için kullanılan bir HTTP ve WebSocket kütüphanesi.
library(httr)#HTTP istekleri göndermek ve almak için kullanılır.
library(tidytext)#Metin maddeciliği ve veri çerçeveleri arasında geçiş yapmak için kullanılır.
library(dplyr)#Veri çerçeveleri üzerinde veri manipülasyonu için kullanılır.
library(ggplot2)#Görselleştirme ve grafik oluşturma için kullanılır.
library(stopwords)#Dil bağımsız bir şekilde sık kullanılan kelimelerin listesini içerir.
library(tm)#Metin maddeciliği için kullanılır.
library(stringr)#Metin dizileri üzerinde işlemler gerçekleştirmek için kullanılır.
library(quanteda)# Metin maddeciliği ve metin analizi uygulamaları için kullanılan bir paket.
library(wordcloud2)# Kelime bulutları oluşturmak için kullanılır.
library(RColorBrewer)# Grafiklerde renk paletleri oluşturmak için kullanılır.
library(htmltools)#HTML belgelerini oluşturmak ve düzenlemek için kullanılır.
library(qdap)#Metin maddeciliği ve analiz için çeşitli araçlar içerir.
library(corpustools)# Metin maddeciliği uygulamaları için kullanılır.
library(slam)#Büyük ve seyrek matrisler üzerinde çalışmak için kullanılır.
library(tokenizers)# Metin maddeciliği uygulamaları için dilimleme görevlerini gerçekleştirir.
library(pander)# R Markdown ve R script dosyalarını zenginleştirmek için kullanılır.
library(DT)#Veri çerçevelerini interaktif olarak göstermek için kullanılır.
library(openxlsx)#Excel dosyalarını okumak ve yazmak için kullanılır.
library(writexl)#Excel dosyalarını yazmak için kullanılır.
library(sentimentr)# Duygu analizi yapmak için kullanılır.
library(pastecs)# Temel istatistik analizlerini gerçekleştirmek için kullanılır.
library(ggthemes)#ggplot2 grafikleri için özel temalar oluşturmak için kullanılır.
library(ggpubr)# ggplot2 ile yayın kalitesinde grafikler oluşturmak için kullanılır.
library(formattable)#Tabloları özelleştirmek ve biçimlendirmek için kullanılır.
library(ggstance)#ggplot2 grafiklerini yatay düzlemde oluşturmak için kullanılır.
library(GGally)#ggplot2 grafiklerini daha hızlı oluşturmak için kullanılır.
library(report)# R Markdown belgelerini oluşturmak ve yönetmek için kullanılır.
library(citation)#R paketleri için atıf bilgilerini otomatik olarak oluşturmak ve yönetmek için kullanılır.
library(hwordcloud)# Metin maddeciliği uygulamalarında kullanılan bir kelime bulutu oluşturma paketi.
library(syuzhet)# Metinlerin içerdiği duygusal özellikleri analiz etmek için kullanılır.

# Youtube API için OAuth kimlik bilgilerini atıyorum.Tuber paketini kullanıyorum.
client_id<-"350512921968-fpi69kcs3d7bpb7i7ekmjmrig3jvbved.apps.googleusercontent.com"
clientsecret<-"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"
yt_oauth(client_id,clientsecret,token = " ")

# Belirli bir YouTube videosundan yorumları çekiyorum.Tuber paketini kullanıyorum.
get_all_comments(video_id ="BN-C5N60u_M")
ilk_çekilen_yorumlar<-get_all_comments(video_id = "BN-C5N60u_M")

# Çekilen veriyi CSV dosyasına kaydediyorum.Writecsv kütüphanesini kullanıyorum.
write.csv(ilk_çekilen_yorumlar,file = "yeni_Tüm_verilerim.csv")

# CSV dosyasından veriyi tekrar okutuyorum.
yorumlar <- read.csv(file.choose(),header = T, sep = ",")

# İki sütunu seçip düzenliyorum.Dplyr kütüphanesini kullanıyorum.
yorumlar <- yorumlar %>%
  dplyr::select(X, textOriginal)

# Yorumları temizliyorum.Dplyr ve Tidytext kütüphanelerini kullanıyorum.
# Metindeki harflerin tamamını küçük harfe dönüştürür.
Duzenlenmıs_hali<-yorumlar %>% mutate(word=str_to_lower(textOriginal))%>% unnest_tokens(word, textOriginal)

# Metindeki noktalama işaretlerini kaldırır.
Duzenlenmıs_hali<-Duzenlenmıs_hali %>% mutate(word=removePunctuation(word)) 

# Metinle iç içe geçmiş (kalem325) rakam ve sayılar metinden ayıklar.
Duzenlenmıs_hali<-Duzenlenmıs_hali %>% mutate(word=str_squish(word))

# Metinden rakam ve sayıları çıkartır.
Duzenlenmıs_hali<-Duzenlenmıs_hali %>% mutate(word=removeNumbers(word)) 

# Stopwords listesini çağırıyorum.
StopWords <- readLines("stopwords.csv")

# StopWord'ün içindeki kelimeler metinden çıkartılır.
Duzenlenmıs_hali<-Duzenlenmıs_hali %>% filter(!word %in% StopWords) 

# Karakter sayısı 4'ten büyük kelimeler filtrelenir.
Duzenlenmıs_hali<-Duzenlenmıs_hali %>% filter(str_length(word)<1)

# İstenmeyen formatta kelimeler varsa ve
# bu kelimeleri çıkarmanız analize zarar verecekse yerine kelimeler atayabilirsiniz.
Duzenlenmıs_hali<-str_replace(Duzenlenmıs_hali$word, "[ı]", "i")

# Metnin değişiklikler sonra tibble tablo düzenine dönüştürülmüştür.
Duzenlenmıs_hali<-Duzenlenmıs_hali %>% as_tibble()%>%rename(word=value)

# Frekans analizi yapıyorum.Dplyr ve Tidytext kütüphanesi kullanıyorum.
verilerin_frekansı <-Duzenlenmıs_hali %>% group_by(word) %>% count() %>% arrange(desc(n))

# Frekans analizini Excel'e kaydediyorum.Writexl kütüphanesi kullanıyorum.
write_xlsx(verilerin_frekansı,"frekans_analizi.xlsx")

# Excel'deki frekans analizini düzenleyip tekrar çağırıyorum.Openxlsx kütüphanesi kullanılmıştır.
duzenlenmis_frekans <- read.xlsx("frekans_analizi.xlsx")

# Frekans analizinin sütun grafiğini yapıyorum.ggplot2 kütüphanesi kullanıyorum.
duzenlenmis_frekans %>%
  filter(n>150)%>% #n sütununda değeri 200 den fazla olanları al.
  ggplot(aes(word,n))+ #grafik oluşturur.
  geom_col(fill = "pink", color = "black")+ #çubuk grafiği oluşturmak için kullanılır.
  xlab("Kelimeler")+
  ylab("Frekans Sayısı")+
  ggtitle("En Çok Kullanılan Kelimeler")

# Kelime bulutu oluşturuyorum. wordcloud2 kütüphanesini kullanıyorum.
# Kelime bulutu 1  
wordcloud2(duzenlenmis_frekans,minSize = 3,
           color = "random-dark", backgroundColor = "lightpink")
# Kelime bulutu 2 
wordcloud2(duzenlenmis_frekans, minSize = 3,
           color = "darkgreen", backgroundColor = "skyblue")

# Polarite analizi yapıyorum.Sentimentr ve pander kütüphanelerini kullanıyorum.
# Polorite Analizi Kısmı
sentences <- get_sentences(duzenlenmis_frekans$word)
polarite <- sentiment(sentences)

polarite_analizi <- cbind(duzenlenmis_frekans, polarite)

# Polarite analizi istatistikleri
stat.desc(polarite$sentiment, basic=T) %>% pander()

# Polorite analizinin grafikle gösterimi
tablo<-cbind(duzenlenmis_frekans$word, polarite[,c(3,4)])

ggplot(tablo, aes(word_count, sentiment))+
  geom_point(color="blue")+
  geom_hline(yintercept = mean(tablo$sentiment), color="red", size=1)+
  labs(y = "Skor", x = "Kelimelerin Frekansı") +
  ylim(-1,1)+
  theme_gray()
labs(caption = "Çevre Aktivistlerine Karşı Yorumların Polarite Analizi")+
  theme(plot.caption = element_text(hjust = 0, face = "italic"))

# Yaptığım polarite analizine duygu puanı atayarak tablosunu oluşturuyorum.
Duygu_durumu <- function(duygu_puani) {
  if (duygu_puani > 0) {
    return("Pozitif")
  } else if (duygu_puani < 0) {
    return("Negatif")
  } else {
    return("Nötr")
  }
}

polarite_analizi$etiket <- sapply(polarite_analizi$sentiment,Duygu_durumu)

# Polarite analizi görselleştiriyorum.ggplot2 kütüphanesi kullanılmıştır.
ggplot(polarite_analizi, aes(x = etiket, fill = etiket)) +
  geom_bar() +
  labs(title = "Kelimelerin Polarite Analizi", x = "Duygu Durumu", y = "Kelime Frekansı") +
  scale_fill_manual(values = c("Pozitif" = "green", "Negatif" = "red", "Nötr" = "gray")) +
  theme_minimal()

# Polarite analizi sonuçları. Dplyr kütüphanesi kullanılmıştır.
polarite_analizi %>%
  group_by(etiket) %>%
  summarize(sayi = n())

# Polarite analizi tablosu yapıyorum.Dplyr ve Tidytext kütüphaneleri kullanılmıştır.
polarite_analiz<-duzenlenmis_frekans %>% inner_join(get_sentiments("bing"),by="word")

# Negatiflik ve pozitiflik oranları için tablo yapıyorum.Dplyr ve DT kütüphaneleri kullanılmıştır.
polarite_analiz[,c(1,3,2)]%>%
  group_by(sentiment) %>%
  summarise(toplam=sum(n)) %>%
  mutate(oran=round(toplam/sum(toplam)*100,2)) %>%
  arrange(desc(oran)) %>%
  rename("duygu"="sentiment") %>%
  datatable()

# Polarite analizi noktalı grafik tablosu yapıyorum.
tablo<-cbind(duzenlenmis_frekans$word, polarite[,c(3,4)])

#Tablo için ggplot2 kütüphanesi kulanılmıştır.
ggplot(polarite_analiz, aes(n, sentiment))+
  geom_point(color="purple",na.rm = T)+
  geom_hline(yintercept = mean(tablo$sentiment), color="pink", linewidth=1)+
  labs(y = "Duygu Durumu", x = "Kelimelerin Frekansı") +
  theme_gray()+
  labs(caption = "Çevre Aktivistlerine Karşı Yorumların Polarite Analizi")+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))

# Polarite analizi kelime bazlı tablo yapıyorum. ggplot2, dplyr ve highcharter kütüphaneleri kullanılmıştır.
polarite_analiz[,c(1,3,2)]%>% group_by(sentiment) %>% arrange(desc(n)) %>%
  top_n(20) %>%
  ggplot(aes(x=reorder(word,n), y=n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Tekrar Sayısı",x = "Kelime") +
  coord_flip() +
  labs(caption = "Çevre Aktivistlerine Karşı Yorumların Polarite Analizi")+
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))

# Affin Leksiyonu kullanarak analiz yapıyorum.dplyr, tidytext, pastecs ve pander kütüphaneleri kullanılmıştır.
affin <- duzenlenmis_frekans %>% inner_join(get_sentiments("afinn"), by = "word")
stat.desc(affin$value, basic = TRUE) %>% pander()

# Loughran leksiyonu kullanarak 6 duygu oranı bulup tablolaştırıyorum.dplyr ve DT paketleri kullanılmıştır.
set.seed(1985) 
loughran <- duzenlenmis_frekans %>% inner_join(get_sentiments("loughran"), by = "word") 

loughran %>% 
  group_by(sentiment) %>% 
  summarise(toplam = n()) %>% 
  mutate(oran = round(toplam / sum(toplam) * 100, 2)) %>% 
  arrange(desc(oran)) %>%  
  rename("duygu" = "sentiment") %>% 
  datatable()

# Loughranın sütun grafiğini yapıyorum.ggplot2 ve scales kütüphaneleri kullaılmıştır.
loughran %>% 
  group_by(sentiment) %>% 
  summarise(n = n()) %>%
  ggplot(aes(reorder(sentiment, n), n, fill = sentiment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(y = "Frekans", x = "Duygu") +
  labs(caption = "Çevre Aktivistlerine Karşı Yorumların Polarite Analizi") +
  theme(plot.caption = element_text(hjust = 0.5, face = "italic"))

