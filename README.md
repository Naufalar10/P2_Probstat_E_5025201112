## Nama-NRP
| Nama                      | NRP        |
|---------------------------|------------|
| Naufal Ariq Putra Yosyam  | 5025201112 |

<br />

## Soal 1
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

| Responden   | X        | Y        |
|-------------|----------|----------|
| 1 | 78 | 100 |
| 2 | 75 | 95 |
| 3 | 67 | 70 |
| 4 | 77 | 90 |
| 5 | 70 | 90 |
| 6 | 72 | 90 |
| 7 | 78 | 89 |
| 8 | 74 | 90 |
| 9 | 77 | 100 |


Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah melakukan aktivitas 𝐴 sebanyak 70.

  - A. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel diatas

    Langkah pertama penyelesaian adalah memasukkan semua data yang ada pada tabel pada sebuah variabel sebagai berikut

    
      ```R
      sebelum_aktivitas <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
      setelah_aktivitas <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
      ```

    Selanjutnya mencari standar deviasinya. Standar deviasi sebelum dan sesudah aktivitas adalah

        ```
         perbedaan_Saturasi <- setelah_aktivitas - sebelum_aktivitas
         sd(perbedaan_Saturasi)
        ```
      ![ss1][ss1a]
      
      [ss1a]: probstat2/1a.png
     

</br>

   - B. carilah nilai t (p-value)
      Untuk mencari nilai t (p-value) dapat menggunakan fungsi `t.test` yaitu sebagai berikut
        ```R
         # Menggunakan t-test
         t.test(sebelum_aktivitas, setelah_aktivitas, paired = TRUE)
       ```    
       Sehingga Hasilnya sebagai berikut:
        ![ss1][ss1b]
      
      [ss1b]: probstat2/1b.png
       
     
  
  </br>

   - C. Tentukanlah Apakah Terdapat Pengaruh yang Signifikan Secara Statistika dalam Hal Kadar Saturasi Oksigen, Sebelum dan Sesudah Melakukan Aktivitas 𝐴  jika Diketahui Tingkat Signifikansi 𝛼 = 5% serta H0 : “tidak ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
     Berdasarkan hasil dari poin sebelumnya, diketahui bahwa nilai probabilitas dari uji 𝑡 (p-value) adalah 6.003e-05 atau 0.00006003. Karena nilai probabilitas tersebut lebih kecil dibandingkan tingkat signifikansi 𝛼 = 0.05, maka **hipotesis nol ditolak dan hipotesis alternatif diterima.** 

Hal ini berarti terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas 𝐴 pada tingkat signifikansi 5%.

</br>

## Soal 2
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun. Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata 23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan 2 library seperti referensi pada modul).

  - A. Apakah Anda setuju dengan klaim tersebut?

    Setuju, karena uji z menolak H0
    
  
    
    </br>

  - B. Jelaskan maksud dari output yang dihasilkan!
       Diketahui n = 100, Rata-Rata (X̄) = 23500, dan standar deviasi(σ) = 3900 Maka null hipotesis adalah
       Maka null hipotesis adalah 
          ```
          H0 : μ = 20000
          ```
          Alternatif hipotesisnya yaitu
          ```
          H1 : μ > 20000
          ```
       ![ss1][ss2b]
      
      [ss2b]: probstat2/2b.png

       
       
       </br>

  - C. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
  
       Untuk mencari nilai z nya yaitu
       </br>
        ![ss1][ss2c1]
      
      [ss2c1]: probstat2/2c1.png
       Lalu mencari nilai p-value nya sebagai berikut
       </br>
        ![ss1][ss2c2]
      
      [ss2c2]: probstat2/2c2.png
               
       

       Sehingga kesimpulan yang didapat adalah bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun

       </br>
  
## Soal 3
Diketahui perusahaan memiliki seorang data analyst ingin memecahkan permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya didapatkanlah data berikut dari perusahaan saham tersebut.


 
   Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada rata-ratanya (α= 0.05)? Buatlah :
   
    A. H0 dan H1
  
    B. Hitung Sampel Statistik
  
    C. Lakukan Uji Statistik (df = 2) 
  
    D. Nilai Kritikal
  
    E. Keputusan
  
    F. Kesimpulan

  </br>
  
  - A. H0 dan H1
    dilakukan perhitungan H0 sebagai berikut
    </br>
     ![ss1][ss3a]
      
      [ss3a]: probstat2/3a.png
    
    </br>
    
    dilakukan perhitungan H1 sebagai berikut
    </br>
    
     ![ss1][ss3a1]
      
      [ss3a1]: probstat2/3a1.png
    </br>

  - B. Hitung Sampel Statistik
      Untuk menghitung sampel statistik dapat menggunakan fungsi `tsum.test` yaitu sebagai berikut
```
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y =2.79 , s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)
```
 
   ![ss1][ss3b]
      
  [ss3b]: probstat2/3b.png
      

  - C. Lakukan Uji Statistik (df = 2) 
      Menggunakan bantuan library `mosaic`
      
      
      
      Sehingga Hasilnya sebagai berikut:
       ![ss1][ss3c]
      
      [ss3c]: probstat2/3c.png     

      </br>
  
  - D. Nilai Kritikal 
       Untuk menghitung sampel statistik dapat menggunakan fungsi `qchisq` dengan `df=2` yaitu sebagai berikut

       Sehingga Hasilnya sebagai berikut:
       
       ![ss1][ss3d]
      
      [ss3d]: probstat2/3d.png

       

       </br>
       
  - E. Keputusan
       Untuk mendapatkan keputusan diperlukan bantuan dengan Teori Keputusan

       Teori keputusan adalah teori formal pengambilan keputusan di bawah ketidakpastian. 
       Aksinya adalah : `({a}_{a∈A})`
       Kemungkinan konsekuensi : `({c}_{c∈C})` (tergantung pada keadaan dan tindakan)
       Maka keputusan dapat dibuat dengan `t.test`

       </br>
       
  - F. Kesimpulan
       Kesimpulan yang didapatkan yaitu perbedaan rata-rata yang terjadi tidak ada jika dilihat dari uji statistik dan akan ada tetapi tidak signifikan jika dipengaruhi nilai kritikal.


       </br>

## Soal 4
> Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan kucing putih dengan panjangnya masing-masing.

   Jika : diketahui dataset https://intip.in/datasetprobstat1
   
   

   H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya sama

   Maka Kerjakan atau Carilah: 
   
    A. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1, grup 2, grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.
    
    B. Carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? Apa hipotesis dan kesimpulan yang dapat diambil?
    
    C. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.
    
    D. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
    
    E. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? Jelaskan.
    
    F. Visualisasikan data dengan ggplot2
    
   </br>

   - A. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1, grup 2, grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians.
   
     Langkah pertama mengambil data dari link yang telah disediadakan

      ```
        myFile  <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt")) 
        dim(myFile)
        head(myFile)
      ```
       ![ss1][ss4a]
      
      [ss4a]: probstat2/4a.png
      

      Selanjutnya membuat myFile menjadi group 
      
      ```
        myFile$Group <- as.factor(myFile$Group)
        myFile$Group = factor(myFile$Group,labels = c("Kucing Oren","Kucing Hitam","Kucing Putih"))
      ```

      Setelah itu, dicek apakah dia menyimpan nilai di groupnya
      
      ```
        class(myFile$Group)
      ```

      Lalu bagi tiap valuer menjadi 3 bagian ke 3 grup
      
      ```
        group1 <- subset(myFile, Group=="Kucing Oren")
        group2 <- subset(myFile, Group=="Kucing Hitam")
        group3 <- subset(myFile, Group=="Kucing Putih")
      ```
      
      </br>

   - B. Carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang didapatkan? Apa hipotesis dan kesimpulan yang dapat diambil?

        Mencari Homogeneity of variances bisa menggunakan command sebagai berikut
        
        ```
        bartlett.test(Length~Group, data=dataoneway)
        ```
        
        Didapatkan nilai dari p-value yaitu = 0.8054. 
        Kesimpulan yang didapatkan yaitu Bartlett's K-squared memiliki nilai sebesar 0.43292 dan df bernilai 2
      
   - C. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus Grup dan beri nama model tersebut model 1.
        Mencari Homogeneity of variances bisa menggunakan command sebagai berikut
        
        ```
        qqnorm(group1$Length)
        qqline(group1$Length)
        ```
      ![ss1][ss4c]
      
      [ss4c]: probstat2/4c.png
        
        
        </br>

   - D. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan dari H0?
        
        Didapatkan nilai dari p-value yaitu = 0.8054. 
        
        </br>
   
   - E. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? Jelaskan.

        ```
        model1 <- lm(Length~Group, data=myFile)
        ```
        
        Selanjutnya menggunakan command 
        
        ```
        anova(model1)
        ```
        
        Lalu menggunakan model Post-hoc Tukey HSD sebagai berikut
        
        ```
        TukeyHSD(aov(model1))
        ```
        </br>

   - F. Visualisasikan data dengan ggplot2
  
        ```
        library(ggplot2)
        ggplot(dataoneway, aes(x = Group, y = Length)) + geom_boxplot(fill = "grey80", colour = "black") + scale_x_discrete() + xlab("Treatment Group") +  ylab("Length (cm)")
        ```
        
        </br>

## Soal 5
> Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk mengetahui pengaruh suhu operasi (100 ̊C, 125 ̊C dan 150 ̊C) dan tiga jenis kaca pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan dilakukan sebanyak 27 kali dan didapat data sebagai berikut: 

  Data Hasil Eksperimen. 
  
   

   Dengan data tersebut:
   
     A. Buatlah plot sederhana untuk visualisasi data
     
     B. Lakukan uji ANOVA dua arah
     
     C. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
     
     D. Lakukan uji Tukey
     
     E. Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey
     
   Berikut adalah contoh daftar package dan fungsi yang dapat digunakan (dapat pula menggunakan contoh lainnya)
   
     - Packages: readr, ggplot2, multcompView, dplyr
     - Function: aov, TukeyHSD, qplot, group_by, summarise, multcompLetters4

   - A. Buatlah plot sederhana untuk visualisasi data
   
        Run semua library yang diperlukan
        
        ```
        install.packages("multcompView")
        install.packages("ggplot2")
        install.packages("readr")
        install.packages("dplyr")
        library(readr)
        library(ggplot2)
        library(multcompView)
        library(dplyr)
        ```

        Selanjutnya membaca file GTL.csv dari documents
        
        ```
        GTL <- read_csv("GTL.csv")
        head(GTL)
        ```
        
       ![ss1][ss5a]
      
      [ss5a]: probstat2/5a.png
        

        Lakukan observasi pada data
        
        ```
        str(GTL)
        ```
        
        ![ss1][ss5a1]
      
      [ss5a1]: probstat2/5a1.png 
        
        </br>

        Selanjutnya lakukan viasualisasi menggunakan simple plot yaitu sebagai berikut
        
        ```
        qplot(x = Temp, y = Light, geom = "point", data = GTL) +
          facet_grid(.~Glass, labeller = label_both)
        ```
        
        ![ss1][ss5a2]
      
      [ss5a2]: probstat2/5a2.png
        
        </br>
        
   - B. Lakukan uji ANOVA dua arah
        
        Langkah pertama adalah membuat variabel as factor sebagai ANOVA
        
        ```
        GTL$Glass <- as.factor(GTL$Glass)
        GTL$Temp_Factor <- as.factor(GTL$Temp)
        str(GTL)
        ```
        ![ss1][ss5b]
      
      [ss5b]: probstat2/5b.png
        
        </br>

        Selanjutnya melakukan analisis of variance (aov) yaitu sebagai berikut 
        
        ```
        anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
        summary(anova)
        ```
        
        
   - C. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
        
        Menggunakan `group_by` lalu melakukan `summarise` sesuai mean dan standar deviasi yang berlaku sehingga scriptnya adalah sebagai berikut
        
          ```
          data_summary <- group_by(GTL, Glass, Temp) %>%
            summarise(mean=mean(Light), sd=sd(Light)) %>%
            arrange(desc(mean))
          print(data_summary)
          ```
      
        ![ss1][ss5c]
        
        [ss5c]: probstat2/5c.png
      
      </br>
        
     - D. Lakukan uji Tukey
          Menggunakan fungsi `TukeyHSD` sebagai berikut
          
          ```
          tukey <- TukeyHSD(anova)
          print(tukey)
          ```
          
         ![ss1][ss5d]
      
      [ss5d]: probstat2/5d.png
          
          
     - E. Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey

          Awalnya yaitu membuat compact letter display sebagai berikut
          
          ```
          tukey.cld <- multcompLetters4(anova, tukey)
          print(tukey.cld)
          ```
          
          ![ss1][ss5e1]
      
      [ss5e1]: probstat2/5e1.png
          
          
          </br>
          
          Tambahkan compact letter display tersebut ke tabel dengan means(rata-rata) dan sd

          ```
          cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
          data_summary$Tukey <- cld$Letters
          print(data_summary)
          ```
           
     ![ss1][ss5e2]
     
      [ss5e2]: probstat2/5e2.png
          
