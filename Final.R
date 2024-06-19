library(shiny)
library(shinydashboard)
library(ggplot2)
library(broom)
library(ggpubr)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(rsconnect)
library(forecast)

#Membuat prefix untuk tempat menyimpan foto (khusus local)
#addResourcePath(prefix = 'foto', directoryPath = 'D:/Univ/FEB/Sem 2/R Programming/Tugas/Final Project/R Program/Final Project/Final Result/foto')

# Data untuk contoh

ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "RAMALANKUrs"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prediksi", tabName = "Prediksi", icon = icon("chart-line")),
      menuItem("Indicators", tabName = "Indicators", icon = icon("dashboard"),
               menuSubItem("Inflasi", tabName = "Inflasi"),
               menuSubItem("BI Rate", tabName = "BI_Rate")
      ),
      menuItem("Rupiah (IDR)", tabName = "Rupiah", icon = icon("map")),
      menuItem("About", tabName = "About", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      #konten untuk dashboard prediksi 
      tabItem(tabName = "Prediksi",
             fluidRow(
               box(
                 title = "Peringatan", background = "red", solidHeader = TRUE,
                 "Peringatan: Hasil prediksi ini hanyalah perkiraan dan tidak 100% akurat. Harap gunakan dengan bijaksana."
               ), 
               box(
                 title = "Prediksi Nilai Tukar Mata Uang Asing Terhadap Rupiah", status = "info",
                 wellPanel(
                   selectInput("select_pred", "Pilih Negara:",
                               choices = c("Australia (AUD)", "Brunei Darusallam (BND)", "Tiongkok (CNY)", "Eropa (EUR)", "Hongkong (HKD)", "Jepang (JPY)", "Korea Selatan (KRW)", "Arab Saudi (SAR)", "Swedia (SEK)", "Singapura (SGD)", "Thailand (THB)", "Amerika Serikat (USD)", "Vietnam (VND)")),
                   sliderInput("slider_pred", "Prediksi untuk berapa tahun ke depan?",
                               min = 1, max = 10, value = 1)
                 ),
                 actionButton("submit", "Submit")
               )
             ),
             fluidRow(
               plotOutput("plot")
             )
      ),   
      
      #konten untuk dashboard inflasi
      tabItem(tabName = "Inflasi",
              tabBox(
                width = 12,
                fluidRow(box(width = 12,
                             textOutput("inftext"))
                ),
                fluidRow(
                  box(plotlyOutput("infplot")),
                  box(DT::dataTableOutput("inftable"))
                )
              )
      ),
      
      #konten untuk dashboard BI Rate
      tabItem(tabName = "BI_Rate",
              tabBox(
                width = 12,
                fluidRow(
                  box(width = 12,
                      textOutput("bitext")),
                  box(plotlyOutput("biplot")),
                  box(DT::dataTableOutput("bitable"))
                )
              )
      ),
  
      # konten untuk dashboard rupiah
      tabItem(tabName = "Rupiah",
              fluidRow(
                box(
                  title = "Informasi Nilai Tukar",width = 12,
                  "Informasi ini memberikan data waktu nyata tentang nilai tukar berbagai mata uang, 
                  mencerminkan nilainya dalam hubungannya satu sama lain di pasar global. Informasi 
                  ini sangat penting bagi bisnis dan investor yang terlibat dalam perdagangan 
                  internasional dan transaksi keuangan, karena memungkinkan mereka untuk memantau 
                  fluktuasi mata uang dan membuat keputusan yang berdasarkan informasi. Selain itu, 
                  data mata uang saat ini berfungsi sebagai indikator kesehatan dan stabilitas ekonomi
                  bagi negara-negara individu, memengaruhi faktor seperti inflasi, tingkat bunga, dan 
                  investasi asing. Berikut adalah nilai tukar rupiah terhadap mata uang asing di 
                  berbagai negara. Data nilai tukar diambil dari website resmi Bank Indonesia (bi.go.id)"
                  )
              ),
              fluidRow(
                box( 
                  title = "Peta Nilai Tukar Rupiah", status = "primary", solidHeader = TRUE, width = 12,
                  leafletOutput("map", height = 600)
                )
              )
      ), 
      # konten untuk dashboard about 
      tabItem(tabName = "About", 
              fluidRow(
                box( width =6,
                  title = "DEVELOPER", status = "info", 
                  textOutput("about1"),
                  tags$figure(column(4, align = "center",
                                     tags$img(src = "Dini.jpeg", width = 144.2, height = 184.2),
                              tags$figcaption("Theodora Puty Andini")
                              ),
                              column(4, align = "center",
                                     tags$img(src = "Nicho.jpeg", width = 144.2, height = 184.2),
                              tags$figcaption("Nicholaus Raditya Wisnu Wardhana")
                              ),
                              column(4, align = "center",
                                     tags$img(src = "Dika.jpeg", width = 144.2, height = 184.2),
                              tags$figcaption("Mahardika Agus Rifa'i")
                              )),
                  
                    box(width = 12, textOutput("about2"))
                    ),
              box(
                box(width = 12,
                  title = "Lukman Heryawan, S.T., M.T., Ph.D.(Dosen Pengampu)", status = "info",
                  "Dosen pengampu mata kuliah R Programming BR123",
                  tags$figure(column(4, align = "center",
                                     tags$img(src = "Lukman.jpeg", width = 184.2, height = 184.2)
                ))),
                box(width = 12,
                  title = "Tentang Mata Kuliah", background = "black", solidHeader = TRUE,
                  "R Programming adalah salah satu mata kuliah yang ditawarkan oleh FEB UGM kepada para mahasiswa/i nya. Mata kuliah ini akan mempelajari bagaimana program R dapat digunakan secara efektif untuk pengolahan data. Mahasiswa/i akan mempelajari melalui contoh pemrosesan dan visualisasi dari studi kasus project."    
                )
              )
              )
          )
    )
  )
)


server <- function(input, output) {
  #Data untuk nilai tukar Rupiah terhadap berbagai negara
    nilai_tukar_data <- data.frame(
      country = c("United States", "India", "Korea, South", "Saudi Arabia", "China", "Zimbabwe", "Australia", 
                  "Switzerland", "Vietnam", "Timor-Leste", "Japan", "Singapore", "Turkey", "Malaysia", 
                  "United Kingdom", "Netherlands", "France"),
      currency = c("USD", "INR", "KRW", "SAR", "CNY", "ZWL", "AUD", "CHF", "VND", "USD", "JPY", "SGD", 
                   "TRY", "MYR", "GBP", "EUR", "EUR"),
      exchange_rate = c(16289, 1900, 10, 2247, 2800, 0.05, 10743, 18159, 0.64, 16289, 103, 12037, 
                        2500, 3452, 20725, 17536, 17536),
      lat = c(37.0902, 20.5937, 35.9078, 23.8859, 35.8617, -19.0154, -25.2744, 46.8182, 14.0583, -8.8742, 36.2048, 1.3521, 
              38.9637, 4.2105, 55.3781, 52.1326, 46.6034),
      lng = c(-95.7129, 78.9629, 127.7669, 45.0792, 104.1954, 29.1549, 133.7751, 8.2275, 108.2772, 125.7275, 138.2529, 
              103.8198, 35.2433, 101.9758, -3.4360, 5.2913, 2.2137)
  )
  
  #input data kurs
  data_AUD <- read.csv("data/AUD.csv")
  data_BND <- read.csv("data/BND.csv")
  data_CNY <- read.csv("data/CNY.csv")
  data_EUR <- read.csv("data/EUR.csv")
  data_HKD <- read.csv("data/HKD.csv")
  data_JPY <- read.csv("data/JPY.csv")
  data_KRW <- read.csv("data/KRW.csv")
  data_SAR <- read.csv("data/SAR.csv")
  data_SEK <- read.csv("data/SEK.csv")
  data_SGD <- read.csv("data/SGD.csv")
  data_PHP <- read.csv("data/PHP.csv")
  data_THB <- read.csv("data/THB.csv")
  data_USD <- read.csv("data/USD.csv")
  data_VND <- read.csv("data/VND.csv")
  
  observeEvent(input$submit, {
    country <- input$select_pred
    years_ahead <- input$slider_pred
    
    # Pilih data berdasarkan negara
    if (country == "Australia (AUD)") {
      data <- data_AUD
    } else if (country == "Brunei Darusallam (BND)") {
      data <- data_BND
    } else if (country == "Tiongkok (CNY)") {
      data <- data_CNY
    } else if (country == "Eropa (EUR)") {
      data <- data_EUR
    } else if (country == "Hongkong (HKD)") {
      data <- data_HKD
    } else if (country == "Jepang (JPY)") {
      data <- data_JPY
    } else if (country == "Korea Selatan (KRW)") {
      data <- data_KRW
    } else if (country == "Arab Saudi (SAR)") {
      data <- data_SAR
    } else if (country == "Swedia (SEK)") {
      data <- data_SEK
    } else if (country == "Singapura(SGD)") {
      data <- data_SGD
    } else if (country == "Filipina (PHP)") {
      data <- data_PHP
    } else if (country == "Thailand (THB)") {
      data <- data_THB
    } else if (country == "Amerika Serikat (USD)") {
      data <- data_USD
    } else if (country == "Vietnam (VND)") {
      data <- data_VND
    }
    
    # Lakukan prediksi menggunakan model ARIMA
    ts_data <- ts(data$nilai_tukar, start = min(data$year), frequency = 1)
    fit <- auto.arima(ts_data)
    forecast_data <- forecast(fit, h = years_ahead)
    
    # Plot hasil prediksi
    output$plot <- renderPlot({
      autoplot(forecast_data) +
        labs(title = paste("Prediksi Nilai Tukar", country, "terhadap Rupiah"),
             x = "Tahun",
             y = "Nilai Tukar") +
        scale_x_continuous(breaks = seq(min(data$year), max(data$year) + years_ahead, by = 1)) +
        theme_minimal()
    })
  })
  
  #input data Inflasi dan BI rate
  DF <- read.csv("data/Inflasi.csv")
  DF2 <- read.csv("data/BI Rate.csv")
  
  #Mendefinisikan urutan bulan
  Urut <- c("Januari 2016", "Februari 2016", "Maret 2016", "April 2016", 
            "Mei 2016", "Juni 2016", "Juli 2016", "Agustus 2016", 
            "September 2016", "Oktober 2016", "November 2016", "Desember 2016", 
            "Januari 2017", "Februari 2017", "Maret 2017", "April 2017", 
            "Mei 2017", "Juni 2017", "Juli 2017", "Agustus 2017", 
            "September 2017", "Oktober 2017", "November 2017", "Desember 2017", 
            "Januari 2018", "Februari 2018", "Maret 2018", "April 2018", 
            "Mei 2018", "Juni 2018", "Juli 2018", "Agustus 2018", 
            "September 2018", "Oktober 2018", "November 2018", "Desember 2018",
            "Januari 2019", "Februari 2019", "Maret 2019", "April 2019", 
            "Mei 2019", "Juni 2019", "Juli 2019", "Agustus 2019", 
            "September 2019", "Oktober 2019", "November 2019", "Desember 2019",
            "Januari 2020", "Februari 2020", "Maret 2020", "April 2020", 
            "Mei 2020", "Juni 2020", "Juli 2020", "Agustus 2020", 
            "September 2020", "Oktober 2020", "November 2020", "Desember 2020",
            "Januari 2021", "Februari 2021", "Maret 2021", "April 2021", 
            "Mei 2021", "Juni 2021", "Juli 2021", "Agustus 2021", 
            "September 2021", "Oktober 2021", "November 2021", "Desember 2021",
            "Januari 2022", "Februari 2022", "Maret 2022", "April 2022", 
            "Mei 2022", "Juni 2022", "Juli 2022", "Agustus 2022", 
            "September 2022", "Oktober 2022", "November 2022", "Desember 2022",
            "Januari 2023", "Februari 2023", "Maret 2023", "April 2023", 
            "Mei 2023", "Juni 2023", "Juli 2023", "Agustus 2023", 
            "September 2023", "Oktober 2023", "November 2023", "Desember 2023",
            "Januari 2024", "Februari 2024", "Maret 2024", "April 2024", 
            "Mei 2024", "Juni 2024", "Juli 2024", "Agustus 2024", 
            "September 2024", "Oktober 2024", "November 2024", "Desember 2024")
  
  #Mengurutkan informasi tabel berdasarkan bulan
  Data_Inflasi <- DF %>% mutate(Waktu = factor(Waktu, 
                                               levels = Urut, ordered = TRUE))
  Data_BI_Rate <- DF2  %>% mutate(Waktu = factor(Waktu, 
                                                 levels = Urut, ordered = TRUE))
  
  # Membuat plot untuk dashboard inflasi dan bi rate
  Grafik_Inflasi <- ggplotly(ggplot(Data_Inflasi, aes(x=Waktu, y=Inflasi)) + 
                               geom_line(col="blue", aes(group = 1))+geom_point(col="blue")+
                               labs(title = "Inflasi Indonesia (persen)", x="Waktu", y="Inflasi") + 
                               theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()))
  
  Grafik_BI_Rate <- ggplotly(ggplot(Data_BI_Rate, aes(x=Waktu, y=BI_Rate))+ 
                               geom_line(col="blue", aes(group = 1))+geom_point(col="blue")+
                               labs(title = "BI Rate (persen)", x="Waktu", y="Suku Bunga") +
                               theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()))
    # Render
    output$infplot <- renderPlotly(
      Grafik_Inflasi)
    output$inftable <- DT::renderDataTable(
      Data_Inflasi, options = list(dom = 'lpt'))
    output$biplot <- renderPlotly(
      Grafik_BI_Rate)
    output$bitable <- DT::renderDataTable(
      Data_BI_Rate, options = list(dom = 'lpt'))
    
  # Membuat teks penjelasan untuk dashboard inflasi dan bi rate
  output$inftext <- renderText("Inflasi merupakan kondisi penurunan daya beli 
                               mata uang sebuah negara yang ditunjukkan dengan 
                               meningkatnya harga barang di pasar. Menurunnya 
                               daya beli sebuah mata uang akan mempengaruhi 
                               nilai tukarnya terhadap mata uang lain. Semakin
                               tinggi tingkat inflasi suatu negara nilai 
                               tukarnya akan semakin melemah (terjadi depresiasi)
                               karena daya belinya semakin berkurang.
                               Akan tetapi, tingkat inflasi yang rendah bukan berarti
                               nilai tukar mata uang akan langsung menguat (apresiasi), 
                               karena inflasi hanya satu  dari banyak faktor 
                               yang mempengaruhi nilai tukar mata uang")
  output$bitext <- renderText("BI Rate atau sekarang disebut BI 7 Day Reverse Repo Rate 
                              adalah tingkat suku bunga yang ditetapkan oleh Bank 
                              Indonesia. BI Rate digunakan sebagai acuan untuk suku bunga
                              bank. Tingkat BI Rate disesuaikan dengan kondisi ekonomi 
                              Indonesia agar bisa mengontrol inflasi dan nilai tukar mata
                              uang.")
  output$about1 <- renderText("Website aplikasi ini dirancang dan dibuat oleh:")
  output$about2 <- renderText("Dengan 
                              mengikuti mata kuliah ini, kami yakin dapat mengembangkan 
                              skill kami dalam mengolah data, menganalisis tren, dan 
                              memvisualisasikannya. Mata kuliah ini sangat berguna bagi 
                              program studi kami masing-masing untuk ke depannya dan 
                              akan sangat membantu dalam mata kuliah lanjutan. Dengan 
                              demikian, mengikuti mata kuliah ini tidak hanya akan 
                              memberikan tantangan intelektual saja, tetapi juga membuka 
                              kesempatan untuk pengembangan diri dan profesional di 
                              jangka panjang.")
  
  # Membuat peta dunia dengan leaflet
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = c("green", "yellow", "orange", "red"), domain = nilai_tukar_data$exchange_rate)
    
    leaflet(nilai_tukar_data) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addCircleMarkers(~lng, ~lat, 
                       radius = 5, 
                       color = ~pal(exchange_rate), 
                       stroke = FALSE,
                       fillOpacity = 0.8, 
                       popup = ~paste("Country: ", country, "<br>",
                                      "Currency: ", currency, "<br>",
                                      "Exchange Rate: ", exchange_rate, " IDR")) %>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~exchange_rate,
                title = "Exchange Rate (IDR)",
                opacity = 1)
  })
}

shinyApp(ui, server)