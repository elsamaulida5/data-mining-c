library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(shinythemes)
library(GGally)
library(tidyr)


# Membaca data dari file CSV
df <- read.csv("https://raw.githubusercontent.com/elsamaulida5/data-mining-c/main/FPdatmin/Hotel%20Reservations.csv")
head(df)

# Menampilkan 5 baris terakhir data
tail(df)

# Menampilkan bentuk data
dim(df)

# Menampilkan informasi tentang dataframe
str(df)

# Mengubah kolom tahun, bulan, dan tanggal kedatangan ke bentuk string
df$arrival_year <- as.character(df$arrival_year)
df$arrival_month <- as.character(df$arrival_month)
df$arrival_date <- as.character(df$arrival_date)

# Membuat fungsi validasi tanggal
is_valid_date <- function(year, month, day) {
  tryCatch({
    as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Menerapkan fungsi validasi tanggal pada setiap baris
valid_dates <- apply(df, 1, function(row) is_valid_date(row['arrival_year'], row['arrival_month'], row['arrival_date']))

# Membuat copy dari dataframe dengan tanggal yang valid
df_valid <- df[valid_dates, ]

# Menggabungkan kolom tahun, bulan, dan tanggal kedatangan menjadi satu kolom tanggal kedatangan
df_valid$arrival_date <- as.Date(paste(df_valid$arrival_year, df_valid$arrival_month, df_valid$arrival_date, sep = "-"), format = "%Y-%m-%d")

# Menampilkan 5 baris pertama dari dataframe yang valid
head(df_valid)

# Menghapus kolom tahun dan bulan karena tidak diperlukan
df_valid <- df_valid %>% select(-arrival_year, -arrival_month)

# Menampilkan informasi tentang dataframe yang valid
str(df_valid)

# Mengecek nilai yang hilang
colSums(is.na(df_valid))

# Mengecek duplikasi data
duplicated_data <- sum(duplicated(df_valid))
print(paste("Jumlah data duplikasi:", duplicated_data))

# Statistik deskriptif
summary(df_valid)




# Definisikan UI
ui <- dashboardPage(
  dashboardHeader(title = "Hotel Reservations Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Visualisasi", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Author", tabName = "author", icon = icon("user"))
    )
  ),
  dashboardBody(
    tabItems(
      # Home tab
      tabItem(tabName = "home",
              tags$img(src = "https://i.pinimg.com/564x/24/e4/48/24e448e0f7a6214a2d18467738569ab4.jpg", 
                       style = "width: 100%; height: auto;"),
              h2("Selamat Datang di Dashboard Reservasi Hotel!!"),
              p("Dashboard ini memberikan visualisasi dan informasi mengenai dataset reservasi hotel."),
              
      ),
      
      # Dataset Explanation & Dataset tab
      tabItem(tabName = "dataset",
              # Menggunakan tabsetPanel di dalam tabItem "dataset"
              tabsetPanel(
                tabPanel("Deskripsi", 
                         h2("Penjelasan Dataset"),
                         p("Dataset ini berisi informasi mengenai reservasi hotel yang didapatkan dari website ", 
                           tags$a(href = "https://www.kaggle.com/datasets/ahsan81/hotel-reservations-classification-dataset?select=Hotel+Reservations.csv", 
                                  "Kaggle.com"), "."),
                         p("Berikut adalah penjelasan untuk setiap variabel dalam dataset ini:"),
                         tableOutput("dataset_description")
                ),
                tabPanel("Summary",
                         h2("Summary Dataset"),
                         fluidRow(
                           valueBoxOutput("total_bookings", width = 6),
                           valueBoxOutput("avg_price_per_room", width = 6),
                           valueBoxOutput("percent_repeated_guests", width = 6),
                           valueBoxOutput("cancelled_percentage", width=6),
                           valueBoxOutput("avg_lead_time", width=6),
                           valueBoxOutput("most_frequent_segment", width=6),
                           valueBoxOutput("top_room_type", width = 6),
                           valueBoxOutput("top_meal_plan", width = 6)
                         ),
                         h2("Summary Statistics"),
                         DT::DTOutput("summary_table")
                ),
                tabPanel("Datasets",
                         DT::DTOutput("data_table")  # Output untuk menampilkan dataset
                ),
                tabPanel("Struktur Data",
                         verbatimTextOutput("str_output")  # Output untuk struktur data
                )
              )
      ),
      
      # Visualization tab
      tabItem(tabName = "visualization",
              h2("Visualisasi"),
              fluidRow(
                box(
                  dateRangeInput("dateRange", 
                                 label = "Select Date Range:",
                                 start = min(df_valid$arrival_date, na.rm = TRUE), 
                                 end = max(df_valid$arrival_date, na.rm = TRUE),
                                 min = min(df_valid$arrival_date, na.rm = TRUE), 
                                 max = max(df_valid$arrival_date, na.rm = TRUE),
                                 format = "yyyy-mm-dd"
                                 
                  )
                ),
                width = 12
              ),
              fluidRow(
                box(plotOutput("line_plot_arrival_date"), width = 12)
              ),
              fluidRow(
                box(plotOutput("histogram_adults"), width = 6),
                box(plotOutput("histogram_children"), width = 6)
              ),
              fluidRow(
                box(plotOutput("bar_plot_meal_plan"), width = 6),
                box(plotOutput("pie_chart_market_segment"), width = 6)
              ),
              fluidRow(
                box(plotOutput("scatter_lead_time_price"), width = 12)
              ),
              fluidRow(
                box(plotOutput("pair_plot"), width = 12)
              ),
              fluidRow(
                box(plotOutput("boxplot_all_variables"), width = 12)
              )
      ),
      # Home tab
      tabItem(tabName = "author",
              h2("Kelompok 5"),
              fluidRow(
                column(4, align = "center",
                       img(src = "https://raw.githubusercontent.com/elsamaulida5/data-mining-c/main/FPdatmin/dini.png", height = "300px"),
                       h4("Dini Athirah Yasmin"),
                       p("5003211033")
                ),
                column(4, align = "center",
                       img(src = "https://raw.githubusercontent.com/elsamaulida5/data-mining-c/main/FPdatmin/ifa.png", height = "300px"),
                       h4("Tarbiyatul Latifah"),
                       p("5003211098")
                ),
                column(4, align = "center",
                       img(src = "https://raw.githubusercontent.com/elsamaulida5/data-mining-c/main/FPdatmin/elsa.png", height = "300px"),
                       h4("Elsa Maulida Pangesti"),
                       p("5003211101")
                )
              )
       
      )
    )
  )
)

# Definisikan server
server <- function(input, output) {
  # Render table output for dataset description
  output$dataset_description <- renderTable({
    # Define the table data structure
    dataset_table <- data.frame(
      Variabel = c("Booking_ID", "no_of_adults", "no_of_children", "no_of_weekend_nights", "no_of_week_nights",
                   "type_of_meal_plan", "required_car_parking_space", "room_type_reserved", "lead_time", "arrival_year",
                   "arrival_month", "arrival_date", "market_segment_type", "repeated_guest", "no_of_previous_cancellations",
                   "no_of_previous_bookings_not_canceled", "avg_price_per_room", "no_of_special_requests", "booking_status"),
      Deskripsi = c("Identifier unik dari setiap pemesanan.", "Jumlah dewasa yang menginap.", "Jumlah anak-anak yang menginap.",
                    "Jumlah malam akhir pekan (Sabtu atau Minggu) yang tamu menginap atau memesan untuk menginap di hotel.", 
                    "Jumlah malam kerja (Senin hingga Jumat) yang tamu menginap atau memesan untuk menginap di hotel.",
                    "Jenis rencana makan yang dipesan oleh pelanggan.", "Apakah pelanggan membutuhkan tempat parkir mobil? (0 - Tidak, 1 - Ya)",
                    "Jenis kamar yang dipesan oleh pelanggan.", "Jumlah hari antara tanggal pemesanan dan tanggal kedatangan.", 
                    "Tahun dari tanggal kedatangan.", "Bulan dari tanggal kedatangan.", "Tanggal dalam bulan dari tanggal kedatangan.",
                    "Penunjukan segmen pasar.", "Apakah pelanggan adalah tamu yang berulang? (0 - Tidak, 1 - Ya)", 
                    "Jumlah pemesanan sebelumnya yang dibatalkan oleh pelanggan sebelum pemesanan saat ini.",
                    "Jumlah pemesanan sebelumnya yang tidak dibatalkan oleh pelanggan sebelum pemesanan saat ini.",
                    "Harga rata-rata per hari dari reservasi; harga kamar dinamis (dalam euro).", 
                    "Total jumlah permintaan khusus yang dibuat oleh pelanggan (mis. lantai tinggi, pemandangan dari kamar, dll).", 
                    "Tanda menunjukkan apakah pemesanan dibatalkan atau tidak.")
    )
    return(dataset_table)
  })
  
  # Render the data table
  output$data_table <- renderDT({
    DT::datatable(
      df,
      options = list(
        pageLength = 10,  # Menampilkan 10 baris per halaman
        lengthMenu = c(10, 25, 50),  # Opsi jumlah baris per halaman
        ordering = TRUE,  # Memungkinkan pengurutan
        searching = TRUE,  # Memungkinkan pencarian
        scrollX = TRUE
      ),
      rownames = FALSE  # Menyembunyikan kolom nama baris
    )
  })
  
  # Render the total bookings valueBoxOutput
  output$total_bookings <- renderValueBox({
    valueBox(
      value = nrow(df_valid),  # Menggunakan df_valid atau dataframe yang sesuai
      subtitle = "Total Bookings",
      icon = icon("list-alt"),
      color = "aqua"
    )
  })
  
  # Render the average price per room valueBoxOutput
  output$avg_price_per_room <- renderValueBox({
    avg_price <- mean(df_valid$avg_price_per_room, na.rm = TRUE)
    valueBox(
      value = paste0("€", round(avg_price, 2)),
      subtitle = "Average Price per Room",
      icon = icon("euro-sign"),
      color = "aqua"
    )
  })
  
  # Render the top room type valueBoxOutput
  output$top_room_type <- renderValueBox({
    top_room <- names(which.max(table(df_valid$room_type_reserved)))
    valueBox(
      value = top_room,
      subtitle = "Top Room Type",
      icon = icon("bed"),
      color = "aqua"
    )
  })
  
  # Render the top meal plan valueBoxOutput
  output$top_meal_plan <- renderValueBox({
    top_meal <- names(which.max(table(df_valid$type_of_meal_plan)))
    valueBox(
      value = top_meal,
      subtitle = "Top Meal Plan",
      icon = icon("utensils"),
      color = "aqua"
    )
  })
  
  # Render the percentage of repeated guests valueBoxOutput
  output$percent_repeated_guests <- renderValueBox({
    percent_repeated <- mean(df_valid$repeated_guest == 1) * 100
    valueBox(
      value = paste0(round(percent_repeated, 2), "%"),
      subtitle = "Percentage Repeated Guests",
      icon = icon("user-check"),
      color = "aqua"
    )
  })
  
  #Render the average of lead time ValueBoxOutput
  output$avg_lead_time <- renderValueBox({
    avg_lead <- mean(df_valid$lead_time)
    valueBox(
      value = paste0(round(avg_lead, 2), " days"),
      subtitle = "Average Lead Time",
      icon = icon("clock"),
      color = "aqua"
    )
  })
  
  # Render Most Frequent Market Segment
  segment_counts <- table(df_valid$market_segment_type)
  most_frequent_segment <- names(which.max(segment_counts))
  # Menambahkan valueBox untuk menampilkan segmentasi pasar terbanyak
  output$most_frequent_segment <- renderValueBox({
    valueBox(
      value = most_frequent_segment,
      subtitle = "Most Frequent Market Segment",
      icon = icon("chart-pie"),
      color = "aqua"
    )
  })
  
  # Render persentase cancelled
  booking_counts <- table(df_valid$booking_status)
  percentage_cancelled <- (booking_counts["Canceled"] / sum(booking_counts)) * 100
  # Menambahkan valueBox untuk menampilkan persentase reservasi yang dibatalkan
  output$cancelled_percentage <- renderValueBox({
    valueBox(
      value = paste0(round(percentage_cancelled, 2), "%"),
      subtitle = "Percentage of Cancelled Bookings",
      icon = icon("times-circle"),
      color = "aqua"
    )
  })
  
  
  # Render the str output
  output$str_output <- renderPrint({
    str(df_valid)
  })
  
  # Render the summary statistics table for all numeric columns
  output$summary_table <- renderDT({
    numeric_vars <- df_valid %>% select(where(is.numeric))
    df_summary <- numeric_vars %>%
      reframe(
        Variable = colnames(numeric_vars),
        Mean = sapply(numeric_vars, mean, na.rm = TRUE),
        Median = sapply(numeric_vars, median, na.rm = TRUE),
        Mode = sapply(numeric_vars, function(x) as.numeric(names(sort(table(x), decreasing = TRUE)[1]))),
        StdDev = sapply(numeric_vars, sd, na.rm = TRUE),
        Variance = sapply(numeric_vars, var, na.rm = TRUE),
        Min = sapply(numeric_vars, min, na.rm = TRUE),
        Max = sapply(numeric_vars, max, na.rm = TRUE)
      )
    
    datatable(
      df_summary,
      options = list(
        scrollX = TRUE,
        scrollY = '400px',
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        )
      )
    )
  })
  
  #RENDER VISUALISASI
  # Reactive expression to filter data based on selected date range
  filtered_data <- reactive({
    df_filtered <- df_valid %>%
      filter(arrival_date >= as.Date(input$dateRange[1]) & arrival_date <= as.Date(input$dateRange[2]))
    return(df_filtered)
  })
  
  # Line Plot for number of reservations by arrival date
  output$line_plot_arrival_date <- renderPlot({
    df_arrival <- filtered_data() %>%
      group_by(arrival_date) %>%
      summarise(count = n())
    
    ggplot(df_arrival, aes(x = arrival_date, y = count)) +
      geom_line(color = "#365486") +
      labs(title = "Jumlah Reservasi Berdasarkan Tanggal Kedatangan", x = "Tanggal Kedatangan", y = "Jumlah Reservasi")
  })
  
  # Histogram for number of adults
  output$histogram_adults <- renderPlot({
    ggplot(filtered_data(), aes(x = no_of_adults)) +
      geom_histogram(binwidth = 1, fill = "#7FC7D9", color = "black") +
      labs(title = "Distribusi Jumlah Tamu Dewasa", x = "Jumlah Tamu Dewasa", y = "Frekuensi")
  })
  
  # Histogram for number of children
  output$histogram_children <- renderPlot({
    ggplot(filtered_data(), aes(x = no_of_children)) +
      geom_histogram(binwidth = 1, fill = "#7FC7D9", color = "black") +
      labs(title = "Distribusi Jumlah Anak-Anak", x = "Jumlah Anak-Anak", y = "Frekuensi")
  })
  
  # Bar Plot for meal plan types
  output$bar_plot_meal_plan <- renderPlot({
    df_meal_plan <- filtered_data() %>%
      count(type_of_meal_plan) %>%
      arrange(desc(n))  # Urutkan berdasarkan jumlah dari yang terbesar
    
    ggplot(df_meal_plan, aes(x = reorder(type_of_meal_plan, -n), y = n)) +
      geom_bar(stat = "identity", fill = "#7FC7D9", color = "black") +
      labs(title = "Bar Plot Jenis Rencana Makan", x = "Jenis Rencana Makan", y = "Frekuensi") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotasi label x jika diperlukan
  })
  
  # Pie Chart for market segment types
  output$pie_chart_market_segment <- renderPlot({
    df_segment <- filtered_data() %>%
      count(market_segment_type) %>%
      mutate(percent = n / sum(n) * 100)  # Hitung persentase
    
    # Buat pie chart
    ggplot(df_segment, aes(x = "", y = n, fill = market_segment_type)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = "Proporsi Segmentasi Pasar", y = "Proporsi") +
      scale_fill_manual(values = c("#0F1035", "#365486", "#4CB9E7", "#7FC7D9", "#AAD7D9")) +
      
      # Tambahkan label persentase di dalam pie chart
      geom_text(aes(label = paste0(round(percent, 1), "%")),
                position = position_stack(vjust = 0.5), size = 4, color = "black")+
     
      theme(
        axis.text = element_blank(),   # Hapus teks axis
      )
  })
  
  # Scatter Plot for correlation between lead time and average price per room
  output$scatter_lead_time_price <- renderPlot({
    ggplot(filtered_data(), aes(x = lead_time, y = avg_price_per_room)) +
      geom_point(alpha = 0.5, color = "#7FC7D9") +
      labs(title = "Korelasi antara Lead Time dan Harga Rata-Rata per Kamar", x = "Lead Time", y = "Harga Rata-Rata per Kamar")
  })
  
  # Pair Plot
  output$pair_plot <- renderPlot({
    # Select variables for pair plot
    variables_to_plot <- c("lead_time", "no_of_adults", "no_of_children", "no_of_weekend_nights", 
                           "no_of_week_nights", "avg_price_per_room", "no_of_special_requests") 
    
    ggpairs(filtered_data(),
            columns = variables_to_plot,
            # ... (other ggpairs arguments) 
            title = "Pair Plot of Selected Variables")
  })
  
  # Boxplot for all numerical variables
  output$boxplot_all_variables <- renderPlot({
    # Convert numerical variables to long format for faceting
    boxplot_data <- filtered_data() %>%
      select(booking_status, lead_time, no_of_adults, no_of_children, 
             no_of_weekend_nights, no_of_week_nights, avg_price_per_room, no_of_special_requests) %>%
      pivot_longer(cols = c("lead_time", "no_of_adults", "no_of_children", 
                            "no_of_weekend_nights", "no_of_week_nights", "avg_price_per_room", "no_of_special_requests"), 
                   names_to = "variable", values_to = "value")
    
    ggplot(boxplot_data, aes(x = booking_status, y = value, fill = booking_status, variable = variable)) +
      geom_boxplot() +
      facet_wrap(~ variable, scales = "free") +
      labs(title = "Boxplots of Numerical Variables by Booking Status", x = "Booking Status", y = "Value") +
      theme(legend.position = "top")
  })
  
}

# Jalankan aplikasi
shinyApp(ui, server)

