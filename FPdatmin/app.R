library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(lubridate)
library(DT)
library(shinythemes)
library(GGally)
library(tidyr)
library(caret)
library(rpart)
library(e1071)
library(randomForest)
library(RColorBrewer)
library(scales)

# Membaca data dari file CSV
df <- read.csv("https://raw.githubusercontent.com/elsamaulida5/data-mining-c/main/FPdatmin/Hotel%20Reservations.csv")

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

# Menghapus kolom tahun dan bulan karena tidak diperlukan
df_valid <- df_valid %>% select(-Booking_ID, -arrival_year, -arrival_month)

# Menampilkan informasi tentang dataframe yang valid
str(df_valid)

# Mengecek nilai yang hilang
colSums(is.na(df_valid))

# Mengecek duplikasi data
duplicated_data <- sum(duplicated(df_valid))
print(paste("Jumlah data duplikasi:", duplicated_data))

# Statistik deskriptif
summary(df_valid)

################################################################################
# Definisikan UI
ui <- dashboardPage(
  dashboardHeader(title = "Hotel Reservations Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Visualisasi", tabName = "visualization", icon = icon("chart-bar")),
      menuItem("Prediksi", tabName = "prediksi", icon = icon("chart-line")),
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
                box(plotOutput("pair_plot"), width = 12)
              ),
              tabsetPanel(
                tabPanel("Histogram", 
                         h2("Histogram"),
                         fluidRow(
                           box(
                             selectInput("hist_category", "Select variable:",
                                         choices = names(df_valid)[sapply(df_valid, is.numeric)]),
                           ),
                           box(plotOutput("histogram"), width = 12)
                         )
                ),
                tabPanel("Barplot",
                         h2("Barplot"),
                         fluidRow(
                           box(
                             selectInput("bar_category", "Select Category Variable:",
                                         choices = names(df_valid)[sapply(df_valid, is.character)]),
                             width = 12
                           ),
                           box(plotOutput("bar_plot"), width = 12)
                         )
                ),
                tabPanel("Boxplot",
                         h2("Boxplot"),
                         fluidRow(
                           box(
                             selectInput("box_category", "Select variable:",
                                         choices = names(df_valid)[sapply(df_valid, is.numeric)]),
                           ),
                           box(plotOutput("box_plot"), width = 12)
                         )
                ),
                tabPanel("Pie chart",
                         h2("Pie Chart"),
                         fluidRow(
                           box(
                             selectInput("pie_category", "Select Category Variable:",
                                         choices = names(df_valid)[sapply(df_valid, is.character)]),
                             width = 12
                           ),
                           box(plotOutput("pie_chart"), width = 12)
                         )
                ),
                tabPanel("Scatterplot",
                         h2("Scatterplot"),
                         fluidRow(
                           box(
                             selectInput("scatter_x", "Select X-axis variable:",
                                         choices = names(df_valid)[sapply(df_valid, is.numeric)]),
                             selectInput("scatter_y", "Select Y-axis variable:",
                                         choices = names(df_valid)[sapply(df_valid, is.numeric)]),
                             width = 12
                           ),
                           box(plotOutput("scatter_plot"), width = 12)
                         )
                )
              )
      ),
      
      #Prediction Tab
      tabItem(tabName = "prediksi",
              h2("Prediksi Reservasi Hotel"),
              fluidRow(
                column(3,
                       numericInput("no_of_week_nights", "Jumlah Malam:", value = 1, min = 0),
                       numericInput("lead_time", "Lead Time:", value = 1, min = 0),
                       selectInput("market_segment_type", "Tipe Segmen Pasar:",
                                   choices = c('Offline' = 0, 'Online' = 1, 'Corporate' = 2, 'Aviation' = 3, 'Complementary' = 4)),
                       numericInput("avg_price_per_room", "Harga per Kamar:", value = 100, min = 0),
                       numericInput("no_of_special_requests", "Jumlah Permintaan Khusus:", value = 0, min = 0),
                       actionButton("predict", "Prediksi")
                ),
                column(9,
                       uiOutput("prediction_result")
                )
              )
      ),
      
      # Author tab
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
    req(input$dateRange)  # Ensure dateRange input exists
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
  
  #Scatter plot 
  output$scatter_plot <- renderPlot({
    ggplot(filtered_data(), aes_string(x = input$scatter_x, y = input$scatter_y)) +
      geom_point(alpha = 0.5, color = "#7FC7D9") +
      labs(title = paste("Scatter Plot of", input$scatter_x, "vs", input$scatter_y))
  })
  
  # Histogram
  output$histogram <- renderPlot({
    df_filtered <- filtered_data()
    ggplot(df_filtered, aes_string(x = input$hist_category)) +
      geom_histogram(binwidth = 1, fill = "#7FC7D9", color = "black") +
      labs(title = paste("Histogram of", input$hist_category), x = input$hist_category, y = "Count")
  })
  
  # Box plot
  output$box_plot <- renderPlot({
    df_filtered <- filtered_data()
    ggplot(df_filtered, aes_string(y = input$box_category)) +
      geom_boxplot(fill = "#7FC7D9") +
      labs(title = paste("Box Plot of", input$box_category), y = input$box_category)
  })
  
  # Pie chart
  output$pie_chart <- renderPlot({
    req(input$pie_category)  # Ensure pie_category input exists
    
    df_select <- filtered_data() %>%
      group_by(across(all_of(input$pie_category))) %>%
      summarise(count = n()) %>%
      mutate(percent = count / sum(count) * 100,
             label = paste0(round(percent, 1), "%"))
    
    print(head(df_select))  # Debugging line to print df_select
    
    if (nrow(df_select) == 0) {
      return(NULL)  # Return NULL if the data is empty
    }
    
    df_select[[input$pie_category]] <- as.factor(df_select[[input$pie_category]])  # Convert to factor
    
    ggplot(df_select, aes(x = "", y = count, fill = .data[[input$pie_category]])) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      labs(title = paste("Pie Chart of", input$pie_category), y = "Proportion") +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4, color = "black") +
      theme_void() +
      theme(legend.position = "right") +  # Position legend
      scale_fill_brewer(palette = "Blues")
  })
  
  # Bar plot
  output$bar_plot <- renderPlot({
    df_select <- filtered_data() %>%
      group_by(.data[[input$bar_category]]) %>%
      summarise(count = n()) %>%
      arrange(desc(count))  # Sort by count in descending order
    
    ggplot(df_select, aes(x = reorder(.data[[input$bar_category]], -count), y = count)) +
      geom_bar(stat = "identity", fill = "#7FC7D9", color = "black") +
      labs(title = paste("Bar Plot of", input$bar_category), x = input$bar_category, y = "Frequency") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed
  })
  
  #RENDER PREDICTION
  # Data transformation
  df_valid <- df_valid %>%
    mutate(
      booking_status = recode(booking_status, 'Not_Canceled' = 0, 'Canceled' = 1),
      type_of_meal_plan = recode(type_of_meal_plan, 'Not Selected' = 0, 'Meal Plan 1' = 1, 'Meal Plan 2' = 2, 'Meal Plan 3' = 3),
      room_type_reserved = recode(room_type_reserved, 'Room_Type 1' = 1, 'Room_Type 2' = 2, 'Room_Type 3' = 3, 'Room_Type 4' = 4, 'Room_Type 5' = 5, 'Room_Type 6' = 6, 'Room_Type 7' = 7),
      market_segment_type = recode(market_segment_type, 'Offline' = 0, 'Online' = 1, 'Corporate' = 2, 'Aviation' = 3, 'Complementary' = 4)
    )
  
  #Fixing Imbalance
  # Identifying majority and minority classes
  class_counts <- table(df_valid$booking_status)
  majority_class <- names(which.max(class_counts))
  minority_class <- names(which.min(class_counts))
  # Splitting the data into majority and minority classes
  df_majority <- df_valid %>% filter(booking_status == majority_class)
  df_minority <- df_valid %>% filter(booking_status == minority_class)
  # Oversampling the minority class
  set.seed(0)
  df_minority_oversampled <- df_minority[sample(1:nrow(df_minority), nrow(df_majority), replace = TRUE), ]
  # Combining the majority class data with the oversampled minority class data
  df_resampled <- bind_rows(df_majority, df_minority_oversampled)
  # Checking class distribution after resampling
  labels_dist_resampled <- table(df_resampled$booking_status)
  labels_dist_percentage_resampled <- round((labels_dist_resampled / sum(labels_dist_resampled)) * 100, 2)
  
  # Prepare the data for machine learning
  set.seed(42)
  df_valid <- df_valid %>% mutate(
    booking_status = as.factor(booking_status),
    market_segment_type = as.factor(market_segment_type)  # Ensure market_segment_type is a factor
  )
  
  # Define features and target variable
  features <- c('no_of_week_nights', 'lead_time', 'market_segment_type', 'avg_price_per_room', 'no_of_special_requests')
  target <- 'booking_status'
  
  # Create training and testing datasets
  index <- createDataPartition(df_valid[[target]], p = 0.8, list = FALSE)
  train_data <- df_valid[index, ]
  
  # Ensure factor levels match between train_data and new_data
  levels_market_segment_type <- levels(train_data$market_segment_type)
  
  model <- randomForest(booking_status ~ ., data = train_data[, c(features, target)], ntree = 100)
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      no_of_week_nights = as.integer(input$no_of_week_nights),
      lead_time = as.integer(input$lead_time),
      market_segment_type = factor(input$market_segment_type, levels = levels_market_segment_type),  # Set levels explicitly
      avg_price_per_room = as.numeric(input$avg_price_per_room),
      no_of_special_requests = as.integer(input$no_of_special_requests)
    )
    
    probabilities <- predict(model, new_data, type = "prob")
    
    output$prediction_result <- renderUI({
      cancel_prob <- round(probabilities[1, "1"] * 100, 2)
      not_cancel_prob <- round(probabilities[1, "0"] * 100, 2)
      
      fluidRow(
        column(width = 6,
               box(
                 title = "Hasil Prediksi",
                 status = "primary",
                 solidHeader = TRUE,
                 width = 12,
                 p("Kemungkinan Reservasi Dibatalkan:",
                   tags$b(style = "font-size: 20px;", paste0(cancel_prob, "%"))),
                 p(style = "margin-top: 20px;", "Kemungkinan Reservasi Tidak Dibatalkan:",
                   tags$b(style = "font-size: 20px;", paste0(not_cancel_prob, "%")))
               )
        )
      )
    })
  })
}


# Jalankan aplikasi
shinyApp(ui, server)

names(df_valid)

unique(df_valid$room_type_reserved)
