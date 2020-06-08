library(shiny)
library(mixdist)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  model <- eventReactive({input$numberUsers
                          input$workWeek
                          input$memReq
                          input$stdDevMemReq
                          input$todWorkWeek
                          input$todOffHours
                          input$hourCostPerServ
                          input$serverSize 
                         }, {

     num_users            <- input$numberUsers
     work_week            <- input$workWeek
     off_hours            <- 168 - work_week
     mean_mem_req         <- input$memReq
     stddev_mem_req       <- input$stdDevMemReq
     tod_work_week        <- input$todWorkWeek
     tod_off_hours        <- input$todOffHours
     hour_cost_per_server <- as.numeric(input$hourCostPerServ)
     server_size          <- as.numeric(input$serverSize)

     simulate_one_week <- function(num_users, 
                              work_week, 
                              off_hours, 
                              mean_mem_req, 
                              stddev_mem_req, 
                              tod_work_week, 
                              tod_off_hours){
        users_work_weeks <- list()
        users_off_hours  <- list()
        wbp <- weibullpar(mean_mem_req, stddev_mem_req)
        sh <- wbp$shape[[1]]
        sc <- wbp$scale[[1]]

        n <- c(1:num_users)
        # chosen weibull distribution

        for (i in n){
             users_work_weeks[[i]] <- rweibull(work_week, shape = sh, scale = sc) * (runif(work_week)  < tod_work_week/100)
             users_off_hours[[i]]  <- rweibull(off_hours, shape = sh, scale = sc) * (runif(off_hours)  < tod_off_hours/100)
        }
        return(list(users_work_weeks, users_off_hours))
    }

      
      
    simulate_n_weeks <- function(weeks){
        all_weeks <- list()
        n <- c(1:weeks)
        for (i in n){
            sims <- simulate_one_week(num_users, work_week, off_hours, mean_mem_req, stddev_mem_req, tod_work_week, tod_off_hours)
            users_work_week_consumption <- Reduce(`+`, sims[[1]])
            users_off_hours_consumption <- Reduce(`+`, sims[[2]])
            total_consumption <- append(users_work_week_consumption, users_off_hours_consumption)
            all_weeks[[i]] <- total_consumption
        }
        return(all_weeks)
    }
      
      
    weeks <- simulate_n_weeks(52)
    sorted_weeks <- lapply(weeks,sort,decreasing=FALSE)

    d <- data.frame(x = unlist(sorted_weeks), 
            grp = rep(c(1:length(weeks)),times = sapply(weeks,length)),
            hour = rep(1:168, times = length(weeks)))
    #ggplot(d, aes(hour, x, group = grp)) + geom_point(colour = "steelblue", alpha = .02)
    histogram = hist(d$x,breaks = seq(0,max(d$x)+server_size, by = server_size),labels = TRUE, main = "Hourly Server load for 1 Year", xlab="Memory load (GB) binned by servers required", ylab="Number of hours in year")
    # return all object as a list
      
    tc_table <- table(cut(d$x, breaks = seq(0,max(d$x)+server_size, by = server_size),  include.lowest = TRUE))
    tc_table

    servers <- data.frame(tc_table)
    servers$num_servers <- seq.int(nrow(servers))
    servers$cost <- servers$Freq * servers$num_servers * hour_cost_per_server

    max_servers = max(servers$num_servers)
    mean_servers = mean(sum(servers$Freq * servers$num_servers)/(24*7*52))
    min_servers = min(servers$num_servers)
      
    total_cost = sum(servers$cost)

    mean_memory = mean(d$x)
    median_memory = median(d$x)
    max_memory = max(d$x)
    min_memory = min(d$x)
    list(histogram = histogram, 
         total_cost = total_cost,
         max_servers = max_servers,
         mean_servers = mean_servers,
         min_servers = min_servers,
         max_memory = max_memory,
         mean_memory = mean_memory,
         median_memory = median_memory,
         min_memory = min_memory
)
  })
    


  output$distPlot <- renderPlot({
        model()$histogram
  })
    
  output$ggPlot <- renderPlot({
       #ggplot(d, aes(hour, x, group = grp)) + geom_point(colour = "steelblue", alpha = .02)
        })
  output$tc <- renderText({
          print(paste0("total cost: $",model()$total_cost))
      })
    output$max_servers <- renderText({
          print(paste0("max servers: ",model()$max_servers))
      })
    
    output$mean_servers <- renderText({
          print(paste0("mean servers: ",model()$mean_servers))
      })
      output$median_servers <- renderText({
          print(paste0("median servers: ",model()$median_servers))
      })
      output$min_servers <- renderText({
          print(paste0("min servers: ",model()$min_servers))
      })
      output$max_memory <- renderText({
          print(paste0("max memory: ",model()$max_memory))
      })
      output$mean_memory <- renderText({
          print(paste0("mean memory: ",model()$mean_memory))
      })
      output$median_memory <- renderText({
          print(paste0("median memory: ",model()$median_memory))
      })
      output$min_memory <- renderText({
          print(paste0("min memory: ",model()$min_memory))
      })
    
})