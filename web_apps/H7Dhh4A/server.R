library(shiny)
library(mixdist)
library(ggplot2)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

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
         users_work_weeks[[i]] <- rweibull(work_week, shape = sh, scale = sc) * (runif(work_week)  < tod_work_week)
         users_off_hours[[i]]  <- rweibull(off_hours, shape = sh, scale = sc) * (runif(off_hours)  < tod_off_hours)
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


    
    
    

  output$plot1 <- renderPlot({
        num_users            <- input$numUsers
        work_week            <- input$workWeek
        off_hours            <- 168 - work_week
        mean_mem_req         <- input$memReq
        stddev_mem_req       <- input$stdDevMemReq
        tod_work_week        <- input$todWorkWeek
        tod_off_hours        <- input$todOffHours
        hour_cost_per_server <- input$hourCostPerServ
        server_size          <- input$serverSize
      
      
        weeks <- simulate_n_weeks(52)
        sorted_weeks <- lapply(weeks,sort,decreasing=FALSE)

        d <- data.frame(x = unlist(sorted_weeks), 
                grp = rep(c(1:length(weeks)),times = sapply(weeks,length)),
                hour = rep(1:168, times = length(weeks)))


        #ggplot(d, aes(hour, x, group = grp)) + geom_point(colour = "steelblue", alpha = .02)

        hist(d$x,breaks = seq(0,max(d$x)+server_size, by = server_size), xlab="Memory load binned by servers required") 
    
  })
})