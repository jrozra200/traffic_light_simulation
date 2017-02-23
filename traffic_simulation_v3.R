##########################
##########################
## SIMULATION VARIABLES ##
##########################
##########################

length_of_sim_sec <- 600
cars_per_second <- 20 / 60
speed_limit_mph <- 35
max_speed_mph <- 50
min_time_between_cars <- 2
cars <- data.frame()
light <- data.frame(color = "green", sec_at_cur_lev = 0)

#########################
#########################
## ASSISTANT FUNCTIONS ##
#########################
#########################

can_a_car_be_added <- function(cars){
        if(dim(cars[cars$seconds_in_sim < min_time_between_cars, ])[1] == 0){
                return(TRUE)
        } else {
                return(FALSE)
        }
}

try_to_add_car <- function(cars, cars_per_second){
        ## TAKE A SAMPLE FROM A NORMAL DISTRIBUTION
        samp <- rnorm(1)
        
        if(samp <= qnorm(cars_per_second)) {
                car_index <- ifelse(dim(cars)[1] == 0, 1, max(cars$index) + 1)
                car_max_speed <- get_max_speed(speed_limit_mph, max_speed_mph)
                car_current_speed <- get_initial_speed(cars, car_index, 
                                                       car_max_speed)
                car_seconds_in_sim <- 0
                car_xposition <- 0
                
                tmp <- data.frame(index = car_index, max_speed = car_max_speed,
                                  current_speed = car_current_speed, 
                                  seconds_in_sim = car_seconds_in_sim,
                                  xposition = car_xposition)
                cars <- rbind(cars, tmp)
        }
        
        return(cars)
}

get_max_speed <- function(speed_limit_mph, max_speed_mph){
        st_dev <- (speed_limit_mph - max_speed_mph) / qnorm(0.005)
        
        return(rnorm(1, speed_limit_mph, st_dev))
}

get_initial_speed <- function(cars, car_index, car_max_speed, light){
        car_current_speed <- if(car_index == 1){ 
                ## ... AND IF THIS IS THE FIRST CAR IN THE SIMULATION ...
                
                car_max_speed ## ... THEN ITS CURRENT SPEED IS ITS MAX SPEED
        } else if (cars$current_speed[car_index - 1] < car_max_speed){ 
                ## ... AND IT IS NOT THE FIRST CAR IN THE SIMULATION AND THE 
                ## NEXT CAR IS SLOWER THAN YOU ...
                
                if(((car_max_speed * 5280) / 1800) > cars$xposition[car_index - 1]){
                        ## ... AND THERE IS LESS THAN 2 SECONDS BETWEEN CARS ...
                        
                        ## ... CHANGE SPEED TO MAINTAIN A 2 SECOND BUFFER
                        (1800 * cars$xposition[car_index - 1]) / 5280  
                } else {
                        ## ... AND THERE IS MORE THAN A 2 SECOND BUFFER BETWEEN 
                        ## CARS ...
                        
                        car_max_speed ## MAINTAIN THE MAXIMUM SPEED
                }
        } else {
                ## ... AND THE CAR IN FRONT IS GOING FASTER THAN YOU ...
                car_max_speed
        }
}


###########################
## INITIALIZE SIMULATION ##
###########################

for(sec in 1:length_of_sim_sec){
        ## CAN A CAR BE ADDED? IF SO, TRY. IF NOT, DON'T
        cars <- if(can_a_car_be_added(cars)){
                try_to_add_car(cars, cars_per_second)
        } else {
                cars
        }
        
        cars$xposition <- cars$xposition + ((cars$current_speed * 5280) / 3600)
        cars$seconds_in_sim <- cars$seconds_in_sim + 1
        light$sec_at_cur_lev <- light$sec_at_cur_lev + 1
}
