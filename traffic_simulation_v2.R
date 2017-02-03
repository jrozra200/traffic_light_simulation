
## SPEED IS NORMALLY DISTRIBUTED
## TO DETERMINE THE STANDARD DEVIATION, I MAKE AN APPROXIMATION ON THE MINIMUM 
## SPEED; THE SPEED THAT 99% OF PEOPLE GO FASTER THAN.
## REFERENCE: http://safety.fhwa.dot.gov/speedmgt/ref_mats/fhwasa10001/#c5.1
simulate_speed <- function(mean_speed = 35, min_speed = 15){
        st_dev <- (min_speed - mean_speed) / qnorm(0.005)
        
        return(rnorm(1, 35, st_dev))
}

simulate_car <- function(cars_per_hour = 60){
        cars_per_second <- cars_per_hour / 3600
        
        rand_samp <- rnorm(1)
        
        car_enters <- if(rand_samp <= qnorm(cars_per_second)){
                TRUE
        } else {
                FALSE
        }
        
        return(car_enters)
}

light_change <- function(current_color, secs_at_current){
        max_light_secs <- 90
        max_yellow_secs <- 4
        
        new_color, new_secs <- if(secs_at_current < max_light_secs){
                current_color, secs_at_current + 1
        } else if((secs_at_current >= max_light_secs) & 
                  (secs_at_current < (max_light_secs + max_yellow_secs))) {
                
        }
}

initialize_car <- function(cars_on_road){
        tmppos <- dim(cars_on_road)[1] + 1
        tmpspeed <- simulate_speed()
        tmpchgspd <- change_speed(tmpspeed, 0, tmppos, cars_on_road)
        
        tmp <- data.frame(position = tmppos,
                          max_speed = tmpspeed,
                          current_speed = tmpchgspd,
                          x_coord = 0)
        
        return(tmp)
}

## stopping distance: https://www.reference.com/vehicles/cars-traveling-35-mph-need-approximately-feet-stop-48861845e53dcf20
## AVERAGE DECELERATION RATE IS 15 f/s^2
## http://nacto.org/docs/usdg/vehicle_stopping_distance_and_time_upenn.pdf


change_speed <- function(current_speed, current_x_coord, current_position, 
                         cars_on_road, light_color){
        
        dist_to_next_car <- if(is.null(cars_on_road$x_coord[current_position - 1])){
                NA
        } else {
                cars_on_road$x_coord[current_position - 1] - current_x_coord
        }
        
        speed_dif <- if(is.null(cars_on_road$x_coord[current_position - 1])){
                NA
        } else {
                current_x_coord - cars_on_road$current_speed[current_position - 1]
        }
        
        ## LIGHT IS AT THE QUARTER MILE MARK
        dist_to_light <- 1320 - current_x_coord
        
        dist_to_stop <- ((current_speed * 5280) / 3600) / 15
        
        chg_spd <- if((current_speed > cars_on_road$current_speed[current_position - 1]) & 
                      ((cars_on_road$x_coord[current_position - 1] - current_x_coord) < 960)){
                
        }
}

################################################################################

cars_on_road <- data.frame()

for(i in 1:60){
        if(simulate_car()){
                tmp <- initialize_car(cars_on_road)
        } 
        
        
}
