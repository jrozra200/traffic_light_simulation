## SIMULATING TRAFFIC AT THE INTERSECTION OF TWO ROADS WITH EQUAL TRAFFIC ##

## TRAFFIC FLOWS IN THE DIRECTIONS OF THE ARROWS
## THE NAME OF THE ROAD IS WRITTEN AND DIRECTIONAL

#           |   ¦ N |
#           |   ¦ O |
#           |   ¦ R |
#           | | ¦ T |
# __________| ∨ ¦ H |______________
#                   <-<- EAST
# - - - - - -       - - - - - - - -
#  WEST ->->
# ￣￣￣￣￣| S ¦ ∧ |￣￣￣￣￣￣￣
#          | O ¦ | |
#          | U ¦   |
#          | T ¦   |
#          | H ¦   |

## ASSUMING 60 CARS PER HOUR TRAVEL FROM EACH DIRECTION THROUGH THE INTERSECTION
## WE WOULD EXPECT THAT THERE WOULD BE 1 CAR PER MINUTE OR 1 / 60 CARS PER 
## SECOND - 

traffic_sim <- function(green_red_length = 90, yellow_length = 3, 
                        length_of_simulation_in_seconds = 3600, 
                        cars_per_second = 20 / 60) {
        
        num_cars <- data.frame(arriving = c(0, 0, 0, 0), 
                               passing_thru = c(0, 0, 0, 0),
                               in_queue = c(0, 0, 0, 0), 
                               total_weight_sec = c(0, 0, 0, 0), 
                               light = c("green", "green", "red", "red"), 
                               cars_per_second = c(cars_per_second, 
                                                   cars_per_second, 
                                                   cars_per_second, 
                                                   cars_per_second))
        
        num_cars$z_val <- qnorm(num_cars$cars_per_second)
        
        is_yellow <- 0
        secs_of_light <- 0
        
        for(i in 1:length_of_simulation_in_seconds){
                for(j in 1:dim(num_cars)[1]){
                        samp <- rnorm(1)
                        
                        if(samp <= num_cars$z_val[j]){ ## CAR ARRIVES
                                num_cars$arriving[j] <- num_cars$arriving[j] + 1
                                
                                if(num_cars$light[j] == "green" & 
                                   is_yellow == 0){ ## LIGHT IS GREEN
                                        num_cars$passing_thru[j] <- (
                                                num_cars$passing_thru[j] + 1)
                                } else { ## LIGHT IS RED
                                        num_cars$in_queue[j] <- (
                                                num_cars$in_queue[j] + 1)
                                }
                        } else { ## NO CAR ARRIVES
                                if(num_cars$light[j] == "green" & 
                                   is_yellow == 0) { ## LIGHT IS GREEN
                                        if(num_cars$in_queue[j] > 0){
                                                num_cars$passing_thru[j] <- (
                                                        num_cars$passing_thru[j] 
                                                        + 1)
                                                num_cars$in_queue[j] <- (
                                                        num_cars$in_queue[j] - 
                                                                1)
                                        }
                                }
                        }
                        
                        num_cars$total_weight_sec[j] <- (
                                num_cars$total_weight_sec[j] + 
                                        num_cars$in_queue[j])
                        
                }
                
                ## INCREASE HOW MANY SECONDS LIGHT HAS BEEN AT CURRENT LEVEL
                secs_of_light <- secs_of_light + 1 
                
                if (secs_of_light >= green_red_length & secs_of_light < 
                    (green_red_length + yellow_length)){
                        is_yellow <- 1
                } else if (secs_of_light >= (green_red_length + yellow_length)){
                        secs_of_light <- 0
                        is_yellow <- 0
                        num_cars$light <- ifelse(num_cars$light == "green", 
                                                 "red", "green")
                }
        }
        
        row.names(num_cars) <- c("north", "south", "east", "west")
        
        print(num_cars)
        
}
