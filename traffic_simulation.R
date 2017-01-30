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
#           | O ¦ | |
#           | U ¦   |
#           | T ¦   |
#           | H ¦   |

## ASSUMING 60 CARS PER HOUR TRAVEL FROM EACH DIRECTION THROUGH THE INTERSECTION
## WE WOULD EXPECT THAT THERE WOULD BE 1 CAR PER MINUTE OR 1 / 60 CARS PER 
## SECOND - 

num_cars <- data.frame(arriving = c(0, 0, 0, 0), passing_thru = c(0, 0, 0, 0),
                       in_queue = c(0, 0, 0, 0), light = c("green", "green", 
                                                           "red", "red"), 
                       cars_per_second = c(30 / 60, 30 / 60, 30 / 60, 30 / 60))

num_cars$z_val <- qnorm(num_cars$cars_per_second)

secs_of_light <- 0

for(i in 1:3600){
        for(j in 1:dim(num_cars)[1]){
                samp <- rnorm(1)
                
                if(samp <= num_cars$z_val[j]){ ## CAR ARRIVES
                        num_cars$arriving[j] <- num_cars$arriving[j] + 1
                        
                        if(num_cars$light[j] == "green"){ ## LIGHT IS GREEN
                                num_cars$passing_thru[j] <- num_cars$passing_thru[j] + 1
                        } else {
                                num_cars$in_queue[j] <- num_cars$in_queue[j] + 1
                        }
                } else { ## NO CAR ARRIVES
                        if(num_cars$light[j] == "green") { ## LIGHT IS GREEN
                                if(num_cars$in_queue[j] > 0){
                                        num_cars$passing_thru[j] <- num_cars$passing_thru[j] + 1
                                        num_cars$in_queue[j] <- num_cars$in_queue[j] - 1
                                }
                        }
                }
                
                #print(paste("Second: ", i, "; Color of Light: ", light, 
                #            "; Number of Cars Arrived: ", num_cars_ariving, 
                #            "; Number of Cars Passed Through: ", num_cars_passing, 
                #            "; Number of Cars in Queue: ", num_cars_in_queue, sep = ""))
                
        }
        
        secs_of_light <- secs_of_light + 1 ## INCREASE HOW MANY SECONDS LIGHT HAS BEEN AT CURRENT LEVEL
        
        if(secs_of_light >= 30){
                secs_of_light <- 0
                
                num_cars$light <- ifelse(num_cars$light == "green", "red", "green")
        }
}

row.names(num_cars) <- c("north", "south", "east", "west")

print(num_cars)
