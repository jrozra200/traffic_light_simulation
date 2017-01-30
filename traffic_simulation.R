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

num_cars_ariving <- 0
num_cars_passing <- 0
num_cars_in_queue <- 0

secs_of_light <- 0
light <- "green"

cars_per_second <- 1/ 60 

z_val_cars_per_sec <- abs(qnorm(cars_per_second))

for(i in 1:180){
        samp <- abs(rnorm(1))
        
        if(samp >= z_val_cars_per_sec){ ## CAR ARRIVES
                num_cars_ariving <- num_cars_ariving + 1
                
                if(light == "green"){ ## LIGHT IS GREEN
                        num_cars_passing <- num_cars_passing + 1
                } else {
                        num_cars_in_queue <- num_cars_in_queue + 1
                }
        } else { ## NO CAR ARRIVES
                if(light == "green") { ## LIGHT IS GREEN
                        if(num_cars_in_queue > 0){
                                num_cars_passing <- num_cars_passing + 1
                                num_cars_in_queue <- num_cars_in_queue - 1
                        }
                }
        }
        
        print(paste("Second: ", i, "; Color of Light: ", light, 
                    "; Number of Cars Arrived: ", num_cars_ariving, 
                    "; Number of Cars Passed Through: ", num_cars_passing, 
                    "; Number of Cars in Queue: ", num_cars_in_queue, sep = ""))
        
        secs_of_light <- secs_of_light + 1 ## INCREASE HOW MANY SECONDS LIGHT HAS BEEN AT CURRENT LEVEL
        
        if(secs_of_light >= 30){
                secs_of_light <- 0
                
                light <- if(light == "green"){
                        "red"
                } else {
                        "green"
                }
        }
}
