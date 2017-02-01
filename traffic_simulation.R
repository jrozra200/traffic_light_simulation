######################## 
##    JACOB ROZRAN    ##
## TRAFFIC SIMULATION ##
########################

## TRAFFIC FLOWS IN THE DIRECTIONS OF THE ARROWS
## THE NAME OF THE ROAD IS WRITTEN AND DIRECTIONAL

#           |   ¦ N |
#           |   ¦ O |
#           |   ¦ R |
#           | | ¦ T |
# __________| ∨ ¦ H |__________
#                   <-<- EAST
# - - - - - -       - - - - - -
#  WEST ->->
# __________  S ¦ ∧  __________
#           | O ¦ | |
#           | U ¦   |
#           | T ¦   |
#           | H ¦   |


traffic_sim_static_light_length <- function(green_red_length_seconds = 90, 
                                            yellow_length_seconds = 3, 
                                            length_of_simulation_in_seconds = 3600, 
                                            cars_per_second = c(20/60, 20/60, 
                                                                20/60, 20/60)) {
        
        ######################################
        ######################################
        ## GENERAL NOTE ABOUT THIS FUNCTION ##
        ######################################
        ######################################
        
        ## THIS FUNCTION SIMULATES CARS TRAVELING THROUGH A SIMPLE INTERSECTION.
        ## green_red_length_seconds IS THE LENGTH OF THE RED/GREEN LIGHT IN 
        ## SECONDS - IN THIS SIMULATION, IT IS THE SAME IN EACH DIRECTION; 
        ## yellow_length_seconds IS THE LENGTH OF THE YELLOW LIGHT IN SECONDS;
        ## length_of_simulation_in_seconds IS HOW LONG THE SIMULATION RUNS FOR 
        ## IN SECONDS; cars_per_second IS THE AVERAGE NUMBER OF CARS THAT ARRIVE
        ## AT THE INTERSECTION EACH SECOND. 20 CARS PER HOUR TRANSLATES TO 1/3 
        ## (OR 20/60) CARS PER SECOND. ALL OF THESE CAN BE CHANGED AS NEEDED.
        
        ############################################################
        ## INITIALIZE THE DATA AT THE BEGINNING OF THE SIMULATION ##
        ############################################################
        
        num_cars <- data.frame(arriving = c(0, 0, 0, 0), ## NO CARS HAVE ARRIVED ON ANY ROAD YET
                               passing_thru = c(0, 0, 0, 0), ## NO CARS HAVE GONE THROUGH THE LIGHT YET
                               in_queue = c(0, 0, 0, 0), ## NO CARS ARE IN QUEUE AT ANY ROAD YET
                               total_wait_sec = c(0, 0, 0, 0), ## THERE HAS BEEN NO WAIT TIME YET
                               light = c("green", "green", "red", "red"), 
                               cars_per_second = cars_per_second)
        
        ## I AM SAMPLING FROM A NORMAL DISTRIBUTION FOR THE SIMULATION - 
        ## DEPENDING ON HOW MANY CARS PER SECOND ARE SPECIFIED, WE NEED TO KNOW
        ## THE CORRESPONDING Z-VALUE THAT THE RANDOM SAMPLE MUST BE MORE 
        ## EXTREME THAN - THIS ESTABLISHES THAT Z-VALUE.
        num_cars$z_val <- qnorm(num_cars$cars_per_second)
        
        is_yellow <- 0 ## IS THE LIGHT YELLOW? THIS STARTS AS NO
        secs_of_light <- 0 ## THIS IS A COUNTER THAT DETERMINES HOW LONG A LIGHT 
                           ## HAS BEEN THE AT ITS CURRENT STATE
        
        ############################
        ## RUNNING THE SIMULATION ##
        ############################
        
        ## THE LOOP RUNS ONCE PER SECOND SIMULATED
        ## THE LENGTH OF THE SIMULATION IS ESTABLISHED AT FUNCTION CALL
        for(i in 1:length_of_simulation_in_seconds){
                
                ## IN EACH SECOND OF THE SIMULATION, THERE CAN BE CARS 
                ## APPROACHING ON EACH ROAD - HERE WE SIMULATE THE SECOND ON 
                ## EACH ROAD INDIVIDUALLY (DEFAULTS CURRENTLY TO 4 ROADS)
                for(j in 1:dim(num_cars)[1]){
                        samp <- rnorm(1) ## TAKE A SAMPLE FROM A NORMAL DISTRIBUTION
                        
                        ## IF OUR SAMPLE WE PULLED BEFORE IS LESS THAN THE Z-VAL
                        ## WE ESTABLISHED UPON INITIALIZATION BASED ON THE MEAN
                        ## NUMBER OF CARS PER SECOND WE EXPECT, THIS SIMULATES 
                        ## A CAR APPROACHING. DEPENDING ON THE COLOR OF THE 
                        ## LIGHT IN THAT DIRECTION AND THE AMOUNT OF CARS IN 
                        ## QUEUE, DIFFERENT OUTCOMES ARE PRODUCED.
                        if(samp <= num_cars$z_val[j]){ ## CAR ARRIVES
                                ## ADD ONE TO THE NUMBER OF CARS ARRIVING
                                num_cars$arriving[j] <- num_cars$arriving[j] + 1
                                
                                if(num_cars$light[j] == "green" & 
                                   is_yellow == 0){ ## & THE LIGHT IS GREEN
                                        ## A CAR PASSES THROUGH - ADD ONE
                                        num_cars$passing_thru[j] <- (
                                                num_cars$passing_thru[j] + 1)
                                } else { ## & LIGHT IS RED
                                        ## A CAR IS ADDED TO THE QUEUE LENGTH
                                        num_cars$in_queue[j] <- (
                                                num_cars$in_queue[j] + 1)
                                }
                        } else { ## NO CAR ARRIVES
                                if(num_cars$light[j] == "green" & 
                                   is_yellow == 0) { ## & LIGHT IS GREEN
                                        if(num_cars$in_queue[j] > 0){ ## & THERE ARE CARS IN QUEUE
                                                ## A CAR PASSES THROUGH 
                                                num_cars$passing_thru[j] <- (
                                                        num_cars$passing_thru[j] 
                                                        + 1)
                                                ## AND THE QUEUE LENGTH DECREASES
                                                num_cars$in_queue[j] <- (
                                                        num_cars$in_queue[j] - 
                                                                1)
                                        }
                                }
                        }
                        
                        ## FOR EACH CAR IN THE QUEUE THIS SECOND, ADD ONE SECOND
                        ## TO THE TOTAL WAIT TIME
                        num_cars$total_wait_sec[j] <- (
                                num_cars$total_wait_sec[j] + 
                                        num_cars$in_queue[j])
                        
                }
                
                ## INCREASE HOW MANY SECONDS LIGHT HAS BEEN AT CURRENT LEVEL
                secs_of_light <- secs_of_light + 1 
                
                ## IF THE NUMBER OF SECONDS THE LIGHT HAS BEEN AT THE CURRENT
                ## LEVEL IS GREATER THAN THE LENGTH OF THE LIGHT, CHANGE IT TO A 
                ## YELLOW LIGHT. AFTER THE YELLOW LIGHT PERIOD EXPIRES, RESET
                ## THE secs_of_light AND CHANGE THE COLOR OF light.
                if (secs_of_light >= green_red_length_seconds & secs_of_light < 
                    (green_red_length_seconds + yellow_length_seconds)){
                        is_yellow <- 1
                } else if (secs_of_light >= (green_red_length_seconds + 
                                             yellow_length_seconds)){
                        secs_of_light <- 0
                        is_yellow <- 0
                        num_cars$light <- ifelse(num_cars$light == "green", 
                                                 "red", "green")
                }
        }
        
        ## RENAME THE ROWS OF THE DATA FRAME TO THEIR "REAL" NAMES
        row.names(num_cars) <- c("north", "south", "east", "west")
        
        ## OUTPUT THE SIMULATION RESULTS
        print(num_cars)
        
}
