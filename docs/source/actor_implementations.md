# Actor Implementations
Here we introduce you to the implmentened actors and their behaviours


# Normal Summa (No Distributed)

## Summa Actor
This is the top level actor that is spawned after the user invokes the program.
This actor is used to control job actors and batch sizes. The user can specifies how large a simulation of GRUs should be from the command line. This actor can then break that into smaller jobs to save on RAM.

## Job Actor

## File Access Actor

### Loading in Forcing Data
Loads Forcing Files in one at a time. The loading of files is initiated by the HRU_Actor by a message to request forcing data. 

## GRU Actor

## HRU Actor




# SUMMA-Distributed (Includes Additional Actors)


## Server Actor

## Client Actor