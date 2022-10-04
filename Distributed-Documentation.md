# SUMMA-Distributed
SUMMA-Distributed is an actor program that can compute the solution to HRUs using multiple computers connected in a network.
To achieve this we had to introduce two new actors into SUMMA-Actors the client actor and the server actor. These actors in combintaiton can 
compute the solution to HRUs.

To use distributed mode. Set the "distributed-mode" setting to true in the Summa_Actors_Settings.json file.

## SUMMA-Server
Run the server with:
 - summaMain -s -c /path/to/config

## SUMMA-Client
Run the cleint with
