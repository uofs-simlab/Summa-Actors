# config_file

The configuration file is used to adjust the settings for summa actors. This file is passed into SUMMA using the -c argument for running summa.

The settings are defined as follows:

## Distributed_Settings - For client and server actors
- distributed_mode
- hostname
- port
- total_hru_count
- num_hru_per_batch
- heartbeat_interval
- lost_node_threshold

## Summa_Actor
- max_gru_per_job

## File_Access_Actor
- num_vectors_in_output_manager

## Job_Actor
- file_manager_path
- output_structure_size
- output_csv
- csv_path

## HRU_Actor
- print_output
- output_frequency