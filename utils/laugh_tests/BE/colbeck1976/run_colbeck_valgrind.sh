#! /bin/bash
valgrind --leak-check=full --show-leak-kinds=all --track-origins=yes --verbose --log-file=summa_out.txt /SUMMA/bin/summa_sundials.exe -g 1 1 -m /Summa-Actors/utils/laugh_tests/colbeck1976/settings/summa_fileManager_verify_colbeck1976-exp3.txt