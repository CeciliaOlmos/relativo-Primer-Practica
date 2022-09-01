      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           COPY "C-Arch.cpy".
      *
       DATA DIVISION.
       FILE SECTION.
           COPY "CopiFd.cpy".
      *
       WORKING-STORAGE SECTION.
       77  i pic 999.
           88 sala VALUE 98.
      *
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT SOCIOS.
           perform varying i from 1 by 1 until i > 117
               MOVE 0 TO soc-codigo
               move " " TO soc-nombre
               if sala
                   move 99 to soc-puntero
               else
                   move 0 to soc-puntero
               end-if
               write soc-reg INVALID KEY DISPLAY "no pude"
                             NOT INVALID KEY
                               DISPLAY "GRABACION EXITOSA"
           END-PERFORM.
           close SOCIOS.
           STOP RUN.
      *
       END PROGRAM YOUR-PROGRAM-NAME.
