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
       77  sen pic 9 value 0.
       77  i pic 999 value 0.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN input SOCIOS.
           perform leo.
           perform until sen = 1
               add 1 to i
               display "posicion: ", i
               display "codigo: ", soc-codigo
               display "nombre: ", soc-nombre
               display "puntero: ", soc-puntero
               perform leo
            END-PERFORM.
            close socios.
            STOP RUN.
       leo.
           read SOCIOS at end move 1 to sen.


       END PROGRAM YOUR-PROGRAM-NAME.
