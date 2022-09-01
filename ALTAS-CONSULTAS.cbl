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
           SELECT SOCIOS ASSIGN TO "..\arch.dat"
           ORGANIZATION RELATIVE
           ACCESS MODE is DYNAMIC
           RELATIVE key is rel-nro.
       DATA DIVISION.
       FILE SECTION.
       COPY "CopiFd.cpy".

       WORKING-STORAGE SECTION.
       77  sen pic 9.
           88 fin-de-archivo value 1.
       01  rel-nro pic 999.
           88 no-quiere-mas value 0.
       77  w-llave-menu pic 9.
           88 salir-menu VALUE 3.
       01  w-soc-ant pic 9(4).
       01  w-resul pic 9(3).
       01  w-resto pic 9(3).
       77  w-oficina pic 9(3) value 98.
       01  w-posicion-nula pic 9(3) VALUE ZERO.

      ******************************************************************
      ******************************************************************
       PROCEDURE DIVISION.

       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-MENU.
           PERFORM UNTIL salir-menu
                PERFORM 300-PROCESO
               PERFORM 200-MENU
           END-PERFORM.
           PERFORM 800-FIN.

            STOP RUN.
      ******************************************************************
      ******************************************************************
       100-INICIO.
           OPEN I-O SOCIOS.
           move w-oficina to w-posicion-nula.

       110-PRIMER-POSICION-VACIA.
           MOVE w-oficina TO rel-nro
           START SOCIOS KEY IS = rel-nro
            INVALID KEY
            DISPLAY "NO HAY LUGAR PARA SINONIMOS"
            NOT INVALID KEY
               READ SOCIOS
               MOVE soc-puntero TO w-posicion-nula.

       200-MENU.

           DISPLAY ".................................... ".
           DISPLAY "  INGRESE UNA OPCION: "
           DISPLAY "  1- ALTA"
           DISPLAY "  2- CONSULTA"
           DISPLAY "  3- FIN"
           DISPLAY "..................................... ".
           ACCEPT w-llave-menu.
           PERFORM 210-VALIDAR-OPCION.

       210-VALIDAR-OPCION.
           PERFORM UNTIL w-llave-menu < 4 AND w-llave-menu >0
               DISPLAY "Opcion incorrecta"
               PERFORM 200-MENU
           END-PERFORM.

       300-PROCESO.
           IF w-llave-menu is EQUAL 1
               PERFORM 400-ALTA
           ELSE
               PERFORM 600-CONSULTA
           END-IF.

       400-ALTA.
           PERFORM 405-PIDO-SOCIO.
           PERFORM 410-INVOCAR-FUNCION-HASHING.
           PERFORM 420-BUSCAR-UBICACION.

       405-PIDO-SOCIO.
           DISPLAY "Ingrese codigo de socio".
           ACCEPT w-soc-ant.

       410-INVOCAR-FUNCION-HASHING.
           DIVIDE 97 INTO w-soc-ant GIVING w-resul REMAINDER rel-nro.
           add 1 to rel-nro.
           MOVE rel-nro to w-resto.

       420-BUSCAR-UBICACION.
           PERFORM 430-LEER-SOCIO
           IF soc-codigo=0
               PERFORM 440-PRIMER-INGRESO
           ELSE
               IF  w-soc-ant = soc-codigo
                   PERFORM 460-INGRESO-EXISTENTE

               ELSE
                   PERFORM 480-UBICAR-SINONIMO
               END-IF
           END-IF.

       430-LEER-SOCIO.
           READ Socios.

       440-PRIMER-INGRESO.
            MOVE w-soc-ant to soc-codigo
               PERFORM 450-PIDO-NOMBRE
               PERFORM 470-ACTUALIZAR-SOCIO.

       460-INGRESO-EXISTENTE.
           DISPLAY "El socio ya se encuentra registrado"
           PERFORM 620-MOSTRAR-SOCIO.

       470-ACTUALIZAR-SOCIO.
           REWRITE soc-reg.

       450-PIDO-NOMBRE.
           DISPLAY "Ingrese el nombre de socio".
           ACCEPT soc-nombre.

       455-MOVER-VARIABLES.
           MOVE soc-puntero to rel-nro.
           MOVE w-soc-ant TO soc-codigo.
           MOVE ZERO to soc-puntero.
           PERFORM 450-PIDO-NOMBRE.
           PERFORM 470-ACTUALIZAR-SOCIO.

       480-UBICAR-SINONIMO.
            PERFORM UNTIL soc-puntero is =0
            or w-soc-ant is =soc-codigo
             MOVE  soc-puntero to rel-nro
             PERFORM 430-LEER-SOCIO
            END-PERFORM.
             IF  w-soc-ant = soc-codigo
                PERFORM 460-INGRESO-EXISTENTE
             ELSE
                PERFORM 500-BUSCO-LUGAR
            END-IF.

       500-BUSCO-LUGAR.
           add 1 to  w-oficina.
           MOVE w-oficina to soc-puntero.
           PERFORM 470-ACTUALIZAR-SOCIO.
           PERFORM 455-MOVER-VARIABLES.
           PERFORM 550-REINICIO-OFICINA.

       550-REINICIO-OFICINA.
           ADD 1 TO rel-nro.
           MOVE rel-nro to soc-puntero.
           MOVE w-posicion-nula TO rel-nro.
           MOVE zero to soc-codigo.
           move " " to soc-nombre.
           PERFORM 470-ACTUALIZAR-SOCIO.

      ******************************************************************
      ******************************************************************

       600-CONSULTA.
            PERFORM 405-PIDO-SOCIO.
            PERFORM 410-INVOCAR-FUNCION-HASHING.
            PERFORM 430-LEER-SOCIO.
             IF  w-soc-ant = soc-codigo
                 PERFORM 620-MOSTRAR-SOCIO
             ELSE
                PERFORM 630-BUSCAR-SOCIO-SINONIMO
            END-IF.

       620-MOSTRAR-SOCIO.
            display "CODIGO: ",soc-codigo," Nombre: ", soc-nombre.


       630-BUSCAR-SOCIO-SINONIMO.

           PERFORM UNTIL soc-puntero is =0 or w-soc-ant is =soc-codigo
               MOVE  soc-puntero to rel-nro
               PERFORM 430-LEER-SOCIO
           END-PERFORM
           IF  w-soc-ant = soc-codigo
            PERFORM 620-MOSTRAR-SOCIO
           ELSE
               DISPLAY "El codigo ingresado no se encuentra"
           END-IF.

       800-FIN.
           CLOSE Socios.

       END PROGRAM YOUR-PROGRAM-NAME.
