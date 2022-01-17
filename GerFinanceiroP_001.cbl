      ******************************************************************
      * Author: NATHAN DE FARIA
      * Date: 05/01/2022
      * Purpose: GERENCIADOR FINANCEIRO PESSOAL
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. GerFinanceiroP_001.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARQ-CONTAS ASSIGN TO
       'C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin\Pub\Contas.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FSTATUS.

           SELECT ARQ-RECEITAS ASSIGN TO
       'C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin\Pub\Receit.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FSTATUS.

           SELECT ARQ-DESPESAS ASSIGN TO
       'C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin\Pub\Despes.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FSTATUS.

           SELECT ARQ-BANCOS ASSIGN TO
       'C:\Program Files (x86)\OpenCobolIDE\GnuCOBOL\bin\Pub\Banc.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS MODE IS SEQUENTIAL
           FILE STATUS IS WS-FSTATUS.




       DATA DIVISION.
       FILE SECTION.
           FD ARQ-CONTAS.
               01 CONT-SLV.
                   03 CONT-SALD    PIC 999.999,99.
                   03 FILLER       PIC X VALUE ';'.
                   03 CONT-TIPO    PIC A(08) VALUE SPACES.
                   03 FILLER       PIC X VALUE ';'.
                   03 BANCO-CONT.
                       05 CONT-ABC-ID  PIC 9(03)   VALUE ZEROS.
                       05 FILLER       PIC X       VALUE ';'.
                       05 CONTA-NMBANC PIC A(25)   VALUE SPACES.
                       05 FILLER       PIC X       VALUE ';'.

           FD ARQ-RECEITAS.
               01 ARQ-REC.
                   03 ARQR-ID          PIC 9(04) VALUE ZEROS.
                   03 FILLER           PIC X(01) VALUE ';'.
                   03 ARQR-VAL         PIC 9(06).
                   03 FILLER           PIC X(01) VALUE ';'.
                   03 ARQR-DAT-RECB.
                       07 ARQR-DIA-RECB    PIC 9(02).
                       07 ARQR-MES-RECB    PIC 9(02).
                       07 ARQR-ANO-RECB    PIC 9(04).
                   03 ARQR-DAT-ESP.
                       07 ARQR-DIA-ESP     PIC 9(02).
                       07 ARQR-MES-ESP     PIC 9(02).
                       07 ARQR-ANO-ESP     PIC 9(04).
                   03 FILLER           PIC X(01) VALUE ';'.
                   03 ARQR-DESC        PIC A(15).
                   03 FILLER           PIC X(01) VALUE ';'.
                   03 ARQR-CONTA       PIC A(15).
                   03 FILLER           PIC X(01) VALUE ';'.
                   03 ARQR-TIPO        PIC 9(02).

      *     FD ARQ-BANCOS.
      *         01 ARQ-REC-BANCO.
      *             03 ARQCOD-BANCO  PIC 9(03).
      *             03 FILLER        PIC X VALUE ';'.
      *             03 ARQNME-BANCO  PIC A(30).


       WORKING-STORAGE SECTION.



      *----------------DECLARAÇÃO DE VARIAVEIS E TABLES----------------*
       01 WS-CONTADORES.
           03 WS-CONT-01   PIC 9(02) VALUE 1.
           03 WS-CONT-02   PIC 9(02) VALUE 0.
           03 WS-CONT-03   PIC 9(02) VALUE 0.
           03 WS-CONT-04   PIC 9(02) VALUE 0.
       01 WSR-BANCO.
           03 WSR-BANCO-TAB OCCURS 13 TIMES.
               05 WSRCOD-BANCO  PIC 9(03).
               05 FILLER        PIC X VALUE ';'.
               05 WSRNME-BANCO  PIC A(30).

       01 WSR-CONTA.
           03 WSR-CONTASALDO   PIC 999.999,99.
           03 FILLER           PIC X       VALUE ';'.
           03 WSR-CONTATIPO    PIC A(08)   VALUE SPACES.
           03 FILLER           PIC X       VALUE ';'.
           03 WSR-BANCODACONTA.
               05 WSR-IDCONTABANCO     PIC 9(03)   VALUE ZEROS.
               05 FILLER               PIC X       VALUE ';'.
               05 WSR-NMECONTABANCO    PIC A(25)   VALUE SPACES.
               05 FILLER               PIC X       VALUE ';'.


       01 WS-TESTESDATA.
           03 WS-TESTE-ANO.
               05 WS-ANO-X         PIC 9(02) VALUE 0.
               05 WS-ANO-100       PIC 9(02) VALUE 0.
               05 WS-ANO-004       PIC 9(02) VALUE 99.


       01 WS-DATA-OK       PIC A(02) VALUE 'NO'.
       01 WS-DIA-OK        PIC A(02) VALUE 'NO'.
       01 WS-MES-OK        PIC A(02) VALUE 'NO'.
       01 WS-ANO-OK        PIC A(02) VALUE 'NO'.
       01 WS-DIA-QTD       PIC 9(02) VALUE ZERO.
       01 WS-VAL-OK        PIC A(02) VALUE 'NO'.
       01 WS-SALVA         PIC A(02) VALUE 'NO'.
       01 WS-CONT-OK       PIC A(02) VALUE 'NO'.
       01 WS-PAG           PIC A(20) VALUE SPACES.
       01 WS-MREGI         PIC A(02) VALUE 'NO'.
       01 WS-ANOBISS       PIC A(02) VALUE 'NO'.
       01 WS-ABC-OK        PIC A(02) VALUE 'NO'.
       01 WS-CAD-CONT      PIC A(02) VALUE 'NO'.
       01 WS-BANCO-OK      PIC A(02) VALUE 'NO'.
       01 WS-CONTA-TIPO    PIC 9(02) VALUE ZERO.
       01 WS-LIMPA-TELA    PIC A(02) VALUE 'NO'.
       01 WS-CONTATIPO-OK  PIC A(02) VALUE 'NO'.

       01 WS-CONTADOR          PIC 9(02) VALUE ZERO.
       01 WS-FSTATUS           PIC 9(02) VALUE ZEROS.
       01 WS-STATUSOP          PIC X(35) VALUE SPACE.
       01 WS-EOF               PIC 9     VALUE ZERO.
       01 WS-ERROR             PIC X(25) VALUE SPACES.

       01 TAB-BANCO.
           03 TAB-BANCOS OCCURS 13 TIMES.
               05 WSR-ID-BANCO     PIC 9(03).
               05 FILLER           PIC X VALUE ';'.
               05 WSR-NME-BANCO    PIC A(30).

       01 WS-OPCAO     PIC 9(02) VALUE ZERO.
       01 WS-TESTE     PIC X(01) VALUE SPACE.
       01 WS-FIM-LOOP  PIC X(02) VALUE 'NO'.

       01 TAB-RECEITAS.
           03 TB-REC-ID            PIC X(04) VALUE ZEROS.
           03 FILLER               PIC X(01) VALUE ';'.
           03 TB-REC-VALOR         PIC 9(06).
           03 FILLER               PIC X(01) VALUE ';'.
           03 TB-DATA-RECEB.
               07 TB-DIA-RECEB     PIC 9(02).
               07 TB-MES-RECEB     PIC 9(02).
               07 TB-ANO-RECEB     PIC 9(04).
           03 TB-DATA-RECEB-ESP.
               07 TB-DIA-RECEB-ESP PIC 9(02).
               07 TB-MES-RECEB-ESP PIC 9(02).
               07 TB-ANO-RECEB-ESP PIC 9(04).
           03 FILLER       PIC X(01) VALUE ';'.
           03 TB-REC-DESC  PIC X(15).
           03 FILLER       PIC X(01) VALUE ';'.
           03 TB-REC-CONTA PIC X(15).
           03 FILLER       PIC X(01) VALUE ';'.
           03 TB-REC-TIPO  PIC 9(02).
       LINKAGE SECTION.
       SCREEN SECTION.

      *------------------LABELS DE DISPLAYS DOS MENUS------------------*
      *----LABEL CABECALHO PRINCIPAL-----------------------------------*
       01 LB-CABECALHO.
           03 BLANK SCREEN.

           03  LINE 01 COL 01 VALUE '                         '
               BACKGROUND-COLOR IS 03.
           03  LINE 01 COL 26 VALUE 'GERENCIADOR DE FINANCAS PESSOAL'
               BACKGROUND-COLOR IS 03 FOREGROUND-COLOR IS 00 .
           03  LINE 01 COL 56 VALUE '                         '
               BACKGROUND-COLOR IS 03.
           03  LINE 02 COL 01 VALUE
       '                                                              '-
       '                  '
               BACKGROUND-COLOR IS 01.
           03  LINE 02 COL 01 USING WS-PAG
               BACKGROUND-COLOR IS 01 FOREGROUND-COLOR IS 07.

       01 LB-OPERACAO.
           03  LINE 24 COL 01 VALUE
       '                                                              '-
       '                  '
               BACKGROUND-COLOR IS 03.
           03  LINE 24 COL 01 USING WS-STATUSOP FOREGROUND-COLOR IS 00
               BACKGROUND-COLOR IS 03.

       01 LB-FILE-ST.
           03 LINE 24 COL 59 VALUE 'FILE STATUS: '
           FOREGROUND-COLOR IS 04 BACKGROUND-COLOR IS 03.
           03 LINE 24 COL 74 USING
           WS-FSTATUS FOREGROUND-COLOR IS 04 BACKGROUND-COLOR IS 03.


      *----LABEL DO MENU PRINCIPAL-------------------------------------*
       01 LB-MENU.
           03  LINE 06 COL 03 VALUE '01-RECEITAS         '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 07 COL 03 VALUE '02-DESPESAS         '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 08 COL 03 VALUE '03-CONTAS           '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 09 COL 03 VALUE '99-ENCERRAR PROGRAMA'
           FOREGROUND-COLOR IS 04.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 10 COL 03 VALUE '**COD.: '
           FOREGROUND-COLOR IS 03.
           03 LB-OPCAO LINE 10 COL 11 USING WS-OPCAO
           FOREGROUND-COLOR IS 03.
           03 FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03 LINE 11 COL 03 VALUE 'DIGITE UMA DAS OPCOES NUMERICAS!'
           FOREGROUND-COLOR IS 03.

      *----LABELS DA PAGINA DE RECEITAS--------------------------------*
       01 LB-RECEITAS.
           03  LINE 06 COL 03 VALUE '01-CADASTRAR RECEITAS          '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 07 COL 03 VALUE '02-EDITAR RECEITAS             '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 08 COL 03 VALUE '03-REMOVER RECEITAS            '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 09 COL 03 VALUE '04-LISTAR RECEITAS(POR PERIODO)'.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 10 COL 03 VALUE '05-LISTAR RECEITAS(POR TIPO)   '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 11 COL 03 VALUE '06-LISTAR TODAS AS RECEITAS    '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 12 COL 03 VALUE '99-VOLTAR AO MENU PRINCIPAL    '
           FOREGROUND-COLOR IS 04.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 13 COL 03 VALUE '**COD.: '
           FOREGROUND-COLOR IS 03.
           03  LB-OPCAO-RECEITA LINE 13 COL 11 USING WS-OPCAO
           FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 14 COL 03 VALUE 'DIGITE UMA DAS OPCOES NUMERICAS!'
           FOREGROUND-COLOR IS 03.

      *----LABEL DE CADASTRO DE RECEITAS-------------------------------*
       01 LB-CADASTRO-RECEITAS.
           03  LINE 06 COL 03     VALUE 'VALOR DA RECEITA: R$ '.
           03  LB-REC-VALOR       LINE 06 COL 25
           USING TB-REC-VALOR     FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|'   FOREGROUND-COLOR IS 02.
           03  LINE 07 COL 03    VALUE 'DATA DE RECEBIMENTO:'.
           03  LB-DIA-RECEB    LINE 07 COL 24
           USING TB-DIA-RECEB    FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '/'  FOREGROUND-COLOR IS 02.
           03  LB-MES-RECEB    LINE 07 COL 28
           USING TB-MES-RECEB    FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '/'  FOREGROUND-COLOR IS 02.
           03  LB-ANO-RECEB    LINE 07 COL 32
           USING TB-ANO-RECEB    FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|'   FOREGROUND-COLOR IS 02.

           03  LINE 08 COL 03     VALUE 'DATA DE RECEBIMENTO ESPERADO:'.
           03  LB-DIA-RECEB-ESP   LINE 08 COL 35
           USING TB-DIA-RECEB-ESP FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '/'   FOREGROUND-COLOR IS 02.
           03  LB-MES-RECEB-ESP   LINE 08 COL 39
           USING TB-MES-RECEB-ESP FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '/'   FOREGROUND-COLOR IS 02.
           03  LB-ANO-RECEB-ESP LINE 08 COL 43
           USING TB-ANO-RECEB-ESP FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|'   FOREGROUND-COLOR IS 02.

           03  LINE 09 COL 03     VALUE 'DESCRICAO RECEITA:'.
           03  LB-REC-DESC        LINE 09 COL 22 USING TB-REC-DESC
           FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|'   FOREGROUND-COLOR IS 02.
           03  LINE 10 COL 03     VALUE 'CONTA:'.
           03  LB-REC-CONTA       LINE 10 COL 11
           USING TB-REC-CONTA     FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|'   FOREGROUND-COLOR IS 02.
           03  LINE 11 COL 03     VALUE 'TIPO DA RECEITA:'.
           03  LB-REC-TIPO        LINE 11 COL 21
           USING TB-REC-TIPO      FOREGROUND-COLOR IS 03.
           03  LINE 12 COL 03     VALUE '[01]Salario |[02]Presente |'-
           '[03]Premio |[04]Outros' FOREGROUND-COLOR IS 03.

      *----LABEL MENU CONTAS-------------------------------------------*
       01  LB-CONTAS.
           03  LINE 06 COL 03 VALUE '01-CADASTRAR CONTA               '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 07 COL 03 VALUE '02-EDITAR CONTA                  '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 08 COL 03 VALUE '03-REMOVER CONTA                 '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 09 COL 03 VALUE '04-LISTAR CONTA                  '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 10 COL 03 VALUE '05-TRANSFERIR SALDO ENTRE CONTAS '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 11 COL 03 VALUE '06-LISTAR SALDO TOTAL            '.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 12 COL 03 VALUE '99-ENCERRAR PROGRAMA             '
           FOREGROUND-COLOR IS 04.
           03  FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 13 COL 03 VALUE '**COD.: '
           FOREGROUND-COLOR IS 03.
           03  LB-OPCAO-CONTA LINE 13 COL 11 USING WS-OPCAO
           FOREGROUND-COLOR IS 03.
           03 FILLER VALUE '|' FOREGROUND-COLOR IS 02.
           03  LINE 14 COL 03 VALUE 'DIGITE UMA DAS OPCOES NUMERICAS!'
           FOREGROUND-COLOR IS 03.

       01 LB-CADASTRO-CONTAS.
           03  LINE 06 COL 03          VALUE 'INSTITUICAO FINANCEIRA: '.
           03  LB-CONTA-ABC            LINE 06 COL 28
           USING WSR-IDCONTABANCO      FOREGROUND-COLOR IS 03.
           03  FILLER VALUE '|'        FOREGROUND-COLOR IS 02.
           03  LINE 07 COL 03          VALUE 'TIPO DA CONTA: '.
           03  FILLER VALUE  '|'       FOREGROUND-COLOR IS 02.
           03  LINE 08 COL 03          VALUE '[01] CARTEIRA'.
           03  FILLER VALUE  '|'       FOREGROUND-COLOR IS 02.
           03  LINE 09 COL 03          VALUE '[02] CONTA CORRENTE'.
           03  FILLER VALUE  '|'       FOREGROUND-COLOR IS 02.
           03  LINE 10 COL 03          VALUE '[03] POUPANCA'.
           03  FILLER VALUE  '|'       FOREGROUND-COLOR IS 02.
           03  LB-CONTA-TIPO           LINE 07 COL 28
           USING WS-CONTA-TIPO         FOREGROUND-COLOR IS 03.
           03 LINE 11  COL 01 VALUE '                                 '.
           03 LINE 12  COL 03 VALUE 'SALDO DA CONTA:'.
           03 LB-SALDO-CONTA            LINE 12 COL 19
           USING WSR-CONTASALDO FOREGROUND-COLOR IS 03.
           03 LINE 13  COL 01 VALUE '                                 '.
           03 LINE 14  COL 01 VALUE '                                 '.


       01 LB-LIMPA-TELA.
           03 BLANK SCREEN.

       PROCEDURE DIVISION.
       MAIN-PROCIDURE.
      *-----------------------ABERTURA DE ARQUIVOS---------------------*
           OPEN EXTEND ARQ-RECEITAS

           IF WS-FSTATUS = 35
               OPEN OUTPUT ARQ-RECEITAS

           IF WS-FSTATUS EQUAL 00
               MOVE 'ARQ ABERTO COM SUCESSO!' TO WS-STATUSOP
           ELSE
               MOVE 'ARQ ERRO NAO IDENTIFICADO' TO WS-STATUSOP.


      *---------------------INICIALIZACAO DO PROGRAMA------------------*
       0000-INICIALIZACAO.

           PERFORM 1000-ZERA-VARIAVEL
           PERFORM 0000-ZERA-FLAGS
           PERFORM 0000-MENU.

      *-------------ZERAR VALORES DE VARIAVEIS, TABLES E LABELS--------*

       0000-ZERA-FLAGS.
           MOVE ZEROS  TO WS-ANO-X
           MOVE ZERO   TO WS-EOF
           MOVE 'NO'   TO WS-MES-OK
           MOVE 'NO'   TO WS-DIA-OK
           MOVE 'NO'   TO WS-ANO-OK
           MOVE SPACES TO WS-SALVA
           MOVE 'NO'   TO WS-FIM-LOOP
           MOVE ZEROS  TO WS-ANO-100
           MOVE 'NO'   TO WS-DATA-OK
           MOVE 'NO'   TO WS-CONT-OK
           MOVE 'NO'   TO WS-ANOBISS
           MOVE 'NO'   TO WS-CAD-CONT
           MOVE 'NO'   TO WS-ABC-OK
           MOVE 'NO'   TO WS-CONTATIPO-OK
           MOVE SPACES TO LB-REC-DESC
           MOVE SPACES TO LB-REC-TIPO
           MOVE SPACES TO LB-REC-VALOR
           MOVE SPACES TO LB-ANO-RECEB
           MOVE SPACES TO LB-MES-RECEB
           MOVE SPACES TO LB-DIA-RECEB
           MOVE SPACES TO LB-REC-CONTA
           MOVE SPACES TO LB-ANO-RECEB-ESP
           MOVE SPACES TO LB-MES-RECEB-ESP
           MOVE SPACES TO LB-DIA-RECEB-ESP.


       1000-ZERA-VARIAVEL.
           INITIALIZE WS-CONTADORES
           MOVE 99       TO WS-ANO-004
           MOVE 01       TO WS-CONT-01
           MOVE ZERO     TO WS-OPCAO
           MOVE 06       TO WS-CONT-02
           MOVE 1        TO WS-CONT-03
           MOVE ZEROS    TO WS-CONT-04
      *     MOVE SPACES  TO ARQ-REC-BANCO
           MOVE SPACE    TO WS-TESTE
           MOVE ZEROS    TO WS-DIA-QTD
           MOVE SPACES   TO WS-VAL-OK
           MOVE SPACES   TO WS-MREGI
           MOVE ZEROS    TO ARQR-VAL
           MOVE ZEROS    TO WS-CONTA-TIPO
           MOVE ZEROS    TO ARQR-ANO-RECB
           MOVE ZEROS    TO ARQR-MES-RECB
           MOVE ZEROS    TO ARQR-DIA-RECB
           MOVE ZEROS    TO ARQR-ANO-ESP
           MOVE ZEROS    TO ARQR-MES-ESP
           MOVE ZEROS    TO ARQR-DIA-ESP
           MOVE SPACES   TO ARQR-DESC
           MOVE SPACES   TO ARQR-CONTA
           MOVE ZEROS    TO ARQR-TIPO
           MOVE ZEROS    TO TB-REC-TIPO
           MOVE ZEROS    TO TB-DIA-RECEB
           MOVE ZEROS    TO TB-MES-RECEB
           MOVE ZEROS    TO TB-ANO-RECEB
           MOVE ZEROS    TO TB-REC-VALOR
           MOVE SPACES   TO TB-REC-DESC
           MOVE SPACES   TO TB-REC-CONTA
           MOVE ZEROS    TO TB-DIA-RECEB-ESP
           MOVE ZEROS    TO TB-MES-RECEB-ESP
           MOVE ZEROS    TO TB-ANO-RECEB-ESP.



      *--------------------------MENU PRINCIPAL------------------------*
       0000-MENU.

           MOVE SPACES TO WS-STATUSOP
           MOVE SPACES TO WS-PAG
           MOVE 'MENU PRINCIPAL' TO WS-PAG
           DISPLAY LB-CABECALHO
           DISPLAY LB-MENU
           DISPLAY LB-FILE-ST
           DISPLAY LB-OPERACAO
           PERFORM UNTIL WS-OPCAO = 99
              MOVE ZERO TO WS-OPCAO
              ACCEPT LB-OPCAO
              EVALUATE WS-OPCAO
              WHEN 01
                  MOVE SPACES TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
                  PERFORM 1000-MENU-RECEITAS
              WHEN 02
                  MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
              WHEN 03
                  MOVE SPACES TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
                  PERFORM 3001-MENU-CONTAS
              WHEN 99
                  MOVE SPACES TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
                  PERFORM 9999-ENCERRAPROGRAM
              WHEN OTHER
                  MOVE 'OPCAO INVALIDA!' TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
           END-PERFORM.

      *------------------- AREA VOLTADA PARA RECEITAS------------------*
       1000-MENU-RECEITAS.
           MOVE SPACES TO WS-STATUSOP
           MOVE SPACES TO WS-PAG
           MOVE 'PAGINA DE RECEITAS' TO WS-PAG
           PERFORM 1000-ZERA-VARIAVEL
           MOVE ZERO TO WS-OPCAO
           DISPLAY LB-CABECALHO
           DISPLAY LB-RECEITAS
           MOVE ZEROS TO LB-OPCAO-RECEITA
           PERFORM UNTIL WS-OPCAO = 99
              MOVE ZEROS TO WS-OPCAO
              ACCEPT LB-OPCAO-RECEITA
              EVALUATE WS-OPCAO
              WHEN 01
                  PERFORM 1001-CADASTRO-RECEITAS
              WHEN 02
                  MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
              WHEN 03
                  MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
              WHEN 04
                  MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
              WHEN 05
                  MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
              WHEN 06
                  MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
              WHEN 99
                  PERFORM 0000-INICIALIZACAO
              WHEN OTHER
                  MOVE 'OPCAO INVALIDA!' 
                  TO WS-STATUSOP
                  DISPLAY LB-OPERACAO
           END-PERFORM.

      *----CADASTRAMENTO DE RECEITAS-----------------------------------*
       1001-CADASTRO-RECEITAS.
           MOVE SPACES TO WS-STATUSOP
           IF WS-FSTATUS EQUAL ZEROS AND WS-FIM-LOOP = 'NO'
               MOVE SPACES TO WS-PAG
               MOVE 'CADASTRAR RECEITAS' TO WS-PAG

               MOVE ZEROS TO WS-OPCAO
               PERFORM UNTIL WS-FIM-LOOP = 'OK'
               PERFORM 0000-ZERA-FLAGS
               PERFORM 1000-ZERA-VARIAVEL
               MOVE WS-CONT-01 TO ARQR-ID
               DISPLAY LB-CABECALHO
               DISPLAY LB-CADASTRO-RECEITAS
               DISPLAY LB-OPERACAO
               PERFORM UNTIL WS-VAL-OK = 'OK'
                   ACCEPT LB-REC-VALOR
                   EVALUATE TRUE
                   WHEN TB-REC-VALOR > 0
                       MOVE SPACES TO WS-STATUSOP
                       DISPLAY LB-OPERACAO
                       MOVE 'OK' TO WS-VAL-OK
                   WHEN OTHER
                       MOVE 'VALOR DEVE SER MAIOR QUE "0"'
                       TO WS-STATUSOP
                       MOVE 'NO' TO WS-VAL-OK
                       DISPLAY LB-OPERACAO
               END-PERFORM

      *----ENTRADA DATA RECEITA----------------------------------------*
               PERFORM UNTIL WS-DATA-OK = 'OK'
                   PERFORM UNTIL WS-ANO-OK = 'OK'
                       ACCEPT LB-ANO-RECEB
                       MOVE 00 TO WS-ANO-100
                       MOVE 99 TO WS-ANO-004
                       MOVE 00 TO WS-ANO-X

                       DIVIDE 100 INTO TB-ANO-RECEB
                       GIVING WS-ANO-X
                       REMAINDER WS-ANO-100
                       DIVIDE 004 INTO TB-ANO-RECEB
                       GIVING WS-ANO-X
                       REMAINDER WS-ANO-004

                       EVALUATE TRUE
                       WHEN WS-ANO-100 NOT EQUAL 0
                       AND WS-ANO-004 = 0
                       AND TB-ANO-RECEB >= 2000
                           MOVE SPACES TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'OK' TO WS-ANOBISS
                           MOVE 'OK' TO WS-ANO-OK
                       WHEN TB-ANO-RECEB < 2000
                           MOVE 'ANO DEVE SER SEPERIOR A 2000'
                           TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'NO' TO WS-ANO-OK
                       WHEN OTHER
                           MOVE SPACES TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'OK' TO WS-ANO-OK
                           MOVE 'NO' TO WS-ANOBISS
                       END-EVALUATE
                   END-PERFORM
                   PERFORM UNTIL WS-MES-OK = 'OK'
                       MOVE ZERO TO TB-MES-RECEB
                       ACCEPT LB-MES-RECEB
                       EVALUATE TRUE
                           WHEN TB-MES-RECEB >=01
                           AND TB-MES-RECEB <=12
                               MOVE 'OK' TO WS-MES-OK
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                           WHEN OTHER
                               MOVE 'MES INVALIDO!' TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-MES-OK
                       END-EVALUATE
                   END-PERFORM
                   PERFORM UNTIL WS-DIA-OK = 'OK'
                       MOVE ZERO TO WS-DIA-QTD
                       MOVE ZERO TO TB-DIA-RECEB
                       ACCEPT LB-DIA-RECEB
                       EVALUATE TRUE
                       WHEN  TB-MES-RECEB = 01
                       OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
                           MOVE 31 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB >= 01
                           AND TB-DIA-RECEB <= WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO!' TO WS-STATUSOP
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN TB-MES-RECEB = 04
                       OR 06 OR 09 OR 11
                           MOVE 30 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB >= 01
                           AND TB-DIA-RECEB <= WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO!' TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN TB-MES-RECEB = 02
                       AND  WS-ANOBISS = 'OK'
                           MOVE 29 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB >= 1
                           AND TB-DIA-RECEB <= WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO' TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN TB-MES-RECEB = 02
                       AND  WS-ANOBISS = 'NO'
                           MOVE 28 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB >= 1
                           AND TB-DIA-RECEB <= WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO' TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN OTHER
                           MOVE ZEROS TO WS-DIA-QTD
                           MOVE 'NO' TO WS-DIA-OK
                       END-EVALUATE
                   END-PERFORM
                   EVALUATE TRUE
                   WHEN WS-ANO-OK = 'OK' AND WS-MES-OK = 'OK'
                   AND WS-DIA-OK = 'OK'
                       MOVE 'OK' TO WS-DATA-OK
                   WHEN OTHER
                       MOVE 'NO' TO WS-DATA-OK
               END-PERFORM

      *----ENTRADA DATA RECEITA ESPERADA-------------------------------*
               PERFORM 0000-ZERA-FLAGS

                   PERFORM UNTIL WS-DATA-OK = 'OK'
                   PERFORM UNTIL WS-ANO-OK = 'OK'
                       MOVE ZEROS TO ARQR-ANO-ESP
                       ACCEPT LB-ANO-RECEB-ESP
                       MOVE 00 TO WS-ANO-100
                       MOVE 99 TO WS-ANO-004
                       MOVE 00 TO WS-ANO-X

                       DIVIDE 100 INTO TB-ANO-RECEB-ESP
                       GIVING WS-ANO-X
                       REMAINDER WS-ANO-100
                       DIVIDE 004 INTO TB-ANO-RECEB-ESP
                       GIVING WS-ANO-X
                       REMAINDER WS-ANO-004

                       EVALUATE TRUE
                       WHEN WS-ANO-100 NOT EQUAL 0
                       AND  WS-ANO-004 = 0
                       AND  TB-ANO-RECEB-ESP >= 2000
                           MOVE SPACES TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'OK' TO WS-ANOBISS
                           MOVE 'OK' TO WS-ANO-OK
                       WHEN TB-ANO-RECEB-ESP < 2000
                           MOVE 'ANO DEVE SER SEPERIOR A 2000'
                           TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'NO' TO WS-ANO-OK
                       WHEN OTHER
                           MOVE SPACES TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'OK' TO WS-ANO-OK
                           MOVE 'NO' TO WS-ANOBISS
                       END-EVALUATE
                   END-PERFORM
                   PERFORM UNTIL WS-MES-OK = 'OK'
                       MOVE ZEROS TO TB-MES-RECEB-ESP
                       ACCEPT LB-MES-RECEB-ESP
                       EVALUATE TRUE
                           WHEN TB-MES-RECEB-ESP >=01
                           AND  TB-MES-RECEB-ESP <=12
                               MOVE 'OK' TO WS-MES-OK
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                           WHEN OTHER
                               MOVE 'MES INVALIDO'
                               TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-MES-OK
                       END-EVALUATE
                   END-PERFORM
                   PERFORM UNTIL WS-DIA-OK = 'OK'
                       MOVE ZERO TO WS-DIA-QTD
                       MOVE ZERO TO TB-DIA-RECEB-ESP
                       ACCEPT LB-DIA-RECEB-ESP
                       EVALUATE TRUE
                       WHEN  TB-MES-RECEB-ESP = 01
                       OR 03 OR 05 OR 07 OR 08 OR 10 OR 12
                           MOVE 31 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB-ESP >= 01
                           AND TB-DIA-RECEB-ESP <=
                               WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO!'
                               TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN TB-MES-RECEB-ESP = 04
                       OR 06 OR 09 OR 11
                           MOVE 30 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB-ESP >= 01
                           AND TB-DIA-RECEB-ESP <=
                           WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO!'
                               TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN TB-MES-RECEB-ESP = 02
                       AND WS-ANOBISS = 'OK'
                           MOVE 29 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB-ESP >= 1
                           AND TB-DIA-RECEB-ESP <=
                               WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO!'
                               TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN TB-MES-RECEB-ESP = 02
                       AND WS-ANOBISS = 'NO'
                           MOVE 28 TO WS-DIA-QTD
                           IF  TB-DIA-RECEB-ESP >= 1
                           AND TB-DIA-RECEB-ESP <=
                               WS-DIA-QTD
                               MOVE SPACES TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'OK' TO WS-DIA-OK
                           ELSE
                               MOVE 'DIA INVALIDO!'
                               TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               MOVE 'NO' TO WS-DIA-OK
                       WHEN OTHER
                           MOVE ZEROS TO WS-DIA-QTD
                           MOVE 'NO' TO WS-DIA-OK
                       END-EVALUATE
                   END-PERFORM
                   EVALUATE TRUE
                   WHEN WS-ANO-OK = 'OK' AND WS-MES-OK = 'OK'
                   AND WS-DIA-OK = 'OK'
                       MOVE 'OK' TO WS-DATA-OK
                   WHEN OTHER
                       MOVE 'NO' TO WS-DATA-OK
                   END-PERFORM

                   ACCEPT LB-REC-DESC
                   ACCEPT LB-REC-CONTA
                   PERFORM UNTIL WS-CONT-OK = 'OK'
                       ACCEPT LB-REC-TIPO
                       EVALUATE TRUE
                       WHEN TB-REC-TIPO = 01 OR 02 OR 03 OR 04
                           MOVE 'OK' TO WS-CONT-OK
                           MOVE SPACES TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                       WHEN OTHER
                           MOVE 'OPCAO INVALIDA!' TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                           MOVE 'NO' TO WS-CONT-OK
                   END-PERFORM



                   PERFORM UNTIL WS-SALVA = 'OK' OR 'NO'
                       MOVE 0 TO WS-OPCAO
                       DISPLAY 'DIGITE UMA DAS OPCOES NUMERICAS!'
                       LINE 13 COL 03 FOREGROUND-COLOR IS 03
                       DISPLAY '**SALVAR RECEITA? ' LINE 14 COL 03
                       FOREGROUND-COLOR IS 05
                       DISPLAY '[01]SIM' LINE 15 COL 21
                       FOREGROUND-COLOR IS 08
                       DISPLAY '[02]NAO' LINE 16 COL 21
                       FOREGROUND-COLOR IS 08
                       DISPLAY 'OPCAO: ' LINE 17 COL 21
                       FOREGROUND-COLOR IS 04
                       ACCEPT  WS-OPCAO  LINE 17 COL 31
                       FOREGROUND-COLOR IS 04
                       EVALUATE TRUE
                       WHEN WS-OPCAO = 01 AND WS-FSTATUS = ZEROS
                           MOVE TAB-RECEITAS TO ARQ-REC
                           WRITE ARQ-REC
                           IF WS-FSTATUS NOT EQUAL ZEROS
                               MOVE 'NAO FOI POSSIVEL GRAVAR O'-
                               ' REGISTRO!' TO WS-STATUSOP
                               DISPLAY LB-OPERACAO
                               DISPLAY LB-FILE-ST

                           ELSE
                              MOVE 'REGISTRO GRAVADO COM SUCESSO!'
                              TO WS-STATUSOP
                              DISPLAY LB-OPERACAO
                           END-IF
                           MOVE 'OK' TO WS-SALVA

                       WHEN WS-OPCAO = 02
                           MOVE 'NO' TO WS-SALVA
                           MOVE SPACES
                           TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                       WHEN OTHER
                           MOVE 'ER' TO WS-SALVA
                           MOVE 'OPCAO INVALIDA!' TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                   END-PERFORM
                   DISPLAY 'DESEJA INSERIR MAIS UMA RECEITA?'
                   LINE 18 COL 03 FOREGROUND-COLOR IS 05

                   PERFORM UNTIL WS-MREGI = 'OK' OR 'NO'
                       DISPLAY '[01]SIM' LINE 19 COL 37
                       FOREGROUND-COLOR IS 08
                       DISPLAY '[02]NAO' LINE 20 COL 37
                       FOREGROUND-COLOR IS 08
                       DISPLAY 'OPCAO: ' LINE 21 COL 37
                       FOREGROUND-COLOR IS 04
                       ACCEPT WS-OPCAO LINE 21 COL 47
                       FOREGROUND-COLOR 04

                           EVALUATE TRUE
                       WHEN WS-OPCAO = 01
                           MOVE 1 TO WS-CONT-01
                           PERFORM 0000-ZERA-FLAGS
                           PERFORM 1000-ZERA-VARIAVEL
                           DISPLAY '               ' LINE 21 COL 51
                           FOREGROUND-COLOR IS 04
                           MOVE 'NO' TO WS-MREGI
                       WHEN WS-OPCAO = 02
                           MOVE 'OK' TO WS-FIM-LOOP
                           PERFORM 0000-ZERA-FLAGS
                           PERFORM 1000-ZERA-VARIAVEL
                           PERFORM 1000-MENU-RECEITAS
                       WHEN OTHER
                           MOVE 'ER' TO WS-MREGI
                           MOVE 'OPCAO INVALIDA!' TO WS-STATUSOP
                           DISPLAY LB-OPERACAO
                   END-PERFORM


               END-PERFORM
           ELSE
               MOVE 'ERRO AO CRIAR O ARQUIVO!' TO WS-STATUSOP
               DISPLAY LB-OPERACAO
               DISPLAY LB-FILE-ST
           END-IF.


      *---------------------AREA RESERVADA PARA CONTAS-----------------*
       3001-MENU-CONTAS.
      *     MOVE SPACES TO WS-STATUSOP
      *     MOVE SPACES TO WS-PAG
      *     MOVE 'PAGINA DE CONTAS' TO WS-PAG
      *     PERFORM 0000-ZERA-FLAGS
      *     PERFORM 1000-ZERA-VARIAVEL
           DISPLAY LB-CABECALHO
           DISPLAY LB-CONTAS
           PERFORM UNTIL WS-OPCAO = 99
               MOVE ZEROS TO WS-OPCAO
               ACCEPT LB-OPCAO-CONTA
               EVALUATE WS-OPCAO
               WHEN 01
                   MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!'
                   TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
               WHEN 02
                   MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!'
                   TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
               WHEN 03
                   MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!'
                   TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
               WHEN 04
                   MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!'
                   TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
               WHEN 05
                   MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!'
                   TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
               WHEN 06
                   MOVE 'OPCAO DO SISTEMA NAO FINALIZADA!'
                   TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
               WHEN 99
                   PERFORM 0000-INICIALIZACAO
               WHEN OTHER
                   DISPLAY '                                 '
                   LINE 13 COL 14
                   MOVE 'OPCAO INVALIDA!' TO WS-STATUSOP
                   DISPLAY LB-OPERACAO
           END-PERFORM.

      *FINALIZACAO DO PROGRAMA.
       9999-ENCERRAPROGRAM.
      *----------------------FECHAMENTO DE ARQUIVOS--------------------*

            CLOSE ARQ-CONTAS ARQ-RECEITAS ARQ-DESPESAS
      *      ARQ-BANCOS.
            GOBACK.
