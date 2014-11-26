DECIMAL 
VARIABLE   CSP    \ Указатель стека контроля
6 CONSTANT L-CAS# \ Допустимый уровень вложенности
CREATE     S-CSP   L-CAS# CELLS ALLOT S-CSP CSP !  \ Стек контроля
: +CSP ( -> P)    CSP @ DUP CELL+ CSP ! ;   \ Добавить уровень
: -CSP ( -> )     CSP @ 1 CELLS - CSP ! ;   \ Убрать уровень
: !CSP ( -> )      SP@ +CSP ! ;             \ Инициализировать уровень
: CSP@ ( -> A)     CSP @ 1 CELLS - @   ;
: ?CSP ( -> ) SP@ CSP@ <> 37 ?ERROR ( ABORT" Сбой стека по CSP !") -CSP ; \ Проверить выдержанность стека
: CASE ( -> )    !CSP ; IMMEDIATE
: OF   POSTPONE OVER POSTPONE =  [COMPILE] IF POSTPONE DROP ; IMMEDIATE
: ENDOF  [COMPILE] ELSE ; IMMEDIATE
: ENDCASE POSTPONE DROP BEGIN SP@ CSP@ = 0= WHILE [COMPILE] THEN REPEAT -CSP ; IMMEDIATE

USER-CREATE /d0/ 10 256 * CELLS USER-ALLOT USER /d/ /d0/ /d/ !

USER /org USER /beg USER /cur USER /lim USER /a/ USER /u/    VECT sm,

: SPDROP ( n -- )   [ BASE @ HEX C1 C, E0 C, 02 C, 03 C, E8 C, 8B C, 45 C, 00 C, 8D C, 6D C, 04 C, C3 C, BASE ! ] ;
: SPMOVE ( a n -- ) [ BASE @ HEX 8D C, 5D C, 04 C, 8B C, 55 C, 00 C, 8B C, 0B C, 89 C, 0A C, 8D C, 52 C, 04 C,
                           8D C, 5B C, 04 C, 48 C, 75 C, F3 C, 8B C, 45 C, 04 C, 8D C, 6D C, 08 C, C3 C, BASE ! ] ;

\ Слово __  надо использовать для реентерабельных целей просто переустанавливает
\ указатель /d/ и готовит возврат к предыдущему буферу(кадру) при выходе из слова 
\ с манипулятором (такой простейший стек).

: __RET -256 CELLS /d/ +! ;      : __   256 CELLS /d/ +! R> ['] __RET >R >R ;

: sm+  256 CELLS /d/ +! ;   IMMEDIATE
   
: sm- -256 CELLS /d/ +! ;   IMMEDIATE    

\ непосредственно установка рабочего буфера(кадра) ( 0 d!, 1 d! ...9 d! ):
: d! ( n -- ) 256 * CELLS /d0/ + /d/ ! ;  

\ вспомагательные слова
: dg? ( a -- f ) C@ [CHAR] 0 [CHAR] 9 1+ WITHIN  ;
: az? ( a -- f ) C@ [CHAR] a [CHAR] z 1+ WITHIN  ;
: AZ? ( a -- f ) C@ [CHAR] A [CHAR] Z 1+ WITHIN  ;

: vec? ( a -- f ) DUP dg? OVER az? OR SWAP AZ? OR 0= ;
: num? ( a u -- f ) OVER DUP dg? SWAP C@ [CHAR] - = OR -ROT OVER SWAP 1 D+ ?DO I dg? AND LOOP ;

: [org] ( char -- org ) 255 SWAP - CELLS ;
: [c,] ( C -- ) [org] LIT, ;
: [d,]  ['] /d/ COMPILE, ['] @ COMPILE, ['] + COMPILE, ; 
: [p,]  2- LIT, ['] PICK COMPILE, ; 
: [@,]  /cur @ C@ [org] LIT, [d,] ['] @ COMPILE, ;

\ Lambda. код внутри конструкции LAMBDA{  }LAMBDA не выполняется, возвращается xt на этот код.   

 : LAMBDA{  ( -- res xt ) 0 BRANCH, >MARK HERE ; IMMEDIATE
 : }LAMBDA  ( res xt -- ) RET, >R >RESOLVE1 R> LIT, ; IMMEDIATE

: S' [CHAR] ' PARSE [COMPILE] SLITERAL ; IMMEDIATE

: ??  ( a u --  f ) SWAP OVER /cur @ OVER COMPARE 0= IF 1- /cur +! 0 THEN ; 

( 0 - data; -1 - cmd )

USER (trigger)  (trigger) 0!   : _trigger ( -- )  (trigger) @ -1 XOR (trigger) ! ;  

: DAND ;
: DOR ;

\ ======
\ распознать что мы имеем на входе m-block ( манипулятор-блок или макро-блок ) и
\ скомпилировать предписанные его символам действия. m-block цепочка символов более
\ одного и заканчивающееся символом '_' 
\ пример  : t1 1 1\ 1._ ;  печать 1

: NOTFOUND  ( A U -- ) \ m-block   
  2DUP /u/ ! /a/ ! + 1- C@ [CHAR] _ = /u/ @ 1 > AND 0= IF /a/ @ /u/ @ NOTFOUND EXIT THEN
  (trigger) 0! /a/ @ DUP /cur ! /u/ @ + 2- /lim ! BEGIN sm, /cur @ /lim @ <> WHILE /cur 1+! REPEAT ;

: ABCparse    

0 CASE                                       

  S" A" ??  OF POSTPONE ACCEPT        ENDOF
  S" B" ??  OF POSTPONE BEGIN         ENDOF 
  S" C" ??  OF POSTPONE CASE          ENDOF 
  S" D" ??  OF POSTPONE ?DO           ENDOF 
  S" E" ??  OF POSTPONE ENDCASE       ENDOF
  S" F" ??  OF ['] FILL      COMPILE, ENDOF
  S" G" ??  OF ( BOUND ?DO )          ENDOF
  S" H" ??  OF ['] WITHIN    COMPILE, ENDOF
  S" I" ??  OF POSTPONE I             ENDOF
  S" J" ??  OF POSTPONE J             ENDOF
  S" K" ??  OF ( EKEY )               ENDOF
  S" L" ??  OF POSTPONE LOOP          ENDOF
  S" M" ??  OF ['] MAX       COMPILE, ENDOF
  S" N" ??  OF POSTPONE +LOOP         ENDOF
  S" O" ??  OF POSTPONE AGAIN         ENDOF
  S" P" ??  OF ['] DEPTH     COMPILE, ENDOF
  S" Q" ??  OF POSTPONE LEAVE         ENDOF
  S" R" ??  OF POSTPONE REPEAT        ENDOF
  S" S" ??  OF ['] SPACES    COMPILE, ENDOF
  S" T" ??  OF ['] TYPE      COMPILE, ENDOF
  S" U" ??  OF POSTPONE UNTIL         ENDOF
  S" V" ??  OF ['] MOVE      COMPILE, ENDOF
  S" W" ??  OF POSTPONE WHILE         ENDOF
  S" X" ??  OF ['] EXECUTE   COMPILE, ENDOF
  S" Y" ??  OF ['] TRUE      COMPILE, ENDOF 
  S" Z" ??  OF ['] 0=        COMPILE, ENDOF
  S" a" ??  OF ['] ABS       COMPILE, ENDOF
  S" b" ??  OF ['] C@        COMPILE, ENDOF
  S" c" ??  OF ['] EMIT      COMPILE, ENDOF
  S" d" ??  OF ['] DUP       COMPILE, ENDOF 
  S" e" ??  OF POSTPONE ELSE          ENDOF
  S" f" ??  OF ( FREE THROW )         ENDOF
  S" g" ??  OF ['] CELL      COMPILE, ENDOF  
  S" h" ??  OF ( ALLOCATE THROW )     ENDOF
  S" i" ??  OF POSTPONE IF            ENDOF
  S" j" ??  OF ['] 1+!       COMPILE, ENDOF
  S" k" ??  OF ['] KEY       COMPILE, ENDOF
  S" l" ??  OF ['] LSHIFT    COMPILE, ENDOF
  S" m" ??  OF ['] MIN       COMPILE, ENDOF
  S" n" ??  OF ['] NEGATE    COMPILE, ENDOF
  S" o" ??  OF ( ------------------ ) ENDOF  
  S" p" ??  OF ( ------------------ ) ENDOF
  S" q" ??  OF ['] COMPARE   COMPILE, ENDOF
  S" r" ??  OF ['] RSHIFT    COMPILE, ENDOF
  S" s" ??  OF ['] SEARCH    COMPILE, ENDOF
  S" t" ??  OF POSTPONE THEN          ENDOF
  S" u" ??  OF ( ------------------ ) ENDOF
  S" v" ??  OF ( ------------------ ) ENDOF
  S" w" ??  OF ['] C!        COMPILE, ENDOF
  S" x" ??  OF ['] DROP      COMPILE, ENDOF
  S" y" ??  OF ['] FALSE     COMPILE, ENDOF  
  S" z" ??  OF ['] 0<>       COMPILE, ENDOF
  S"  " ??  OF NOOP                   ENDOF

 ENDCASE  
;

\ ^ перед всеми адрес ячейки

: ^parse /cur 1+!  /cur @ C@ [c,] [d,] ;

\ : записать со стека в ячейку 

: :parse /cur 1+!  /cur @ C@ [c,] [d,] ['] ! COMPILE, ;

\ ` перед 0...9 a..z A..Z значение из альтернативного блока 
\ ` перед остальными исполнить вектор

: `parse  /cur 1+!  

/cur @ vec? IF /cur @ C@ [c,] [d,] ['] @ COMPILE, ['] EXECUTE COMPILE, EXIT THEN 

/cur @ vec? 0= (trigger) @ AND IF [@,] EXIT THEN ( 0..9 or a..z or A..Z and cmd-block )

/cur @ dg? (trigger) @ 0= AND IF /cur @ C@ 48 - LIT, EXIT THEN   ( constant 0-9 )

/cur @ az? /cur @ AZ? OR (trigger) @ 0= AND 

IF ABCparse THEN

;

\ ' дополнительный набор односимвольных операторов

: 'parse   /cur 1+! 

0 CASE
  S" !" ??  OF ['] 2!          COMPILE, ENDOF
  S' "' ??  OF ( -------------------- ) ENDOF
  S" #" ??  OF ['] CELLS       COMPILE, ENDOF
  S" $" ??  OF ( -------------------- ) ENDOF
  S" %" ??  OF ( -------------------- ) ENDOF
  S" &" ??  OF ['] DAND        COMPILE, ENDOF
  S" '" ??  OF ( -------------------- ) ENDOF
  S" (" ??  OF ( -------------------- ) ENDOF
  S" )" ??  OF ( -------------------- ) ENDOF
  S" *" ??  OF ( -------------------- ) ENDOF
  S" +" ??  OF ['] D+          COMPILE, ENDOF
  S" ," ??  OF ( -------------------- ) ENDOF
  S" -" ??  OF ['] D-          COMPILE, ENDOF
  S" ." ??  OF ['] D.          COMPILE, ENDOF
  S" 0" ??  OF 1024 LIT,      ( 2^10  ) ENDOF
  S" 1" ??  OF    2 LIT,      ( 2^1   ) ENDOF
  S" 2" ??  OF    4 LIT,      ( 2^2   ) ENDOF
  S" 3" ??  OF    8 LIT,      ( 2^3   ) ENDOF
  S" 4" ??  OF   16 LIT,      ( 2^4   ) ENDOF
  S" 5" ??  OF   32 LIT,      ( 2^5   ) ENDOF
  S" 6" ??  OF   64 LIT,      ( 2^6   ) ENDOF
  S" 7" ??  OF  128 LIT,      ( 2^7   ) ENDOF
  S" 8" ??  OF  256 LIT,      ( 2^8   ) ENDOF
  S" 9" ??  OF  512 LIT,      ( 2^9   ) ENDOF
  S" :" ??  OF ( -------------------- ) ENDOF
  S" ;" ??  OF ( -------------------- ) ENDOF
  S" <" ??  OF ['] D<          COMPILE, ENDOF
  S" =" ??  OF ['] D=          COMPILE, ENDOF
  S" >" ??  OF ['] D>          COMPILE, ENDOF
  S" ?" ??  OF ( DXOR )                 ENDOF
  S" @" ??  OF ['] 2@          COMPILE, ENDOF
  S" A" ??  OF ( -------------------- ) ENDOF
  S" B" ??  OF ( -------------------- ) ENDOF 
  S" C" ??  OF ( -------------------- ) ENDOF 
  S" D" ??  OF ( -------------------- ) ENDOF 
  S" E" ??  OF ( -------------------- ) ENDOF
  S" F" ??  OF ( -------------------- ) ENDOF
  S" G" ??  OF ( -------------------- ) ENDOF
  S" H" ??  OF ( -------------------- ) ENDOF
  S" I" ??  OF ( -------------------- ) ENDOF
  S" J" ??  OF ( -------------------- ) ENDOF
  S" K" ??  OF ( -------------------- ) ENDOF
  S" L" ??  OF ( -------------------- ) ENDOF
  S" M" ??  OF ( -------------------- ) ENDOF
  S" N" ??  OF ( -------------------- ) ENDOF
  S" O" ??  OF ( -------------------- ) ENDOF
  S" P" ??  OF ( -------------------- ) ENDOF
  S" Q" ??  OF ( -------------------- ) ENDOF
  S" R" ??  OF ( -------------------- ) ENDOF
  S" S" ??  OF ( -------------------- ) ENDOF
  S" T" ??  OF ( -------------------- ) ENDOF
  S" U" ??  OF ( -------------------- ) ENDOF
  S" V" ??  OF ( -------------------- ) ENDOF
  S" W" ??  OF ( -------------------- ) ENDOF
  S" X" ??  OF ( -------------------- ) ENDOF
  S" Y" ??  OF ( -------------------- ) ENDOF 
  S" Z" ??  OF ( D0=                  ) ENDOF
  S" [" ??  OF ( -------------------- ) ENDOF
  S" \" ??  OF ( -------------------- ) ENDOF
  S" ]" ??  OF ( -------------------- ) ENDOF
  S" ^" ??  OF ( -------------------- ) ENDOF
  S" _" ??  OF ( -------------------- ) ENDOF
  S" `" ??  OF ( -------------------- ) ENDOF
  S" a" ??  OF ( -------------------- ) ENDOF
  S" b" ??  OF ( ['] W@      COMPILE, ) ENDOF
  S" c" ??  OF ( -------------------- ) ENDOF
  S" d" ??  OF ( -------------------- ) ENDOF 
  S" e" ??  OF ( -------------------- ) ENDOF
  S" f" ??  OF ( -------------------- ) ENDOF
  S" g" ??  OF ( -------------------- ) ENDOF  
  S" h" ??  OF ( -------------------- ) ENDOF
  S" i" ??  OF ( ['] 1-      COMPILE, ) ENDOF
  S" j" ??  OF ( -------------------- ) ENDOF
  S" k" ??  OF ( ['] 1-!     COMPILE, ) ENDOF
  S" l" ??  OF ( ['] DLSHIFT COMPILE, ) ENDOF
  S" m" ??  OF ( -------------------- ) ENDOF
  S" n" ??  OF ( ['] DNEGATE COMPILE, ) ENDOF
  S" o" ??  OF ( -------------------- ) ENDOF  
  S" p" ??  OF ( -------------------- ) ENDOF
  S" q" ??  OF ( -------------------- ) ENDOF
  S" r" ??  OF ( ['] DRSHIFT COMPILE, ) ENDOF
  S" s" ??  OF ( -------------------- ) ENDOF
  S" t" ??  OF ( -------------------- ) ENDOF
  S" u" ??  OF ( -------------------- ) ENDOF
  S" v" ??  OF ( -------------------- ) ENDOF
  S" w" ??  OF ( ['] W!      COMPILE, ) ENDOF
  S" x" ??  OF ( ['] 2DROP   COMPILE, ) ENDOF
  S" y" ??  OF ( -------------------- ) ENDOF  
  S" z" ??  OF ( D0= INVERT           ) ENDOF
  S" {" ??  OF ( -------------------- ) ENDOF
  S" |" ??  OF ( ['] DOR     COMPILE, ) ENDOF
  S" }" ??  OF ( -------------------- ) ENDOF
  S" ~" ??  OF ( ['] DINVERT COMPILE, ) ENDOF
  S"  " ??  OF NOOP                     ENDOF
 ENDCASE  
;

\ " признак двухсимвольных операторов

: "parse /cur 1+! 

/cur @ 2 num? IF /cur @ 2 ?SLITERAL1 /cur 1+! EXIT THEN ( "00-"99 )

0 CASE
       S" DS" ??  OF ['] D>S                 COMPILE, ENDOF
       S" SD" ??  OF ['] S>D                 COMPILE, ENDOF
       S" f;" ??  OF ['] CLOSE-FILE          COMPILE, ENDOF
       S" f:" ??  OF ['] CREATE-FILE         COMPILE, ENDOF
       S" F:" ??  OF ['] CREATE-FILE-SHARED  COMPILE, ENDOF
       S" Fo" ??  OF ['] OPEN-FILE-SHARED    COMPILE, ENDOF
       S" fx" ??  OF ['] DELETE-FILE         COMPILE, ENDOF
       S" fp" ??  OF ['] FILE-POSITION       COMPILE, ENDOF
       S" fs" ??  OF ['] FILE-SIZE           COMPILE, ENDOF
       S" fo" ??  OF ['] OPEN-FILE           COMPILE, ENDOF
       S" fr" ??  OF ['] READ-FILE           COMPILE, ENDOF
       S" fP" ??  OF ['] REPOSITION-FILE     COMPILE, ENDOF
       S" lr" ??  OF ['] READ-LINE           COMPILE, ENDOF
       S" fw" ??  OF ['] WRITE-FILE          COMPILE, ENDOF
       S" fS" ??  OF ['] RESIZE-FILE         COMPILE, ENDOF
       S" lw" ??  OF ['] WRITE-LINE          COMPILE, ENDOF
       S" ff" ??  OF ['] FLUSH-FILE          COMPILE, ENDOF
       S" fe" ??  OF ['] FILE-EXIST          COMPILE, ENDOF
       S" ts" ??  OF ['] START               COMPILE, ENDOF
       S" t|" ??  OF ['] SUSPEND             COMPILE, ENDOF
       S" tr" ??  OF ['] RESUME              COMPILE, ENDOF
       S" t;" ??  OF ['] STOP                COMPILE, ENDOF
       S" tp" ??  OF ['] PAUSE               COMPILE, ENDOF
       S" tx" ??  OF ['] TERMINATE           COMPILE, ENDOF
       S"   " ??  OF NOOP                             ENDOF
   /cur @ 2 SFIND 0= IF NOTFOUND ELSE COMPILE, /cur 1+! THEN 
 ENDCASE  
;

: BINARY 2 BASE ! ;

\ ~  признак трехсимвольных операторов

: ~parse   /cur 1+! 

/cur @ 3 num? IF /cur @ 3 ?SLITERAL1 2 /cur +! EXIT THEN  ( ~000-~999 )

0 CASE

      S" hex" ??  OF ['] HEX              COMPILE, ENDOF
      S" dec" ??  OF ['] DECIMAL          COMPILE, ENDOF
      S" bin" ??  OF ['] BINARY           COMPILE, ENDOF
      S"    " ??  OF NOOP                          ENDOF

   /cur @ 3 SFIND 0= IF NOTFOUND ELSE COMPILE, 2 /cur +! THEN  

 ENDCASE  

;

: \.tst? ( -- f )  /a/ @ 1+ C@ [CHAR] \ = /u/ @ 1 > AND ;
: |.tst? ( -- f )  /a/ @ 1+ C@ [CHAR] | = /u/ @ 1 > AND ;
: /.tst? ( -- f )  /a/ @ 1+ C@ [CHAR] / = /u/ @ 1 > AND ;
: :.tst? ( -- f )  /a/ @ 1+ C@ [CHAR] : = /u/ @ /a/ @ C@ 48 - 1+ > AND ;
: %.tst? ( -- f )  /a/ @ 1+ C@ [CHAR] % = /u/ @ /a/ @ C@ 48 - 1+ > AND ;

: sm.tst? ( a u -- f ) /u/ ! DUP /a/ ! dg? \.tst? |.tst? /.tst? :.tst? %.tst? OR OR OR OR AND ;

: sm.set ( org -- ) /a/ @ + 2+ /beg ! /a/ @ /u/ @ + /lim ! ;

: var.load ( -- ) /a/ @ 2+ DUP /org @ + 1- DO I C@ [c,] [d,] ['] ! COMPILE, -1 +LOOP ;                                   
: var.copy ( -- ) 2 /a/ @ 2+ DUP /org @ + 1- DO DUP [p,] I C@ [c,] [d,] ['] ! COMPILE, 1+ -1 +LOOP DROP ; 

: prm.copy /a/ @ C@ [c,] [d,] /org @ LIT, ['] SPMOVE COMPILE, ; 
: prm.load /a/ @ C@ [c,] [d,] /org @ LIT, ['] SPMOVE COMPILE, /org @ LIT, ['] SPDROP COMPILE, ; 

: sm.load  (trigger) 0!   /a/ @ C@ 48 - /org !

  /org @ 0= IF 0 sm.set EXIT THEN

  /a/ @ 1+ C@ 

 CASE

   [CHAR] : OF var.load /org @ sm.set ENDOF
   [CHAR] % OF var.copy /org @ sm.set ENDOF
   [CHAR] | OF prm.copy      0 sm.set ENDOF      
   [CHAR] \ OF prm.load      0 sm.set ENDOF 
   [CHAR] / OF /org @ LIT, ['] d! COMPILE, 0 sm.set ENDOF

 ENDCASE

;

: sm.comp /beg @ /cur ! BEGIN sm, /lim @ /cur @ > WHILE /cur 1+! REPEAT ;

: sm,,,]  /cur @ 1+ /beg ! BEGIN /cur @ 1+ C@ [CHAR] ] <> WHILE /cur 1+! REPEAT 
          /cur 1+! /beg @ /cur @ OVER - sm.tst? 
IF sm.load sm.comp ELSE /a/ @ /u/ @ SFIND 0= IF NOTFOUND ELSE COMPILE, THEN THEN ;


: op, ( -- )                                 

\  CR ." /cur= " /cur @ C@ DUP EMIT  ."  " . ."  trig=" (trigger) @ .

/cur @ vec? 0= (trigger) @ 0= AND IF [@,] EXIT THEN ( 0..9 or a..z or A..Z and data-block )

/cur @ dg? (trigger) @ AND IF /cur @ C@ 48 - LIT, EXIT THEN   ( constant 0-9 )

/cur @ vec? 0= (trigger) @ AND IF  ABCparse  EXIT THEN
                                           
0 CASE                                       

  S" !" ??  OF ['] !         COMPILE, ENDOF 
  S' "' ??  OF "parse                 ENDOF
  S" #" ??  OF ['] >NUMBER   COMPILE, ENDOF
  S" $" ??  OF ['] SFIND     COMPILE, ENDOF
  S" %" ??  OF ['] MOD       COMPILE, ENDOF
  S" &" ??  OF ['] AND       COMPILE, ENDOF
  S" '" ??  OF 'parse                 ENDOF
  S" (" ??  OF POSTPONE    OF         ENDOF
  S" )" ??  OF POSTPONE ENDOF         ENDOF
  S" *" ??  OF ['] *         COMPILE, ENDOF
  S" +" ??  OF ['] +         COMPILE, ENDOF 
  S" ," ??  OF NOOP                   ENDOF 
  S" -" ??  OF ['] -         COMPILE, ENDOF
  S" ." ??  OF ['] .         COMPILE, ENDOF 
  S" /" ??  OF ['] /         COMPILE, ENDOF
  S" :" ??  OF :parse                 ENDOF
  S" ;" ??  OF POSTPONE EXIT          ENDOF
  S" <" ??  OF ['] <         COMPILE, ENDOF
  S" =" ??  OF ['] =         COMPILE, ENDOF
  S" >" ??  OF ['] >         COMPILE, ENDOF
  S" ?" ??  OF ['] XOR       COMPILE, ENDOF  
  S" @" ??  OF ['] @         COMPILE, ENDOF
  S" [" ??  OF sm,,,]                 ENDOF    
  S" \" ??  OF ['] CR        COMPILE, ENDOF
  S" ]" ??  OF NOOP                   ENDOF 
  S" ^" ??  OF ^parse                 ENDOF
  S" _" ??  OF _trigger               ENDOF
  S" `" ??  OF `parse                 ENDOF
  S" {" ??  OF POSTPONE LAMBDA{       ENDOF
  S" |" ??  OF ['] OR  COMPILE,       ENDOF 
  S" }" ??  OF POSTPONE }LAMBDA       ENDOF
  S" ~" ??  OF ~parse                 ENDOF
  S"  " ??  OF NOOP                   ENDOF
         
 ENDCASE  ;

 ' op, TO sm,

: NOTFOUND ( A U -- ) sm.tst? IF sm.load sm.comp ELSE /a/ @ /u/ @ NOTFOUND THEN ;

\ разрыв манипуляторов с помощью суффикса ','

USER-CREATE buffstr 1024 USER-ALLOT
USER-VALUE dpstr 0 TO dpstr   
USER-VALUE flsym? 0 TO flsym?
USER-VALUE /a,/
USER-VALUE /u,/

: NOTFOUND ( a u ) 
  2DUP TO /u,/ TO  /a,/ + 1- C@ [CHAR] , = 
  IF /a,/ buffstr dpstr + /u,/ 1- MOVE dpstr /u,/ 1- + TO dpstr 1 TO flsym?
  ELSE flsym?  
       IF  0 TO flsym? /a,/ buffstr dpstr + /u,/ MOVE dpstr /u,/ + TO dpstr buffstr dpstr 1- NOTFOUND  0 TO dpstr EXIT 
       ELSE 0 TO flsym? /a,/ /u,/ NOTFOUND  0 TO dpstr EXIT THEN
  THEN ; 

\EOF
