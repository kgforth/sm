sm
==

стековые манипуляторы. реализация на форте SPF4. частично совместима с https://github.com/chess2007/-chess


стековый манипулятор это строка символов, стартующая с заголовка
и ряда чередующихся блоков символов параметров и блоков символов команд

          | триггер-переключатель параметры-команды 

   5\xxxxx_xxxxx_xxxxx

   |  |     |     | блок параметров

   |  |     | блок команд

   |  | блок параметров

   | заголовок sm

Заголовки sm могут быть следующих видов:

( n1 n2 n3 -- ) 

3\  загрузить значение n1 со стека в ячейку 1, n2 в ячейку 2, n3 в ячейку 3
первый символ (3) число снимаемых со стека параметров может быть 0..9
\ - признак загрузки со снятием значения со стека в авто-назначенные ячейки 1..9

( n1 n2 n3 -- n1 n2 n3 ) 

3|  копировать значение n1 со стека в ячейку 1, n2 в ячейку 2, n3 в ячейку 3
первый символ (3) число копируемых со стека параметров может быть 0..9
| - признак копирования со стека в авто-назначенные ячейки 1..9

( n1 n2 n3 -- ) 

3:abc  загрузить значение n1 со стека в ячейку a, n2 в ячейку b, n3 в ячейку c
первый символ (3) число снимаемых со стека параметров может быть 0..9
: - признак загрузки со снятием значения со стека в буквенные ячейки 

( n1 n2 n3 -- n1 n2 n3 ) 

3%abc копировать значение n1 со стека в ячейку a, n2 в ячейку b, n3 в ячейку c
первый символ (3) число копируемых со стека параметров может быть 0..9
% - признак копирования со стека в буквенные ячейки 

сразу же за заголовком всегда идет блок символов параметров до суффикса-триггера ( _ )
символы 0-9 a-z A-Z в данном блоке означают положить на стек значение соответствующей
ячейки. символы команд ! # $ - .. } имеют свой обычный смысл. А вот чтобы использовать
односимвольные команды и константы в этом блоке нужно использовать префикс альтернативы ` 

за символом триггером идет блок символов команд до следующего триггера.
В блоке команд символы 0-9 означает константы 0..9, a-z A-Z соответствующие команды.
для вызова значений из ячеек надо использовать префикс альтернативы ` 

Назначение префиксов. 

^ - префикс адреса. положить на стек адрес ячейки соответсвующей следующему символу

: - префикс записи. значение со стека записать в ячейку соответствующую следующему символу

' - префикс дополнительных односимвольных команд. совместно со следующим одним символом  формирует дополнительный набор команд 

" - префикс двухсимвольных команд. В первую очередь проверяются два следующих символа
    на цифры - если да то это константа 00-99, далее на определенные двухсимвольные команды
    если нет, то попытка найти в общем словаре форт-системы.

~ - префикс трехсимвольных команд. В первую очередь проверяются три следующих символа
    на цифры - если да то это константа 000-999, далее на определенные трехсимвольные команды
    если нет, то попытка найти в общем словаре форт-системы.

` - префикс альтернативы. доступ к параметрам и командам альтернативных блоков.
    для символов ! # $ - .. } компиляция вызова и исполнения соответствующего вектора.

Назначение суффиксов

_ - триггер блоков символов в стековом манипуляторе, или терминатор макро-блока символов

, - склейка частей стекового манипулятора 

Для манипуляторов использовать будем младшую половину ASCII таблицы с кодами 0..127,
всего 128 символов, исключаем младшие 32 символа 128-32=96,
код пробела исключаем и код символа 127 т.к. не всеми редакторами отображается.
Получается мы можем использовать 96-2=94 символа.
эти символы можно разбить на группы

0-9 a-z A-Z  10+26+26=62  для имен параметров и/или команд

! # $ - .. }  24 команды                        

` ' " ~ ^ :   6 префиксов

, _           2 суффикса         

Итого 62+24+6+2=94 символа                     

\ ===========================================================================

рассмотрим возможные способы записи и варианты на примерах:

пусть на стеке лежат координаты точки и приращения, 
необходимо получить новые значения:

: t1 ( x y z dx dy dz - x+dx y+dy z+dz ) 6:XYZxyz, Xx+Yy+Zz+ ;

первое слово загружает ячейки, второе производит вычисления,
можно было бы записать одним словом, но так нагляднее.

Еще одно следствие использования именнованных ячеек - простота расширения
лексикона. Пусть нам требуется пересчитать координаты X Y Z а на 
стеке только приращения:

: t2 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) 3:xyz, Xx+:X, Yy+:Y, Zz+:Z ;

можно записать чуть короче, учитывая что приращения можно не 
сохранять в ячейках, правда надо соблюсти правильный порядок расчета

: t2 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) 0\Z+:Z, Y+:Y, X+:X ;

суффикс запятая ( , ) подсказывает что следующее слово является частью
стекового манипулятора и склеивается в одну общую строку в буфере до начала
компиляции. так как суммы по координатам независимы можно записать еще чуть короче

: t2 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) Z+:Z_ Y+:Y_ X+:X_ ;

суффикс нижняя черта ( _ ) указывает, что можно попробовать скомпилировать слово 
как манипулятор-блок, можно записать и слитно

: t2 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) Z+:ZY+:YX+:X_ ;

Можно и не извлекать значение ячеек на стек, а взяв адрес ячейки префиксом ^ 
применить форт операцию +! предварив ее префиксом "

: t2 ( dx dy dz - x+dx->x y+dy->y z+dz->y ) 0\^Z"+!, ^Y"+!, ^X"+! ;

или записав как манипулятор-блоки

: t2 ( dx dy dz - x+dx->x y+dy->y z+dz->y ) ^Z"+!_ ^Y"+!_ ^X"+!_ ;

продолжим наш коне-сферический пример, приращения не могут быть бесконечны,
нужны пределы, используем для X ячейки a и b, для Y ячейки c и d, для Z ячейки e и f

: Xlim! ( Xmin Xmax -- ) 2:ab ;

: Ylim! ( Ymin Ymax -- ) 2:cd ;

: Zlim! ( Zmin Zmax -- ) 2:ef ;

тогда приращения координат с учетом пределов можно записать так:

: t3 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) 

   3:xyz, Xx+a~MAXb~MIN:X, Yy+c~MAXd~MIN:Y, Zz+e~MAXf~MIN:Z ;

но у нас MIN и MAX входят в базовый набор командного блока, используем это

: t3 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) 

   3:xyz, Xx+a`Mb`m:X, Yy+c`Md`m:Y, Zz+e`Mf`m:Z ;

весь манипулятор у нас из символов блока данных, а MIN и MAX мы вызываем как 
односимвольные команды из альтернативного блока (блока команд)
перегруппируем данные и явно переключим блоки данных/команд

: t3 ( dx dy dz - x+dx->x y+dy->y z+dz->z ) 

   3:xyz, baXxdcYyfeZz_+Mm:Z+Mm:Y+Mm:X ;

допустим у нас по каждой координате есть особое значение, при достижении
которого надо выдать некоторое сообщение.

: MsgX ( -- ) ." Opps X detected !!!" CR ;

: MsgY ( -- ) ." Opps Y detected !!!" CR ;

: MsgZ ( -- ) ." Opps Z detected !!!" CR ;

Обратите внимание как "длинные" слова можно встраивать в манипуляторы,
а также на переключение контекста, ну и как использовать IF .. THEN 

: t4 ( Xdet Ydet Zdet -- ) 0|Z=`i[MsgZ]`t, Y=`i[MsgY]`t, X=`i[MsgX]`t ;

: t4 ( Xdet Ydet Zdet -- ) Z=_i[MsgZ]t_Y=_i[MsgY]t_X=_i[MsgX]t_ ;

стековый манипулятор состоит из 3 разных частей:
заголовка, блока параметров и блока команд. Наверное по правилу Миллера 
каждая часть может содержать оптимально  5 +-2 символа. 

: t4x ( Xdet -- ) ['] MsgX 1:.X=_i`.t ;

: t4y ( Ydet -- ) ['] MsgY 1:.Y=_i`.t ;

: t4z ( Zdet -- ) ['] MsgZ 1:.Z=_i`.t ;

: t4 ( Xdet Ydet Zdet - ) t4z t4y t4x ;

В примерах выше обратите внимание что заголовок записывает xt слова Msg*
в ячейку  с именем . а префикс альтернативы ` компилирует вызов и исполнение
этого xt. Это пример загрузки и использования векторов. А вектора у нас
все символы, которые не цифры и не буквы, всего 32 вектора:

! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ `  { | } ~

а вообще любой параметр может быть вектором, достаточно сохранить xt 
а затем вызвать EXECUTE, например в блоке парметров будет выглядеть
так: a`X а в блоке команд так: `aX

Пусть у нас есть точка с координатами XYZ необходимо просканировать
пространство, и если точка обнаружена выдать сообщение. Используем
тройной цикл DO ... LOOP

: Msg ( -- ) ." Opps XYZ detected !!! " X.Y.Z._ CR ;

: tst ( -- )  Xx=Yy=Zz=&&_i~Msgt_ ;

: t5 ( -- ) 50 50 50 3:XYZ 100 0 2:ab_`a`bDI:x`a`bDI:y`a`bDI:z~tstLLL ;

или обернув циклы в отдельные слова

: zLP ( -- ) ab_DI:z~tstL_ ;

: yLP ( -- ) ab_DI:y~zLPL_ ;

: xLP ( -- ) ab_DI:x~yLPL_ ;

: t5  ( -- ) 50 50 50 3:XYZ 100 0 2:ab xLP ;

Пример конструкции выбор по целому ( CASE )

: <<  CR ." Налево пойдешь - себя потеряешь, коня спасешь ..." ;

: >>  CR ." Направо пойдешь - коня потеряешь, себя спасешь ..." ;

: ||  CR ." Прямо пойдешь - и себя и коня потеряешь..." ;

: Вещий_камень ( n -- ) _C1("<<)2("||)3(">>)E_ ;



стековый манипулятор мощный инструмент, но даже короткие фрагменты
(микро и нано) можно удачно использовать, всего надо 1-2 символа чтобы
форт понял, что имеет дело со стековым манипулятором:

6\             снять 6 параметров со стека в ячейки 1..6 

4|12           аналог 2OVER

3:ijk          загрузить ячейки i j k

2%au           скопировать значения в ячейки a u 

 12+._         сложить и отпечатать значения ячеек 1 и 2 

_12+._         сложить константы 1 и 2 затем результат отпечатать 

1\111      *

1|11       *   утроить вершину стека

1:aaaa     *   ( n -- n n n )

1%aaa      *

X_X_       *    

X`X_       *   выполнить слово, xt которого хранится в ячейке X

_`XX_      *

P~255&:P_  *    взять значение из ячеки P (Port), выделить только младший байт и записать в P

0xFF P&:P_ *



