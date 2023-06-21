( constants)
30 CONSTANT SIZE                    ( the size of the galactic array)
SIZE 2 * CONSTANT NO-OF-STARS       ( no. of stars in the galaxy)
SIZE 3 * 2 / CONSTANT NO-OF-PLANETS ( planets in galaxy)
4 CONSTANT NO-OF-B-HOLES            ( no. of black holes)
200 CONSTANT W1                     ( weight value assigned to planets in score)
5 CONSTANT W2                       ( weight assigned to ship and troops)
10 CONSTANT W3                      ( weight assigned to computers troops)
20000 CONSTANT SPEED                ( how quickly computer moves)
250 constant delayms 		    ( milliseconds to wait for in DELAY) \ accelerate by 4

( VARIABLES)
0 VARIABLE BUY-V        ( count to stop player buying every move)
0 VARIABLE C-FLEETS     ( no. of computer fleets)
0 VARIABLE C-LEGIONS    ( no. of computer legions for attacks)
0 VARIABLE C-PLANETS    ( no. of computer planets)
0 VARIABLE CLASS-TOTALS ( computers planets classes totals)
0 VARIABLE COMP-START   ( how quickly computer plays)
0 VARIABLE COMPUTER     ( how quickly computer plays)
0 VARIABLE DIFF         ( difficulty of game 1-4)
0 VARIABLE FLEET-FLAG   ( no. of players current fleet)
0 VARIABLE LEG          ( the no. of legions available to buy)
0 VARIABLE LEN          ( no. of moves remaining in game)
0 VARIABLE NEW          ( how often new fleet created)
0 VARIABLE PLANETS      ( no. of players planets)
0 VARIABLE RAND1        ( first random number seed)
0 VARIABLE RAND2        ( second random number seed)
0 VARIABLE START        ( starting score in the game)
0 VARIABLE TEMP1        ( a temporary storage v ariable)
0 VARIABLE TROOPS       ( no. of computers troops in game)
0 VARIABLE VTAX         ( tax collected)
0 VARIABLE X            ( temporary storage for X position)
0 VARIABLE Y            ( temporary storage for Y position)
0 VARIABLE score
0 VARIABLE Tcount
250 VARIABLE CREDIT     ( players credit in taxes)

( TARGET SPECIFIC WORDS)
needs modernise.fs
require random.fs

( DEFINING WORDS)
: ARRAY ( 2D Array)
	CREATE DUP C, * ALLOT DOES>
	ROT 1 - OVER C@ * + + 
;

SIZE SIZE ARRAY GALAXY ( the galactic array array)
SIZE SIZE ARRAY INFO1 ( planetary array)
SIZE SIZE ARRAY INFO2 ( strength array)
11 11 ARRAY SCREEN ( the screen array)
2 6 ARRAY FLEETS ( player fleets info)

( general utility words)
: DELAY                      ( delay a fixed amount of time)
   delayms ms  ( wait a second.  Is this long enough?  Who knows, should probably be a CONSTANT)
;

: CLEAR-MSGE                 ( clear message area on text screen)
   18 10 DO
           I 0 vhtab 50 SPACES
         LOOP 
;

: XY@
   X @ Y @ 
;

: CLEAR-SCREEN ( clear hires screen 1)
   HCLR 
;

: CLEAR-DISP ( fill screen array with FF's)
   1 1 SCREEN 121 255 FILL 
;

: CLEAR-GALAXY ( fills galactic array with NULLS)
   1 1 GALAXY SIZE SIZE * 0 FILL 
;

: CLEAR-INFO ( fills info arrays with NULLs)
   1 1 INFO1 SIZE SIZE * 0 FILL
   1 1 INFO2 SIZE SIZE * 0 FILL 
;

: RANDOM1 ( --- ran) ( random number in range 1-SIZE)
   RAND1 @ 37 * 651 + DUP RAND1 ! ABS SIZE MOD 1+ 
;

: RANDOM2 ( --- ran ) ( random number in range 1-SIZE)
   RAND2 @ 53 * 773 + DUP RAND2 ! ABS SIZE MOD 1+ 
;

: EDGE-CHECK ( n --- ng ) ( calculates wrap around of galaxy)
   SIZE 1 - + SIZE MOD 1+ 
;

: INPUT ( --- n1 ) ( number input routine)
   0 BEGIN ( start with zero total)
       KEY DUP EMIT DUP 8 = ( is it backspace?)
       IF
         DROP 10 / 0 ( get rid of last digit)
       ELSE
         DUP 57 > ( check if char is digit)
         IF DROP 1
         ELSE DUP 48 <
           IF DROP 1
           ELSE 48 - SWAP 10 * + 0
           ENDIF
         ENDIF
       ENDIF
     UNTIL 
;

: F ( n1 --- add1 ) ( indexes current fleet array)
   FLEET-FLAG @ SWAP FLEETS 
;

: END-MSGE                  ( end of game message)
   12 0 vhtab ." END OF GAME COMMANDER " 
;

( graphics shapes and utilities)
0 VARIABLE SPACEFIG 80 ALLOT  ( shape tables)

: C$                        ( loads 8-bit value into table)
   OVER C! 1+ 
;

: $                         ( loads 16-bit value into table)
   OVER ! 2 + 
;
\ this is a static map
SPACEFIG                    ( load shape tables)  \ game map 
   12 $ 31 $ 41 $ 47 $ 63 $ 74 $ HEX
( space shape)
   24 C$ 3F C$ 37 C$ 36 C$ 2E C$ 24 C$ 2C C$ 36 C$ 2E C$ 2C C$
   2E C$ 25 C$ 24 C$ 3C C$ 37 C$ 2E C$ 34 C$ 36 C$ 00 C$
( colony shape)	
   12 C$ 2D C$ 24 C$ 24 C$ 3F C$ 3F C$ 36 C$ 36 C$ 2D C$ 00 C$
( planet shape )
   2C C$ 36 C$ 3F C$ 24 C$ 05 C$ 00 C$
( star shape)
   3C C$ 36 C$ 2D C$ 24 C$ 1C C$ 1F C$ 16 C$ 16 C$ 0D C$ 0D C$
   1C C$ 2C C$ 1C C$ 0C C$ 04 C$ 00 C$
( players fleet shape)
   3C C$ 1B C$ 36 C$ 26 C$ 08 C$ 2D C$ 2D C$ 25 C$ 36 C$ 36 C$
   00 C$
( computers fleet shape)
   36 C$ 07 C$ 20 C$ 29 C$ 32 C$ 00 C$
\ feel free to modify the map 
DECIMAL DROP
: SKETCH  ( n ---  )    ( sketch shape n at current position)
   2 * 0 SWAP SPACEFIG + w@ SPACEFIG + 2drop
;
\ now adds galaxy
: SET-UP-GALAXY
   NO-OF-STARS 0 DO 2 RANDOM1 RANDOM2 GALAXY C!
                 LOOP   ( set up stars in galaxy)
   NO-OF-PLANETS 0 DO RANDOM1 RANDOM2 2DUP 4 ROT ROT
                      GALAXY C!  ( set up planets)
                      RANDOM1 4 * 8 + ROT ROT INFO1 C!
                      ( set up class of planet)
                   LOOP
   NO-OF-B-HOLES 0 DO 8 RANDOM1 RANDOM2 GALAXY C!
                      ( set up black holes)
                   LOOP 
;

: INITIALISE         ( initialise all variables and arrays)
   CR CR
   BEGIN
      32 colorize
      ." WHAT LEVEL OF DIFFICULTY (1-4) " INPUT DUP
      5 < IF  ( correct response) 1
          ELSE ( incorrect response) DROP CR 0
          ENDIF
   UNTIL
   DIFF !      ( store difficulty)
   hclr ( screen is messy otherwise)
   HOME CR CR
   ." DO YOU WANT" CR ." 1. SHORT" CR ." 2. MEDIUM" CR
   ." 3. LONG" CR ." GAME"
   KEY 127 AND     ( pick up reply)
   CASE
      49 ( 1) OF 350 LEN ! ( 350 moves) ENDOF
      50 ( 2) OF 700 LEN ! ( 700 moves) ENDOF
                1500 LEN ! ( 1500 moves otherwise)
   ENDCASE
   HOME ." INITIALISING"
   CLEAR-GALAXY CLEAR-DISP CLEAR-INFO SET-UP-GALAXY
   1 FLEET-FLAG !          ( make -fleet 1 current fleet)
   250 CREDIT !            ( players credit)
   0 PLANETS !             ( no planets)
   0 C-PLANETS !           ( none for computer either)
   20 1 3 FLEETS w! 20 2 3 FLEETS w! ( fleets start with 20 ships)
   50 1 5 FLEETS w! 50 2 5 FLEETS w! ( fleets have 50 legions each)
   DIFF @ 4 * 0 DO ( position computers fleets)
                   RANDOM1 RANDOM2 2DUP 17 ROT ROT GALAXY C!
                   15 ROT ROT INFO2 C!
              LOOP
   16 22 18 GALAXY C! 16 18 22 GALAXY C! ( position fleets)
   22 1 1 FLEETS C! 18 1 2 FLEETS C!
   18 2 1 FLEETS C! 22 2 2 FLEETS C!
   29 3 DIFF @ * - NEW !  ( how often computer creates fleets)
   15 DIFF @ 4 * * TROOPS ! ( initial no. of computer troops)
   20 DIFF @ * C-LEGIONS ! ( no. of spare computer legions)
   DIFF @ 4 * C-FLEETS !   ( no. of computer fleets)
   SPEED DUP COMPUTER !
   COMP-START !   ( how often computer moves)
   1 BUY-V ! 
;

: DRAW-BORDERS   ( draw borders o-f display and headings)
   CLEAR-SCREEN
   HOME
   32 colorize
   ." PLAYER"
   2  0  vhtab ." PLANETS = "
   4  0  vhtab ." EMPIRE "
   6  0  vhtab ." FLEETS "
   6  21 vhtab ." PLANETS "
   20 0  vhtab ." X= "
   20 7  vhtab ." Y= "
   21 0  vhtab ." NO. OF SHIPS = "
   22 0  vhtab ." LEGIONS = "
   20 25 vhtab ." SCORE = "
   22 21 vhtab ." CREDITS " 
   33 colorize
;

: FIND-DIRECTION     (  --- X Y )
                     ( find out which square player means)
   32 colorize
   23 0 vhtab ." WHICH DIRECTION?" 
   33 colorize
   2 SPACES inkey 127 AND
   CASE
      73 ( up)    OF -1  0 ENDOF 
      75 ( down)  OF  1  0 ENDOF
      76 ( right) OF  0  1 ENDOF
      74 ( left)  OF  0 -1 ENDOF
                      0  0
   ENDCASE ( vim ijkl mode )
   23 0 vhtab 35 SPACES   ( clear message )
   2 F C@ + EDGE-CHECK SWAP
   1 F C@ + EDGE-CHECK SWAP 
;

: PRINT-IT  ( c  --- )
	( shape determined by c is printed on screen at)
	( position in X,Y)
	\ DUP X @ 1+ Y @ 1+ SCREEN C@ =
	\ IF  ( display is already showing this shape so don't bother)
	\   DROP
	\ ELSE
      dup \ for ANSI print
      DUP X @ 1+ Y @ 1+ SCREEN C! ( remember what screen has)
      X @ 20 * 27 + Y @ 1+ 11 * 2drop
      0 SKETCH                    ( blank out char. there)
      X @ 20 * 27 + Y @ 1+ 11 * 2drop
      CASE                        ( draw shape)
         2 ( a star)         OF 3 SKETCH ( draw star)     ENDOF
         4 ( empty planet)   OF 2 SKETCH ( a planet)      ENDOF
         5 ( enemy planet)   OF 2 SKETCH ( a planet)      ENDOF
       132 ( players planet) OF 1 SKETCH ( a colony)      ENDOF
        16 ( players fleet)  OF 4 SKETCH ( players fleet) ENDOF
        17 ( enemy fleet)    OF 5 SKETCH ( enemy fleet)   ENDOF
      ENDCASE
      \ and once again, in ANSI
      \ FIXME this needs moved out to a separate file
      Y @ 8 + X @ 2 * 12 + vhtab ."   "
      Y @ 8 + X @ 2 * 12 + vhtab
      CASE                        ( draw shape)
         2 ( a star)         OF 33 colorize ." *" 0 colorize ( draw star)     ENDOF
         4 ( empty planet)   OF 36 colorize ." O" 0 colorize ( a planet)      ENDOF
         5 ( enemy planet)   OF 36 colorize ." 0" 0 colorize ( a planet)      ENDOF
       132 ( players planet) OF 31 colorize ." @" 0 colorize ( a colony)      ENDOF
        16 ( players fleet)  OF 0 colorize ." P" 0 colorize ( players fleet)  ENDOF
        17 ( enemy fleet)    OF 31 colorize ." E" 0 colorize ( enemy fleet)   ENDOF
      ENDCASE
   \ ENDIF
;

: DRAW-SCAN                       ( draw the screen display)
   1 F C@ 5 - 2 F C@ 5 -
   11 0 DO
      11 0 DO
              OVER EDGE-CHECK OVER EDGE-CHECK
              J Y ! I X ! GALAXY C@
              PRINT-IT 1+
           LOOP
           11 - SWAP 1+ SWAP
        LOOP
   DROP DROP 
;

: DRAW-FIGURES    ( draw the totals in the disp1ay)
	2 10 vhtab PLANETS @ 5 .R
	20 33 vhtab score @ 
	1 3 FLEETS w@ 2 3 FLEETS w@ + W2 * +
	1 5 FLEETS w@ 2 5 FLEETS w@ + W2 * +
	TROOPS @ W3 * - 6 .R
	6 8   vhtab C-FLEETS @ 5 .R
	6 29  vhtab C-PLANETS @ 5 .R
	20 3  vhtab 2 F C@ 2 .R
	20 9  vhtab 1 F C@ 2 .R
	21 15 vhtab 3 F w@ 4 .R
	22 10 vhtab 5 F w@ 6 .R
	22 31 vhtab CREDIT @ 6 .R 
;

: DRAW-DISPLAY
   DRAW-SCAN DRAW-FIGURES 
;

: NEW-FLEET  ( fleet destroyed for some reason)
   \ 24 0 vhtab ." fleet destroyed"
   \ you can still move the fleet cursor around though
   \ and buy more ships and take on more legions
   0 1 F C@ 2 F C@ GALAXY C!   ( remove fleet symbol)
   0 3 F w!                     ( no ships left)
   0 5 F w!                     ( no legions left)
;

: MOVE-FLEET (  X Y ---  )
   2DUP
   0 1 F C@ 2 F C@ GALAXY C!   ( remove old symbol)
   16 ROT ROT GALAXY C!       ( position fleet)
   2 F C! 1 F C!               ( update fleet array)
;

: CHECK-POSITION  ( X Y --- )
	33 colorize
	( check if move to position X Y is possib
	( and take apropriate action)
   EDGE-CHECK SWAP EDGE-CHECK SWAP 2DUP GALAXY C@
   CASE
      0 ( space)      OF MOVE-FLEET ENDOF
      8 ( black hole) OF 23 0 vhtab ." FLEET IN BLACK HOLE"
                         MOVE-FLEET DELAY NEW-FLEET
                         23 0 vhtab 35 SPACES ENDOF
      DROP DROP
   ENDCASE
   DRAW-DISPLAY 
;

: OTHER-FLEET   ( make other fleet curent fleet)
   FLEET-FLAG @ 1 =
   IF  2 FLEET-FLAG !
   ELSE 1 FLEET-FLAG !
   ENDIF
   DRAW-DISPLAY 
;

: MOVE-LEFT
	1 tcount +!
   1 F C@ 2 F C@ 1 - CHECK-POSITION 
;

: MOVE-RIGHT
	1 tcount +!
   1 F C@ 2 F C@ 1+ CHECK-POSITION 
;

: MOVE-DOWN
	1 tcount +!
   1 F C@ 1+ 2 F C@ CHECK-POSITION 
;

: MOVE-UP
	1 tcount +!
   1 F C@ 1 - 2 F C@ CHECK-POSITION 
;

: ENLIST    ( enlisting 1egions on a planet)
	1 tcount +!
   33 colorize
   BUY-V @ 0=
   IF  ( it's ok to buy)
       5 BUY-V !  ( can't buy for 5 more moves)
       ( calculate cost of legions)
       RANDOM1 8 / XY@ INFO1 C@ 7 / + DUP TEMP1 !
       10 0 vhtab ." COST PER LEGION =" 3 .R
       ( calculate no. of legions available)
       XY@ INFO1 C@ 6 / DUP LEG !
       12 0 vhtab ." NO OF LEGIONS AVAILABLE = " 3 .R
       ( take the order)
       14 0 vhtab ." HOW MANY DO YOU REQUIRE? " INPUT
       LEG @ MIN DUP TEMP1 @ * CREDIT @ >
       IF  ( not enough money)
         16 0 vhtab ." NOT ENOUGH CREDIT"
       ELSE
         5 F w@ OVER + 5 F w!  ( update legions)
         TEMP1 @ * CREDIT @ SWAP - CREDIT ! ( update credit)
       ENDIF
   ELSE
      10 0 vhtab ." NO TROOPS AVAILABLE"
   ENDIF 
;

: BUY    ( purchasing of ships at planet)
	1 tcount +!
	33 colorize
   BUY-V @ 0=
   IF    ( it's ok to buy)
      5 BUY-V !               ( stop continous buying)
      RANDOM1 5 / XY@ INFO1 C@ 10 / + 1+ DUP TEMP1 !
      10 0 vhtab ." COST PER SHIP = " 2 .R
      12 0 vhtab ." HOW MANY DO YOU WANT? " INPUT
      CREDIT @ TEMP1 @ / MIN    ( no more than he can afford)
      DUP 3 F w@ + 3 F w!       ( update ships in fleet)
      TEMP1 @ * CREDIT @ SWAP - CREDIT !  ( update credit)
      16 1 F C@ 2 F C@ GALAXY C!  ( make sure fleet symbol there)
   ELSE
      10 0 vhtab ." NO SHIPS AVAILABLE"
   ENDIF 
;

: GATHER   ( pick up legions from garrison onto fleet)
	1 tcount +!
	33 colorize
   10 0 vhtab ." HOW MANY DO YOU WISH TO TAKE? " INPUT
   XY@ INFO2 C@ MIN TEMP1 !  ( no more than are there)
   5 F w@ TEMP1 @ + 5 F w!     ( update legions on fleet)
   XY@ INFO2 C@ TEMP1 @ - XY@ INFO2 C! ; ( update on planet)

\ Orignal name: "LEAVE"
: DEPLOY   ( leave legions from fleet on planet as garrison)
	1 tcount +!
	33 colorize
   10 0 vhtab ." HOW MANY DO YOU WISH TO LEAVE? " INPUT
   5 F w@ MIN TEMP1 !         ( no more than you have)
   5 F w@ TEMP1 @ - 5 F w!     ( update legions on fleet)
   XY@ INFO2 C@ TEMP1 @ + 255 MIN ( no more than 255)
   XY@ INFO2 C! ;            ( update on planet)

: FRIENDLY-PLANET   ( options upon landing at colony)
	1 tcount +!
   BEGIN
      33 colorize
      10 0 vhtab ." CLASS " XY@ INFO1 C@ 8 / 2 .R
      ."  PLANET" 16 SPACES CR  ( give class of planet)
      ." LOCAL GARRISON IS " XY@ INFO2 C@ 3 .R ."  LEGIONS"
                                ( give size of local garrison)
      12 0 vhtab ." DO YOU WISH TO:" 12 SPACES ( give options)
      CR ." 1.  LEAVE LEGIONS ON PLANET"
      CR ." 2.  GATHER LEGIONS FROM PLANET"
      CR ." 3.  BUY SHIPS"
      CR ." 4.  ENLIST TROOPS"
      CR ." 5.  LEAVE" CR
      KEY 127 AND              ( get reply)
      CLEAR-MSGE
      CASE
         49 ( 1) OF DEPLOY 0 ( leave legions)  ENDOF
         50 ( 2) OF GATHER 0 ( gather legions) ENDOF
         51 ( 3) OF BUY 0    ( buy ships)      ENDOF
         52 ( 4) OF ENLIST 0 ( en1ist troops)  ENDOF
                           1 ( the default: leave planet)
      ENDCASE DELAY
   UNTIL
   33 colorize
   CLEAR-MSGE DRAW-DISPLAY 
;

: COLONISE ( attack an uncolonised planet)
	1 tcount +!
   CLEAR-MSGE
   XY@ INFO1 C@ 8 / RANDOM1 1 - 5 / 7 + * 10 / DUP TEMP1 !
   ( calaculate relative strength of planet)
   5 F w@ >
   IF   ( planet drives off your forces)
      10 0 vhtab ." YOUR FORCES RETREAT"
      12 0 vhtab ." YOUR LOSSES = "
      5 F w@ 2 / DUP 3 .R 5 F w@ SWAP - 5 F w!
      DELAY DELAY
   ELSE ( you capture planet)
      10 0 vhtab ." PLANET CAPTURED"
      12 0 vhtab ." YOUR LOSSES = "
      TEMP1 @ 3 .R
      5 F w@ TEMP1 @ - 5 F w!   ( update legions in fleet)
      1 PLANETS +!            ( increment no. of planets)
      200 score +! 		\ capture planets increases score now
      132 XY@ GALAXY C!       ( colony symbol in galaxy)
      DELAY DELAY
      FRIENDLY-PLANET
   ENDIF 
;

: EMPTY-PLANET   ( in orbit round uncolonised planet)
	1 tcount +!
   CLEAR-MSGE
   10 0 vhtab ." UNCOLONISED CLASS " XY@ INFO1 C@ 8 / 2 .R
   ." PLANET"
   12 0 vhtab ." DO YOU WISH TO ATTACK? " inkey 127 AND 89 =
   IF
      COLONISE
   ENDIF
   CLEAR-MSGE 
;

: NOT-PLANET   ( there isn't a planet where he's trying to land)
   10 0 vhtab ." NO PLANET THERE"
   DELAY CLEAR-MSGE 
;

: ATTACK       ( attack a planet controlled by the computer)
	1 tcount +!
   CLEAR-MSGE
   XY@ INFO2 C@ RANDOM1 1 - 5 / 7 + * 10 / DUP TEMP1 !
               ( calaculate enemy garrlsons effective strength)
   5 F w@ >
   IF   ( enemy garrison wins)
      10 0 vhtab ." YOUR FORCES RETREAT"
      12 0 vhtab ." YOUR LOSSES = "
      XY@ INFO2 C@ 5 F w@ * TEMP1 @ / 2 / XY@ INFO2 C@ SWAP
      - XY@ INFO2 C!
      5 F w@ 2 / DUP 3 .R 5 F w@ SWAP - 5 F w!
   ELSE
      0 XY@ INFO2 C!           ( reduce legions on planet to 0)
      10 0 vhtab ." PLANET CAPTURED"
      12 0 vhtab ." YOUR LOSSES = "
      TEMP1 @ 3 .R
      5 F w@ TEMP1 @ - 5 F w!    ( update legions with fleet)
      132 XY@ GALAXY C!        ( put colony in galaxy)
      1 PLANETS +!             ( increment planets)
	500 score +! 		\ increase score then ! 
      -1 C-PLANETS +!          ( decrement computer planets)
      XY@ INFO1 C@ 8 / MINUS CLASS-TOTALS +!
      DELAY                   ( reduce classes of compo plnts)
      FRIENDLY-PLANET
   ENDIF
   DELAY CLEAR-MSGE 
;

: ENEMY-PLANET   ( player orbits enemy planet)
   XY@ INFO1 C@ 8 /
   10 0 vhtab ." CLASS " 2 .R ."  PLANET" CR CR
   ." ENEMY GARRISON OF STRENGTH "
   XY@ INFO2 C@ 3 .R CR CR
   ." DO YOU WISH TO ATTACK? " inkey 127 AND 89 =
   IF
      ATTACK
   ENDIF
   CLEAR-MSGE 
;

: LAND   ( land on adjacent planet)
	1 tcount +!
   FIND-DIRECTION
   2DUP Y ! X ! GALAXY C@
   CASE
      4 ( uncolonised planet) OF EMPTY-PLANET    ENDOF
      5 ( computers planet)   OF ENEMY-PLANET    ENDOF
    132 ( players colony)     OF FRIENDLY-PLANET ENDOF
        NOT-PLANET  ( otherwise it's not a planet)
   ENDCASE 
;

: REVOLT? ( planet at X,Y revolts)
   12 0 vhtab ." PLANET AT " Y @ . X @ . ." REVOLTS" DELAY
   XY@ INFO1 C@ 8 / XY@ INFO2 C@ 2DUP >
   IF   ( revolt succeeds)
      DROP 4 XY@ GALAXY C!            ( place planet symbol)
      8 * 7 + XY@ INFO1 C!            ( set revolt factor 7)
      0 XY@ INFO2 C!                  ( set lpgions to 0)
      -1 PLANETS +!                   ( reduce no.of planets )
	-1000 score +! 			\ loosing planet makes a score crash ! 
      14 0 vhtab ." SUCCEEDS"
   ELSE ( revolt fails)
      SWAP 2 / - XY@ INFO2 C!         ( reduce legions)
      XY@ INFO1 C@ 7 OR XY@ INFO1 C!  ( set revolt factor 7)
      14 0 vhtab ." FAILS"
   ENDIF
   DELAY
   12 0 vhtab 30 SPACES
   14 0 vhtab 12 SPACES ;             ( clear messages)

: TAX     ( collect taxes on players planets)
	1 tcount +!
   0 VTAX !                           ( set tax to 0)
   10 0 vhtab ." TAX COLLECTED = "
   10 17 vhtab 0 .
   SIZE 1+ 1 DO
     SIZE 1+ 1 DO
                 I J GALAXY C@ 132 =
                 IF   ( it's a colony)
                    I J INFO1 C@ 3 * 5 / ( tax from planet)
                    VTAX @ + DUP VTAX !  ( update tax)
			VTAX @ score +! \ add tax on score ! 
                    10 17 vhtab 5 .R
                    I J INFO1 C@ 7 AND -DUP
                    IF ( doesn't revolt)
                       I J INFO1 DUP C@ 1 - SWAP C!
                    ELSE ( revolt)
                       I X ! J Y ! REVOLT?
                    ENDIF
                 ENDIF
               LOOP
             LOOP
   CREDIT @ VTAX @ + CREDIT !            ( update credit)
   CLEAR-MSGE DRAW-DISPLAY 
;

: COMPUTER-TURN   ( computers turn to do something)
   -1 NEW +!                            ( decrement NEW)
   NEW @ 0=
   IF    ( computer creates new fleet)
      7 EMIT                            ( ring bell)
      1 C-FLEETS +!                     ( update comp. fleets)
      29 4 DIFF @ * - NEW !             ( reset NEW)
      CLASS-TOTALS @ 8 / DUP C-LEGIONS +!
      DUP TROOPS +!
      BEGIN
         RANDOM1 RANDOM2 2DUP GALAXY C@ 0=
         IF  ( empty space in galaxy)
            2DUP 17 ROT ROT GALAXY C!   ( place fleet symbol)
            INFO2 C! 1                  ( plus legions)
         ELSE
            DROP DROP DROP 0
         ENDIF
      UNTIL
   ENDIF
   DIFF @ 0 DO   ( see if computer colonises planet)
      RANDOM1 RANDOM2 2DUP GALAXY C@
      CASE
         4 OF ( empty planet)
              2DUP 2DUP 5 ROT ROT GALAXY C! ( place colony)
              C-LEGIONS @ 2 / DUP C-LEGIONS !
              ROT ROT INFO2 C!
              1 C-PLANETS +!
              INFO1 C@ 8 / CLASS-TOTALS +! ENDOF
       132 OF ( players planet)
              2DUP Y ! X ! INFO2 C@ C-LEGIONS @ 2 / <
              IF ( captures planet)
                 C-LEGIONS @ 3 / C-LEGIONS !
                 5 XY@ GALAXY C!
                 XY@ INFO1 C@ 8 / CLASS-TOTALS +!
                 1 C-PLANETS +!
                 -1 PLANETS +!
                 5 0 DO 7 EMIT LOOP ENDIF ENDOF
           DROP DROP
      ENDCASE
   LOOP
   DRAW-FIGURES 
;

: FIRE     ( players fleet attacks computer fleet)
	1 tcount +!
   0 X !
   2 F C@ 2 + DUP 3 - DO
      1 F C@ 2 + DUP 3 - DO
         I EDGE-CHECK J EDGE-CHECK GALAXY C@ 17 =
         IF  ( there's a fleet in range)
            I EDGE-CHECK X ! J EDGE-CHECK Y !
         ENDIF
      LOOP
   LOOP
   X @ 0=
   IF
      10 0 vhtab ." NO ENEMY FLEET IN RANGE"
   ELSE
      3 F w@ XY@ INFO2 C@ OVER 4 * 10 /
      OVER 4 * 10 / DUP
      10 0 vhtab ." FLEET HIT BY " 5 .R ." UNITS" start @ 100 + start ! \ add 1 to score on combat
      ROT ROT - 0 MAX DUP 0=
      IF ( computers fleet destroyed)
      DROP TROOPS @ XY@ INFO2 C@ - TROOPS !
      start @ 400 + start ! \ add 5 to score on winning a fight
      ( reduce computers troops)
      0 XY@ GALAXY C!          ( destroy fleet symbol)
         -1 C-FLEETS +!        ( reduce comps fleets)
      ELSE
         XY@ INFO2 C@ OVER - TROOPS @ SWAP - TROOPS !
         ( reduce spare troops)
         XY@ INFO2 C!          ( reduce legions in fleet)
      ENDIF
      - 0 MAX DUP 0=
      IF  ( players fleet destroyed)
         DROP NEW-FLEET
      ELSE
         3 F w!
      ENDIF
   ENDIF
   DELAY DELAY DRAW-DISPLAY CLEAR-MSGE 
;

: exitprog ( -- ) 
	\ quit properly program
	PLANETS @ C-PLANETS @ - W1 * abs score ! 
	highscore?		\ write highscore if possble (if player made it)
	page			\ clearscreen
	cr ." Your final score was : " score @ . cr
	0 colorize		\ restore colors
	s" tput cnorm" system 	\ restore cursor
	bye
;

HEX

: OBEY-COMMAND
   BUY-V @ -DUP ( fetch BUY-V, duplicate if nonzero)
   IF ( nonzero)
      1 - BUY-V !
   ENDIF
   inkey
   CASE
      ( J) 4A OF MOVE-LEFT   ENDOF
      ( L) 4C OF MOVE-RIGHT  ENDOF
      ( I) 49 OF MOVE-UP     ENDOF
      ( k) 4B OF MOVE-DOWN   ENDOF
      ( C) 43 OF OTHER-FLEET ENDOF
      ( R) 52 OF KEY ENDOF
      ( G) 47 OF LAND        ENDOF
      ( T) 54 OF TAX         ENDOF
      ( F) 46 OF FIRE        ENDOF
      ( Q) 51 OF exitprog    ENDOF
      ( q) 71 OF exitprog    ENDOF
   ENDCASE
   \ 24 0 vhtab .s ( print current state of stack )
   \ not sure why sp! is required here and it appears to be syntactically different from gForth
   \ this should reset the parameter stack, are there values ever left lying around?
   \ SP! 
;

\ think this needs to be here to reset base
decimal

: COMPUTER?    ( is it the computers turn or not)
	1 tcount +!
   COMPUTER @ 1 - DUP 0=
   IF
      COMP-START @ COMPUTER ! DROP 1
   ELSE
      COMPUTER ! 0
   ENDIF 
;

: othergamend? ( -- ) 
	\ other conditions to end the game
	start @ -5000 <= if 31 colorize ." Critical score : Game ENDED ! " key exitprog then 
	LEN @ 0 <= if 31 colorize ." to much moves : Game ENDED ! " key exitprog then 
	tcount @ 35000000 >= if 31 colorize ." You played to much : Game ENDED ! " key exitprog then 
;

: RESTART        ( restarts the stopped game)
	1 tcount +!
   rnd
   CLEAR-DISP
   HOME DRAW-BORDERS DRAW-DISPLAY
   BEGIN
      \ ?TERMINAL
      \ I think this means something else here, is ?TERMINAL meant to mean "check for keypress"
      \ rather than "check for break" ?
      key?
      IF    ( player has pressed a key)
         OBEY-COMMAND
         \ -1 LEN + !
         \ COMPUTER-TURN
         100 ms
      ENDIF
      COMPUTER?
      IF
         COMPUTER-TURN
      ENDIF
      othergamend? ( exit ? ) \ new way to end the game
      LEN @ 0=
   \ slow the game a little by printing how many loops until it's the computer's turn
   0 30 vhtab computer @ .
   UNTIL
   END-MSGE 
;

: CONQUEST  ( the main game word)
	1 tcount +!
   checkversion 
   page	\ clear screen
   31 colorize
   s" gamedata/title.txt" slurp-file type rand1 ! cr		\ title 
   33 colorize
   s" tput civis" system					\ hide cursor to avoid it blinking 
   s" gamedata/author.txt" slurp-file cr type key Rand2 ! cr 	\ auhor
   INITIALISE
   RESTART 
;

CONQUEST	\ main
BYE
