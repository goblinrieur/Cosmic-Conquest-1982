( TARGET SPECIFIC WORDS)
\ targets might be only PC/Smartphones/TAbs now ...  so ...  feel free to use high level modern gforth words words
27 constant ESC
\
create fname 8 allot s" .score" fname place			\ nice way to assign a file name 
\
variable #src-fd-in
variable 'src-fd-in
variable tobeat 									\ to know if new high score is reached
variable fd-in
\
0 value fid1
\ words
: COLORIZE ( n -- ) 
	\ just like terminal ascii \e[31m, here use 31 colorize 
	ESC EMIT ." [" base @ >R 0 <# #S #> type R> base ! ." m"  
; 
: home ( -- ) ( set cursor to home position, using ANSI codes)
	.\" \e[H" 
;
\
: vhtab ( y x -- x y -- ) ( position cursor on screen, using ANSI codes)
	swap at-xy
;
\ this woudln't be a problem on the Apple ][ but on modern systems the KEY routine is case sensitive
HEX		\ thos inkey replacement is idea from bfox9900 reddit.com (":; r/forth" section) 
: ?lower ( c -- ? ) [CHAR] a [CHAR] z 1+ WITHIN ;
: toupper ( c -- C ) DUP ?lower if 5F and then ; 
: inkey ( --c ) key 7f and toupper ; 
decimal
\ not really needed as far as I guess noone uses versions of gforth older than 0.7.x
: checkversion ( -- exit|continue ) 
	\ check gforth version 
	version-string 2 - s\" 0.7" str= 0 >= if 		\ easy method to get tooling version
		cr 
		." you might update your gnu-forth version" cr
		." prehistoric age is over" cr
		cr
		0 colorize
		s" tput cnorm" system
		page					\ if too old gforth exit ( might not be so possible but ... just in case... ) 
		1 (bye)
	then
;
\ Read highscore from file & if needed update the file with a new highscrore 
: readfile
	here 'src-fd-in ! 									\ ram position
	fname count r/o open-file throw fd-in !				\ no problems if file is not existing because variable is 0 at start
	here 16 fd-in @ read-file throw 
	dup allot											\ one alloc = 1 line
	fd-in @ close-file throw							\ now close file
	here 'src-fd-in @ - #src-fd-in ! 					\ get allocated
	'src-fd-in @ #src-fd-in @ s>number drop tobeat ! 
;
: highscore? ( finalscore > fd-in -- file )
	    score @ tobeat @ > if
		fname count file-status nip if i				\ fileexists ?
			fname count r/w create-file throw			\ if now create it 
		else											\ if yes continue
			fname count r/w open-file throw
		then to fid1 								\ do not forget the file ID
		score @ s>d <# #s #> 						\ format score as a string
		fid1 write-line throw						\ write it on file 
		fid1 close-file throw						\ make real save of file 
		0 colorize
	then
;
