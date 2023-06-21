( TARGET SPECIFIC WORDS)
27 constant ESC
create fname 8 allot s" .score" fname place						\ nice way to assign a file name 
variable #src-fd-in
variable 'src-fd-in
variable tobeat 									\ to know if new high score is reached
variable fd-in
0 value fid1
: COLORIZE ( n -- ) 
	\ just like terminal ascii \e[31m, here use 31 colorize 
	ESC EMIT ." [" base @ >R 
	0 <# #S #> type 
	R> base ! ." m"  
; 

\ Forth words for HIRES-mode graphics
\ given these dialect-specific words an "adaptor shim" could be made for any other Forth
: hcolour ( colour ---) drop ;	( select current colour for HIRES drawing mode)
: hline ( x y --- ) 2drop ;	( draw HIRES mode line to position)
: hposn ( x y --- ) 2drop ;	( moves HIRES mode pixel cursor)
: scale ( scale --- ) drop ;	( sets shape table scaling value)

\ implement home, hclr and vhtab using ANSI commands
\ SEE: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797

: home ( --- ) ( set cursor to home position, using ANSI codes)
	.\" \e[H" 
;

\ real HCLR would clear HIRES1
: hclr ( --- ) ( clear screen using ANSI codes)
	page
;

: vhtab ( y x --- ) ( position cursor on screen, using ANSI codes)
	swap .\" \e[" 
	0 <# #s #> type
	." ;"
	0 <# #s #> type ." H" 
;

: draw ( addr delim --- ) ( draw shape table, presumably first value is address and second is delimiter)
   2drop  ( discard values from stack)
;

( POSSIBLE FIG WORDS NOT SUPPORTED IN ANS FORTH)
\ HELPFUL RESOURCE: https://dwheeler.com/6502/fig-forth-glossary.txt
\ Helpful book: Forth Fundamentals Vol. 2, C. Kevin McCabe, dilithium Press 1983, ISBN 0-88056-092-4
: MINUS negate	 ( FFv2 page 97) ;
: -DUP ?dup	 ( FFv2 page 39) ;

\ this woudln't be a problem on the Apple ][ but on modern systems the KEY routine is case sensitive

: inkey ( --- key)
   key dup dup
   96 >
      if ( ASCII value 'a' or higher)
         123 <
            if ( ASCII value 'z' or lower)
               223 and ( mask off upper/lower case bit)
            endif
     endif
;    

: checkversion ( -- exit|continue ) 
	\ check gforth version 
	version-string 2 - s\" 0.7" str= 0 >= if 	\ easy method to get tooling version
		cr 
		." you might update your gnu-forth version" cr
		." prehistoric age is over" cr
		cr
		0 colorize
		s" tput cnorm" system
		page					\ if too old gforth exit ( might not be so possible but ... just in case... ) 
		bye
	then
;
\ Read highscore from file & if needed update the file with a new highscrore 
: readfile
	here 'src-fd-in ! 							\ ram position
	fname count r/o open-file throw fd-in !
	\ s" gamedata/.score" r/o open-file throw fd-in !
	here 16 fd-in @ read-file throw 
	dup allot								\ one alloc = 1 line
	fd-in @ close-file throw						\ now close file
	here 'src-fd-in @ - #src-fd-in ! 					\ get allocated
	'src-fd-in @ #src-fd-in @ s>number drop tobeat ! 

;
: highscore? ( finalscore > fd-in -- file )
	tobeat @  score @  < if
		fname count file-status nip if i				\ fileexists ?
			fname count r/w create-file throw
		else
			fname count r/w open-file throw
		then to fid1 							\ do not forget the file ID
		score @ s>d <# #s #> 						\ format score as a string
		fid1 write-line throw						\ write it on file 
		fid1 close-file throw						\ make real save of file 
		0 colorize
	then
;
