( TARGET SPECIFIC WORDS)
27 constant ESC
: COLORIZE ( n -- ) 
	\ just like terminal ascii \e[31m, here use 31 colorize 
	ESC EMIT ." [" base @ >R 
	0 <# #S #> type 
	R> base ! ." m"  
; 

\ Forth words for HIRES-mode graphics
\ given these dialect-specific words an "adaptor shim" could be made for any other Forth
: hcolour ( colour ---) drop ; ( select current colour for HIRES drawing mode)
: hline ( x y --- ) drop drop ; ( draw HIRES mode line to position)
: hposn ( x y --- ) drop drop ; ( moves HIRES mode pixel cursor)
: scale ( scale --- ) drop ; ( sets shape table scaling value)

\ implement home, hclr and vhtab using ANSI commands
\ SEE: https://gist.github.com/fnky/458719343aabd01cfb17a3a4f7296797

: home ( --- ) ( set cursor to home position, using ANSI codes)
   .\" \e[H" ;

\ real HCLR would clear HIRES1
: hclr ( --- ) ( clear screen using ANSI codes)
   .\" \e[2J" ; ( '.\" " is hard to Google for but means print, using C-style escape sequences) 

: vhtab ( y x --- ) ( position cursor on screen, using ANSI codes)
   swap .\" \e[" 
   0 <# #s #> type
   ." ;"
   0 <# #s #> type ." H" ;

: h1 ( --- ) ; ( selects HIRES mode 1, without clearing screen)
: text ( --- ) ( selects TEXT screen leaving HIRES1 unchanged) ;

: draw ( addr delim --- ) ( draw shape table, presumably first value is address and second is delimiter)
   drop drop ; ( discard values from stack)

( POSSIBLE FIG WORDS NOT SUPPORTED IN ANS FORTH)
\ HELPFUL RESOURCE: https://dwheeler.com/6502/fig-forth-glossary.txt
\ Helpful book: Forth Fundamentals Vol. 2, C. Kevin McCabe, dilithium Press 1983, ISBN 0-88056-092-4
: MINUS negate ( FFv2 page 97) ;
: -DUP ?dup ( FFv2 page 39) ;

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
		page
		bye
	then
;
     
