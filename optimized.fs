
\ Start hash table at larger size
15 :noname to hashbits hashdouble ; execute

65536 constant buf-size
create buf buf-size allot  \ Buffer for read-file
wordlist constant counts   \ Hash table of words to count
variable num-uniques  0 num-uniques !

\ Convert character to lowercase.
: to-lower ( C -- c )
    dup [char] A [ char Z 1+ ] literal within if
        32 +
    then ;

\ Convert string to lowercase in place.
: lower-in-place ( addr u -- )
    over + swap ?do
        i c@ to-lower i c!
    loop ;

\ Count given word in hash table.
: count-word ( c-addr u -- )
    2dup counts find-name-in dup if
        ( name>interpret ) >body 1 swap +! 2drop
    else
        drop nextname create 1 ,
        1 num-uniques +!
    then ;

\ Process text in the buffer.
: process-string ( -- )
    begin
        parse-name dup
    while
        count-word
    repeat
    2drop ;

\ Less-than for words (true if count is *greater* for reverse sort).
: count< ( nt1 nt2 -- )
    >r name>interpret >body @
    r> name>interpret >body @
    > ;

\ In-place merge sort taken from Rosetta Code:
\ https://rosettacode.org/wiki/Sorting_algorithms/Merge_sort#Forth
: merge-step ( right mid left -- right mid+ left+ )
    over @ over @ count< if
        over @ >r
        2dup - over dup cell+ rot move
        r> over !
        >r cell+ 2dup = if  rdrop dup  else  r>  then
    then
    cell+ ;

: merge ( right mid left -- right left )
    dup >r begin
        2dup >
    while
        merge-step
    repeat
    2drop r> ;

: mid ( l r -- mid )
    over - 2/ cell negate and + ;

: mergesort ( right left -- right left )
    2dup cell+ <= if
        exit
    then
    swap 2dup mid recurse rot recurse merge ;
 
: sort ( addr len -- )
    cells over + swap mergesort 2drop ;

\ Append word from wordlist to array at given offset.
: append-word ( addr offset nt -- addr offset+cell true )
    dup name>string lower-in-place
    >r  2dup + r> swap !
    cell+ true ;

\ Show "word count" line for each word, most frequent first.
: show-words ( -- )
    num-uniques @ cells allocate throw
    0 ['] append-word counts traverse-wordlist drop
    dup num-uniques @ sort
    num-uniques @ 0 ?do
        dup i cells + @
        dup name>string type space
        name>interpret >body @ . cr
    loop
    drop ;

\ Find last LF character in string, or return -1.
: find-eol ( addr u -- eol-offset|-1 )
    begin
        1- dup 0>=
    while
        2dup + c@ 10 = if
            nip exit
        then
    repeat
    nip ;

: main ( -- )
    counts set-current  \ Define into counts wordlist
    0 >r  \ offset after remaining bytes
    begin
        \ Read from remaining bytes till end of buffer
        buf r@ + buf-size r@ - stdin read-file throw dup
    while
        \ Process till last LF
        buf over r@ + find-eol
        dup buf swap ['] process-string execute-parsing
        \ Move leftover bytes to start of buf, update offset
        dup buf + -rot  buf -rot  - r@ +
        r> drop dup >r  move
    repeat
    drop r> drop
    show-words ;

main
bye
