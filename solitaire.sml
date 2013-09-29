
(*
Comp 212 Solitaire
Daniel Cohen
Sam Nebel
*)
structure Solitaire =
struct


(**********GAME SET-UP: TYPES/FUNCTIONS***********)
(*************************************************)

datatype suit_t = Heart | Diamond | Club | Spade
datatype number_t = Num of int | Ace | Jack | Queen | King

(*card type is a tuple of number type and suit type *)
datatype card_t = Card of number_t * suit_t
datatype color_t = Red | Black

(* found_t is in order (Heart, Diamond, Club, Spade) 
*)
type found_t = (card_t list)*(card_t list)*(card_t list)*(card_t list)
type column_t = (card_t list)*(card_t list)
type tab_t = column_t list
type deck_t = card_t list
(*takes suit_t as a paramter and returns the suit's color (color_t) as the output*)
fun color(Heart) = Red
  | color(Diamond) = Red
  | color(Club) = Black
  | color(Spade) = Black

(* makeDeck() returns a proper deck of 52 cards 
*)
local
  (* takes an int and a suit_t as parameters and returns the corresponding Card
  *)
  fun intSuitToCard(1,s:suit_t)=Card(Ace,s)
    | intSuitToCard(11,s)=Card(Jack,s)
    | intSuitToCard(12,s)=Card(Queen,s)
    | intSuitToCard(13,s)=Card(King,s)
    | intSuitToCard(x,s)=Card(Num(x),s);

  (* creates a card_t list of each suit
   * makeDeck'(n,s) = ([c_0,...c_{n-1}]) where every c is a card with the same suit
  *)
  fun makeDeck'(0,s)=[]
	| makeDeck'(n,s)=intSuitToCard(n,s)::makeDeck'(n-1,s);
in
  fun makeDeck():deck_t =
    makeDeck'(13,Heart)@makeDeck'(13,Diamond)@
	makeDeck'(13,Spade)@makeDeck'(13,Club)
end


local
  (* Returns the leftover deck and table after table is dealt
  *)
  fun makeDeckAndTable(x::xs: deck_t): (deck_t * tab_t) =
  let
	(* makeCol([y_0,...,y_{p-1}],n,[z_0,...,z_{q-1}) = ([y_n-1],[y_0,...,y_n-2])
	*)
	fun makeCol([]:card_t list,_:int, _:card_t list) : column_t=([],[])
  	| makeCol(x::xs, 1, zs) = ([x],zs)
  	| makeCol(x::xs, n, zs)= makeCol(xs,n-1,x::zs)

    (* takes the original deck as a parameter and creates the tableau by creating 7 columns
    *)
	fun makeTab(xs: deck_t): tab_t =
 	[makeCol(xs,7,[]),
  	makeCol(List.drop(xs,7),6,[]),
  	makeCol(List.drop(xs,13),5,[]),
  	makeCol(List.drop(xs,18),4,[]),
  	makeCol(List.drop(xs,22),3,[]),
  	makeCol(List.drop(xs,25),2,[]),
  	makeCol(List.drop(xs,27),1,[]) ]

	val remainingdeck: deck_t = List.drop(x::xs, 28)
	val tablecards = List.take(x::xs, 28)
	val startingtable = makeTab(tablecards)
  in
	(remainingdeck, startingtable)
  end
in
  (* Returns a board consisting of (deck, dis, found, table)
  *)
  fun dealDeck(xs : deck_t) =
  let
	val (newdeck:deck_t, table:tab_t) = makeDeckAndTable(xs)
	val discard:deck_t = []
	val found:found_t = ([],[],[],[])
  in
	(newdeck, discard, found, table)
  end
end

(*takes a card as a parameter and returns its value of the number_t as an int
*)
fun cardToInt(Card(Ace,s)):int=1
  | cardToInt(Card(King,s))=13
  | cardToInt(Card(Queen,s))=12
  | cardToInt(Card(Jack,s))=11
  | cardToInt(Card(Num(x),s))=x

(* takes a card as a parameter and returns a string of the number_t
 * where getNumberString Ace="A",getNumberString Num(5)= "5", etc 
*)
fun getNumberString(Card(Ace,s):card_t) = "A"
  | getNumberString(Card(King,s)) = "K"
  | getNumberString(Card(Queen,s)) = "Q"
  | getNumberString(Card(Jack,s)) = "J"
  | getNumberString(Card(Num(x),s)) = Int.toString(x)

(* takes a card as a parameter and returns its suit type 
*)
fun getSuit(Card(n,s):card_t):suit_t = s

(* takes a card as a parameter and returns its suite_t as a string
 *)
fun getSuitString(Card(_,Heart):card_t) = "H"
  | getSuitString(Card(_,Diamond)) = "D"
  | getSuitString(Card(_,Spade)) = "S"
  | getSuitString(Card(_,Club)) = "C"

(* takes a card as a parameter and returns its string representation
 * where cardToString(Card(Ace,"Heart")) = A-H,
 * cardToString(Card(Num(5),Spade))=5-S, etc.
*)
fun cardToString(x:card_t):string = getNumberString(x) ^ "-" ^ getSuitString(x)

(***************END OF GAME SET UP****************)


(******BEGINNING OF SHUFFLE AND GETUSERINPUT******)
(*************************************************)

(*  shuffle xs = ys, where ys is a random permutation of xs.
*)
local
  val seed =
  let
    val tNow = Time.toMicroseconds(Time.now())
    val (_, p) = IntInf.divMod(tNow, IntInf.fromInt (valOf(Int.maxInt)))
  in
    Random.rand(0, IntInf.toInt p)
  end

  (*  getNth([x_0,...,x_{n-1}], i) =
  *   (x_i, [x_0,...,x_{i-1},x_{i+1},...,x_{n-1}]).
  *)
  fun getNth([], _) = raise Subscript
    | getNth(x :: xs, 0) = (x, xs)
    | getNth(x :: xs, n) =
      let
        val (y, ys) = getNth(xs, n-1)
      in
        (y, x :: ys)
      end

  (*  shuffle' (xs, n, acc) = ys @ acc, where ys is a random permutation
  *   of xs.  Pre-condition:  n = length xs.
  *)
  fun shuffle' ([], 0, acc) = acc
    | shuffle' (xs, n, acc) =
      let
        val m = Random.randRange(0, n-1) seed
        val (y, ys) = getNth(xs, m)
      in
        shuffle' (ys, n-1, y :: acc)
      end
in
  fun shuffle(xs) = shuffle' (xs, length xs, [])
end


(* getUserInput(msg) = x, where x is the string user enters
 * in repose to message.
*)
fun getUserInput(msg) =
let
  (*  The type of whitespace we are seeing or looking for.
  *)
  datatype ws_t = Leading | Trailing

  (*  trimWS [c_0,...,c_{k-1}] = [c_i,...,c_j], where c_i is the
  *   first non-whitespace character and c_j is the first
  *   trailing whitespace character.  Pre-condition:  c_{k-1} = #"\n".
  *)
  fun trimWS ((#" " | #"\t") :: cs, Leading) = trimWS (cs, Leading)
    | trimWS (c :: cs, Leading) = c :: (trimWS (cs, Trailing))
    | trimWS ((#" " | #"\t" | #"\n") :: cs, Trailing) = []
    | trimWS (c :: cs, Trailing) = c :: (trimWS (cs, Trailing))

  (*  trimWhiteSpace s = s', where s is obtained from s' by
  *   removing all leading whitespace and all characters following
  *   the first trailing whitespace character.
  *)
  fun trimWhiteSpace(s) = implode (trimWS(explode s, Leading))

  val () = print (msg ^ " ")
  val SOME(resp) = TextIO.inputLine(TextIO.stdIn)
in
  trimWhiteSpace(resp)
end

(*********END OF SHUFFLE AND GETUSERINPUT*********)


(********METHODS USED IN EXECUTING A MOVE*********)
(**************************************************)

(* grabUp takes a column and returns the face-up cards
*)
fun grabUp((xs,ys):column_t):card_t list = xs

(* grabDown takes a column and returns the face-down cards
*)
fun grabDown((xs, ys):column_t):card_t list = ys

(* replaceCol(table, i, col) = table'. This function replaces
 * the ith column in table with col. 
*)
fun replaceCol(ys:tab_t, i:int, z:column_t):tab_t =
let
  val part1 = List.take(ys, i)
  val part2 = List.drop(ys, i)
in
  part1@[z]@tl(part2)
end

(* replaceColumns (table, j, colj', k, colk') outpus table'
 * by replacing table's jth column with colj' and replacing 
 * table's kth column with colk'
*)
fun replaceColumns(xs:tab_t, j:int, colj':column_t, k:int, colk':column_t) =
let
  val xs' = replaceCol(xs, j, colj')
in
  replaceCol(xs', k, colk')
end

(* addToTable (table, card) outputs table' by asking the user for
 * an int, j, and adding card to the jth column of table
*)
local
  (* addCardToCol (table, card, j) outputs table' by making card
   * the first face-up card in table's jth column
  *)
  fun addCardToCol(table:tab_t, x:card_t, j:int) =
  let
    val jcol:column_t = List.nth(table, j)
    val (jup, jdown):column_t = jcol
    val jcol':column_t = (x::jup, jdown)
  in
    replaceCol(table, j, jcol')
  end
in
  fun addToTable(table:tab_t, x:card_t):tab_t =
  let
    val resp = getUserInput("Move card to which column 0-6?")
  in
    case resp of
      "0" => addCardToCol(table, x, 0)
    | "1" => addCardToCol(table, x, 1)
    | "2" => addCardToCol(table, x, 2)
    | "3" => addCardToCol(table, x, 3)
    | "4" => addCardToCol(table, x, 4)
    | "5" => addCardToCol(table, x, 5)
    | "6" => addCardToCol(table, x, 6)
    |  _  => (print("ILLEGAL COLUMN \n"); addToTable(table, x))
  end
end

(* changeCols (col1, col2) ouputs (col1', col2') by taking the
 * top card of col1 and adding it to the top of col2.
 * Also handles flipping over a face-down card if necessary.
*)
fun changeCols(x:column_t, y:column_t) =
let
  val up1 = grabUp(x)
  val down1 = grabDown(x)
  val up2 = grabUp(y)
  val down2 = grabDown(y)
  val a = hd(up1)

  val col1 : column_t = 
    if up1=[] then raise Subscript
    else if tl(up1)=[] then (
      if down1=[] then ([],[])
      else ([hd(down1)], tl(down1)) )
    else (tl(up1), down1)

  val col2 : column_t = 
    if up2=[] andalso down2=[] then (
      (*CAN ONLY MOVE A KING TO AN EMPTY COLUMN*)
      if getNumberString(hd(up1))="K" then (hd(up1)::up2, down2)
      else raise Subscript )
    else (hd(up1)::up2, down2)
in
  (col1, col2)
end 


(* shiftTable(table, j, k) ouputs table' by moving the top card
 * of the jth column of table to the top kth column of table
*)
fun shiftTable(table:tab_t, j, k):tab_t = 
let
  val colj = List.nth(table, j)
  val colk = List.nth(table, k)
  val (colj', colk') = changeCols(colj, colk)
in
  replaceColumns(table, j, colj', k, colk')
end

(* addToFounds(founds, card) outputs founds' by adding card to 
 * the appropriate card_t list in founds. Recall that founds
 * has the form ([Hearts], [Diamonds], [Clubs], [Spades])
*)
fun addToFounds((hs, ds, cs, ss): found_t, x: card_t):found_t =
  if getSuit(x) = Heart then (x::hs, ds, cs, ss)
  else if getSuit(x) = Diamond then (hs, x::ds, cs, ss)
  else if getSuit(x) = Club then (hs, ds, x::cs, ss)
  else (hs, ds, cs, x::ss)

(* colToFounds(table, founds, j) outputs founds' by taking the
 * top card of the jth column of table and adding it to founds 
*)
fun colToFounds(table:tab_t, founds:found_t, j:int)=
let
  val jcol = List.nth(table, j)
  val topcard = hd(grabUp(jcol))
in
  addToFounds(founds, topcard)
end

(* reduceTable(table, j) outputs table' by removing the top 
 * card of column j and flipping over a new card if necessary
*)
fun reduceTable(table:tab_t, j) =
let
  val (jups, jdowns) = List.nth(table, j)
  val jcol' = 
    if tl(jups)=[] then (
      if (jdowns)=[] then ([],[])
      else ([hd(jdowns)], tl(jdowns)) )
    else  (tl(jups), jdowns)
in
  replaceCol(table, j, jcol')
end

(***METHODS THAT HELP WITH EXECUTING A MOVE END***)


(******METHODS THAT DIRECTLY EXECUTE A MOVE*******)
(*************************************************)

(* D: DRAWING A CARD FROM THE DECK
 *
 * drawCard(deck, dis, founds, table) ouputs (deck', dis', founds', table')
 * by removing and playing the top card from the deck.
 * this card can either be added to the table, added to
 * to the foundation, or added to the discard pile
*)
fun drawCard(x::xs:deck_t, dis:deck_t, founds, table) = 
let
  val () = print("You are playing"^" "^cardToString(x)^" ")
  val resp = getUserInput("Move to (C)olumn, (F)ound, or (D)iscard")
in
  ();
  case resp of 
    "C" => (xs, dis, founds, addToTable(table, x))
  | "F" => (xs, dis, addToFounds(founds, x), table)
  | "D" => (xs, x::dis, founds, table)
  |  _  => drawCard(x::xs, dis, founds, table)
end


(* C: MOVING A CARD FROM ONE COLUMN TO ANOTHER. 
 *
 * moveToCol(deck, dis, founds, table) ouputs (deck, dis, founds, table')
 * by asking the user for two columns, i,j, and moving the top card
 * column i to the top of column j
*)
local
  (* test(x,y) = true if x and y are string representations of
   * legal column numbers, false otherwise
  *)
  fun test(x:string, y:string) =
    (x="0" orelse 
     x="1" orelse 
     x="2" orelse
     x="3" orelse
     x="4" orelse
     x="5" orelse 
     x="6")
    andalso
    (y="0" orelse 
     y="1" orelse 
     y="2" orelse 
     y="3" orelse 
     y="4" orelse 
     y="5" orelse
     y="6")
    andalso
    (x<>y)

  (* toInt(x) changes x to its Integer representation.
   * will only be called on strings passing test(x,y)
  *)
  fun toInt(x:string) =
    if x="0" then 0
    else if x="1" then 1
    else if x="2" then 2
    else if x="3" then 3
    else if x="4" then 4
    else if x="5" then 5
    else if x="6" then 6
    else raise Subscript 
in
  fun moveToCol(deck:deck_t, dis:deck_t, founds:found_t, table:tab_t) =
  let
    val r1 = getUserInput("From which column 0-6?")
    val r2 = getUserInput("To which column 0-6?")
  in
    case test(r1, r2) of
      true => (deck, dis, founds, shiftTable(table, toInt(r1), toInt(r2)))
    | false => (print("ILLEGAL COLUMNS \n"); 
                 moveToCol(deck, dis, founds, table) )
  end
end
  

(* F: MOVE CARD FROM A COLUMN TO A FOUNDATION
 *
 * moveToFounds(deck, dis, founds, table) outputs (deck, dis, founds', table')
 * by asking the user for a column number, j, and moving the top card of
 * column j to the appropriate foundation
*)
fun moveToFounds(deck: deck_t, dis:deck_t, founds:found_t, table:tab_t) =
let
  val resp = getUserInput("From which column 0-6?")
in
  case resp of
    "0" => (deck, dis, colToFounds(table, founds, 0), reduceTable(table, 0))
  | "1" => (deck, dis, colToFounds(table, founds, 1), reduceTable(table, 1))
  | "2" => (deck, dis, colToFounds(table, founds, 2), reduceTable(table, 2))
  | "3" => (deck, dis, colToFounds(table, founds, 3), reduceTable(table, 3))
  | "4" => (deck, dis, colToFounds(table, founds, 4), reduceTable(table, 4))
  | "5" => (deck, dis, colToFounds(table, founds, 5), reduceTable(table, 5))
  | "6" => (deck, dis, colToFounds(table, founds, 6), reduceTable(table, 6))
  |  _  => moveToFounds(deck, dis, founds, table)
end


(* P: PLAY DISCARD TO COLUMN OR FOUNDATION
 * 
 * playDiscard(deck, dis, founds, table) outputs (deck, dis', founds', table')
 * by removing the top card from the discard pile, and either adding it to 
 * the table or adding it to the proper foundation
*)
fun playDiscard(deck:deck_t, dis:deck_t, founds:found_t, table:tab_t) =
let
  val resp = getUserInput("Move to (C)olumn or Move to (F)oundation?")
in
  case resp of
    "C" => (deck, tl(dis), founds, addToTable(table, hd(dis)))
  | "F" => (deck, tl(dis), addToFounds(founds, hd(dis)), table)
  | _ => playDiscard(deck, dis, founds, table) (*ADD ERROR MESSAGE FOR WRONG INPUT*)
end

(* Q: QUITGAME PRINTS EXIT GAME *)
fun quitGame(_,_,_,_) = ([]:deck_t, []:deck_t, ([],[],[],[]):found_t, []:tab_t)


(**************PRINT FUNCTIONS****************)
(*********************************************)

(*prints the number of cards left in the deck
*)
fun deckString(d:deck_t):string=
  ("Number of cards left in deck: " ^ Int.toString(length(d)) ^".")

(*prints the number of cards in the discard pile 
*)
fun discardString([] : deck_t) : string = "Discard pile is empty."
  | discardString(x::xs)= ("The top card of discard pile: " ^ cardToString(x) ^ ".")

(* creates a string representing the foundations
*)
local
  (* grabs the 1st card_t list in the foundation
  *)
  fun foundToCol1(f: found_t) : card_t list= #1(f)
  (* grabs the 2nd card_t list in the foundation
  *)
  fun foundToCol2(f: found_t) : card_t list= #2(f)
  (* grabs the 3rd card_t list in the foundation
  *)
  fun foundToCol3(f: found_t) : card_t list= #3(f)
  (* grabs the 4th card_t list in the foundation
  *)
  fun foundToCol4(f: found_t) : card_t list= #4(f)
        
  (*prints the top card in a card_t list
  *)
  fun cardListToString([]: card_t list) : string = "----"
    | cardListToString (x::xs) = cardToString(x)
in
  fun foundationString(f: found_t) : string=
    (cardListToString(foundToCol1(f)) ^ " " ^ cardListToString(foundToCol2(f)) ^ " " ^
    cardListToString(foundToCol3(f)) ^ " " ^ cardListToString(foundToCol4(f)))
end

(* creates a string that represents all of the up cards in a column
*)
fun upString([] : card_t list) : string= ""
  | upString(x::xs)= cardToString(x) ^ " " ^ upString(xs)

(* creates a string that represents all of the down cards in a column
*)
fun downString([] : card_t list) : string= ""
  | downString(x::xs) = "**** " ^ downString(xs)
        
   
(* creates a string that represents a column
*)
fun colString(c: column_t)=
let
  val xs= grabUp(c)
  val ys=grabDown(c)
in
  upString(xs)^downString(ys)
end
       
(* creates a string that adds the strings of the columns
*)
fun tabString'(6:int, t : tab_t):string = "(6) "^colString(List.nth(t,6))^"\n"
  | tabString'(n, t) =
    ("(" ^ Int.toString(n) ^ ") " ^ colString(List.nth(t,n)) ^ "\n" ^ tabString'(n+1,t))

(* creates a string that represents the tableau
*)
fun tabString(t:tab_t)= tabString'(0,t)

(* prints the string versions of all of the parameters, creating the board
*)
fun boardPrint(deck:deck_t, discard: deck_t, found: found_t, tableau: tab_t)=
print("\n"^foundationString(found)^"\n"^tabString(tableau)^"\n"^discardString(discard)^"\n"^deckString(deck)^"\n")


(***********END PRINTING FUNCTIONS*************)


(***********CHECKING BOARD FUNCTIONS***********)
(**********************************************)


(* checkTable(table) outputs true if the table is 
 * in a valid Solitaire state after a single move: 
 * every column has the first card of a smaller number and different
 * suit than the card behind it, if there is one,
 * false otherwise
*)
local
  (* takes a column and ouputs true if the first face-up
   * face-up cards are of alternating suits
  *)
  fun legalColumnColor([]: card_t list) = true
    | legalColumnColor([x])=true
    | legalColumnColor(x::xs) =
      if color(getSuit(x))=color(getSuit(hd(xs))) then false
      else true
  
  (* takes a column and outputs true if the first face-up
   * card has number_t 1 less than the second card
  *)
  fun legalColumnNumber([]: card_t list) :bool = true
    | legalColumnNumber(x::[] : card_t list)=true
    | legalColumnNumber((x::xs) : card_t list)=
      if cardToInt(x)-cardToInt(hd(xs))= ~1 then true
      else false
        
  (* takes a column and returns true if it is in a valid state
  *)
  fun checkCol(c: column_t) =
  let
    val ups = grabUp(c)
    val legalnum = legalColumnNumber(ups)
    val legalcolor = legalColumnColor(ups)
  in
    (legalnum andalso legalcolor)
  end
in
  fun checkTable(table: tab_t) =
  let
    val c0 = checkCol(List.nth(table, 0))
    val c1 = checkCol(List.nth(table, 1))
    val c2 = checkCol(List.nth(table, 2))
    val c3 = checkCol(List.nth(table, 3))
    val c4 = checkCol(List.nth(table, 4))
    val c5 = checkCol(List.nth(table, 5))
    val c6 = checkCol(List.nth(table, 6))
  in
    (c0 andalso c1 andalso c2 andalso c3
    andalso c4 andalso c5 andalso c6)
  end
end

(* checkFounds takes a found_t and returns (bool1, bool2) if
 * where bool1=true if foundation is valid, false otherwise,
 * bool2=true if the user wins the game, false otherwise
*)   
local
  (* checkF takes a single foundation and returns true
   * if it is in a valid state, false otherwise
  *)
  fun checkF([] : card_t list)= true
    | checkF([x])=if getNumberString(x)="A" then true else false
    | checkF(x::xs : card_t list)=
      if cardToInt(x) - cardToInt(hd(xs)) = 1 then true
      else false

  (* checkWinF returns true if all the columns have a King on top,
   * false otherwise
  *)
  fun checkWinF([] : card_t list)= false
    | checkWinF(x::xs)=if getNumberString(x)="K" then true else false
in
  fun checkFounds(founds : found_t):bool*bool=
  let
    val f1= #1(founds)
    val f2= #2(founds)
    val f3= #3(founds)
    val f4= #4(founds)
  in
   ((checkF(f1) andalso checkF(f2) andalso checkF(f3) andalso checkF(f4)),
    (checkWinF(f1) andalso checkWinF(f2) andalso checkWinF(f3) andalso checkWinF(f4)))
  end
end

(* checkBoard(deck, dis, founds, table) outputs (bool1,bool2,bool3)
 * bool1=true if user can keep playing, false otherwise
 * bool2=true if user has won the game, false otherwise
 * bool3=true if game should quit, false otherwise
*)
fun checkBoard([],[],([],[],[],[]),_) : bool*bool*bool = (false,false,true)
  | checkBoard(deck:deck_t, dis:deck_t, founds:found_t, table:tab_t) =
    let
      val (legalf,winner) = checkFounds(founds)
      val legalt = checkTable(table)
    in
    ((legalf andalso legalt),winner,false)
    end

(********END CHECKING FUNCTIONS*********)


(*****WHATS EXECUTED TO ACTUALLY PLAY*******)
(*******************************************)

(* takes current board parameters and calls methods based on user input
 * that updates board parameters
*)
fun go1(deck:deck_t, dis:deck_t, founds:found_t, table:tab_t) =
let
  val resp = getUserInput("(D)raw card, Move card to (C)olumn, (F)oundation, (P)lay discard, (Q)uit?")
in
  case resp of
    "D" => drawCard(deck, dis, founds, table)
  | "C" => moveToCol(deck, dis, founds, table)
  | "F" => moveToFounds(deck, dis, founds, table)
  | "P" => playDiscard(deck, dis, founds, table)
  | "Q" => quitGame(deck, dis, founds, table)
  |  _  => go1(deck, dis, founds, table)
end


(* takes and prints original board parameters, calls go1 and determines if a new
 * board is in valid state, and if so calls go(new board), if not it
 * calls go (original board)
 * if deck is empty, switches with the reverse of the discarded pile
*)
fun go([], dis, founds, table) = go(List.rev(dis),[],founds,table)
  | go(deck:deck_t, dis:deck_t, founds:found_t, table:tab_t) =
    let
      val printedBoard= boardPrint(deck, dis, founds, table)
      val board' = go1(deck, dis, founds, table)
      val (result,winner,quit) = checkBoard(board')
      val printIllegalMove=
            if quit=true then print("Thank you for playing. \n")
            else if winner= true then print("Congratulations, you won!!! \n")
            else if result=true then print("")
            else print("ILLEGAL MOVE, please go again \n")
    in
      printedBoard;
      if quit=true then print("")
      else if winner = true then print("")
      else if result = true then go(board')
      else go(deck, dis, founds, table)
    end



(* initializes the board and starts the game
*)
fun play() =
let
  val deck = shuffle(makeDeck())
  val initboard = dealDeck(deck)
in
  go(initboard)
end




end







