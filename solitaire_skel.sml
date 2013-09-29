(*  COMP 212 Midterm:  Solitaire (Klondike) game.
*   N. Danner
*)

structure Solitaire =
struct

  (*  You need not use any of the definitions I have provided for you
  *   in this code skeleton, except that you must define the play
  *   function.  But I've given you some sample types and hopefully-helpful
  *   functions to give you a start on some of the tasks you'll need to
  *   complete.
  *)

  (*  The result of a single round of play.
  *)
  datatype go_t = KeepGoing

  (*  The type of columns, foundations, and tableaus.  A tableau
  *   consists of seven columns; it is a lot simpler to make it
  *   a list than a 7-tuple.
  *
  *   A column consists of the up-cards and the down cards.
  *   A column <c_0,...,c_{k-1} | c_k,...,c_{n-1}>, where c_0,...,c_{k-1}
  *   are the up-cards and c_k,...,c_{n-1} are the down-cards, is represented
  *   by ([c_0,...,c_{k-1}], [c_k,...,c_{n-1}]).
  *
  *   A foundation is four lists of cards.
  *)
  type column_t = (card_t list) * (card_t list)
  type found_t = (card_t list) * (card_t list) * (card_t list) * (card_t list)
  type tab_t = column_t list

  (*  A deck is just a list of cards.
  *)
  type deck_t = card_t list

  (*  shuffle xs = ys, where ys is a random permutation of xs.
  *)
  local
    (*  seed is a value that says where to "start" the random number
    *   generator that is used to shuffle the list.
    *)

    (*  Use this value of seed if you want the same shuffling every
    *   time you run your program.  This can be helpful for debugging,
    *   not so interesting for game play.

    val seed = Random.rand(0, 0)
    *)

    (*  Use this value of seed if you want a different shuffling (almost)
    *   every time you run your program.
    *)
    val seed =
    let
      val tNow = Time.toMicroseconds(Time.now())
      val (_, p) = IntInf.divMod(tNow, IntInf.fromInt (valOf(Int.maxInt)))
    in
      Random.rand(0, IntInf.toInt p)
    end

    (*  getNth([x_0,...,x_{n-1}], i) = 
    *     (x_i, [x_0,...,x_{i-1},x_{i+1},...,x_{n-1}]).
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
    fun shuffle xs = shuffle' (xs, length xs, [])
  end

  (*  getUserInput(msg) = s, where:
  *     - msg is printed to the terminal with no newline;
  *     - The user enters s' at the terminal.
  *     - s is obtained from s' by removing all leading whitespace
  *       and all characters following the first trailing whitespace
  *       character.
  *   Leading whitespace is defined to be whitespace that occurs before
  *   any non-whitespace character.  Trailing whitespace is defined
  *   to be whitespace that occurs after any non-whitespace character.
  *   Pre-condition:  s' must have at least one non-whitespace character.
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

  (*  Display the foundations.  This just shows the top card of
  *   each foundation, since that gives all the relevant information.
  *)
  fun showFounds(hearts, dmnds, clubs, spds) =
  let
    val line = String.concatWith " " [topCardToString hearts,
                               topCardToString dmnds,
                               topCardToString clubs,
                               topCardToString spds]
  in
    print (line ^ "\n")
  end

  (*  Here is an example of implementing loop-like behavior using a
  *   recursive function.  When getResponse is called, it prompts
  *   the user to enter a single character at the keyboard.  In
  *   most cases, the function does the appropriate thing; e.g.,
  *   if the user types "F", then it calls other functions that
  *   prompt the user for card to move to the foundation and
  *   then moves the card appropriately.  But if the user doesn't
  *   type one of the legal responses, we call getResponse() again.
  *   In other words, this is like the following while loop:
  *     while (true) {
  *       resp = getUserInput ...
  *       if resp = "D" then ... return
  *       else if resp = "M" then ... return
  *       else if resp = "F" then ... return
  *       else if resp = "P" then ... return
  *       else if resp = "Q" then ... return
  *     }
  *)
  fun getResponse() =
  let
    val resp = getUserInput
      "(D)raw card, (M)ove card, Move to (F)oundation, (P)lay discard, (Q)uit? "
  in
    case resp of
         "D" => ...
       | "M" => ...
       | "F" => ...
       | "P" => ...
       | "Q" => ...
       | _ => getResponse()
  end

  (*  Here's another example of implementing loop-like behavior using
  *   a recursive function (the function go).  In this case, the "body"
  *   of the loop is encapsulated in a function go1.  go1 returns a
  *   value which is then analyzed to determine whether to loop again
  *   or to terminate.
  *)
  fun go(deck : deck_t, discard : deck_t, founds : found_t, tableau : tab_t) =
  let
    val (deck', discard', founds', tableau', result) = 
      go1(deck, discard, founds, tableau)
  in
    case result of
         KeepGoing => go(deck', discard', founds', tableau')
  end

  (*  You must define this function.  I will play your game as follows:
  *   
  *   $ sml solitaire.sml
  *   - Solitaire.play()
  *)
  fun play() = 
  let
    val () = print "Fill in this function!"
  in
    ()
  end

end
