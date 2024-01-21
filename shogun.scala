object M4 {   

type Pos = (Int, Int)    // a position on a chessboard 

// Colours: Red or White
abstract class Colour
case object Red extends Colour
case object Wht extends Colour

// Pieces: Either Pawns or Kings
abstract class Piece {
  def pos : Pos       
  def col : Colour    
  def en : Int      // energy for Pawns 1 - 4, for Kings 1 - 2
}
case class Pawn(en: Int, col: Colour, pos: Pos) extends Piece
case class King(en: Int, col: Colour, pos: Pos) extends Piece


// checks if a piece is a king
def is_king(pc: Piece) : Boolean = pc match {
  case King(_, _, _) => true
  case _ => false
}

// incrementing and decrementing the position of a piece
def incx(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x+1,y))
  case King(en, c, (x,y)) => King(en, c, (x+1,y))
}

def incy(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x,y+1))
  case King(en, c, (x,y)) => King(en, c, (x,y+1))
}

def decx(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x-1,y))
  case King(en, c, (x,y)) => King(en, c, (x-1,y))
}

def decy(pc: Piece) : Piece = pc match {
  case Pawn(en, c, (x,y)) => Pawn(en, c, (x,y-1))
  case King(en, c, (x,y)) => King(en, c, (x,y-1))
}

//pretty printing colours and pieces
def pp_color(c: Colour) : String = c match {
  case Red => "R"
  case Wht => "W"
}

def pp(pc: Piece) : String = pc match {
  case Pawn(n, c, _) => s"P${pp_color(c)}$n"
  case King(n, c, _) => s"K${pp_color(c)}$n"
}

// Boards are sets of pieces
case class Board(pces: Set[Piece]) {
  def +(pc: Piece) : Board = Board(pces + pc)
  def -(pc: Piece) : Board = Board(pces - pc)
}

// checking whether a position is occupied in a board
def occupied(p: Pos, b: Board) : Option[Piece] =  
  b.pces.find(p == _.pos)
  
def occupied_by(p: Pos, b: Board) : Option[Colour] =
  occupied(p, b).map(_.col)

def is_occupied(p: Pos, b: Board) : Boolean =
  occupied(p, b).isDefined

// is a position inside a board
def inside(p: Pos, b: Board): Boolean = 
  1 <= p._1 && 1 <= p._2 && p._1 <= 8 && p._2 <= 8 

// pretty printing a board
def print_board(b: Board): Unit = {
  println()
  for (i <- 8 to 1 by -1) {
    println("----" * 8)
    for (j <- 1 to 8) {
      val opc = occupied((j,i), b)
      if (opc.isDefined) print(s"|${pp(opc.get)}") 
      else print("|   ")
    }
    println("|")
  } 
  println("----" * 8)
}

// example board: initial board
val b_init = Board(Set(King(2,Wht,(4,1)), King(1,Red,(5,8)),
                  		 Pawn(4,Wht,(1,1)), Pawn(4,Red,(1,8)),
                  		 Pawn(3,Wht,(2,1)), Pawn(2,Red,(2,8)),
                  		 Pawn(2,Wht,(3,1)), Pawn(3,Red,(3,8)),
                  		 Pawn(1,Wht,(5,1)), Pawn(1,Red,(4,8)),
                  		 Pawn(4,Wht,(6,1)), Pawn(3,Red,(6,8)),
                  		 Pawn(3,Wht,(7,1)), Pawn(1,Red,(7,8)),
                  		 Pawn(2,Wht,(8,1)), Pawn(3,Red,(8,8))))



// Moves
abstract class Move
case object U extends Move    // up
case object D extends Move    // down
case object R extends Move    // right
case object L extends Move    // left
case object RU extends Move   // first right, then possibly up
case object LU extends Move   // first left, then possibly up
case object RD extends Move   // ...
case object LD extends Move
case object UR extends Move
case object UL extends Move
case object DR extends Move
case object DL extends Move


/* Takes a piece, a move, an energy and a board as arguments. 
Recursively calculate all fields that can be reached by the move. 
The energy acts as a counter and decreases in each recursive
call until 0 is reached (the final field).  */
def eval(pc: Piece, m: Move, en: Int, b: Board) : Set[Piece] = {
  pc.pos match{
    case ps if !inside(ps, b)=>Set()
    case ps if en == 0 && !is_occupied(ps, b)=>Set(pc)
    case ps if en == 0 && is_occupied(ps, b)=>
      if (occupied_by(ps, b).get != pc.col){
        Set(pc)
      }
      else{
        Set()
      }
    case ps if en > 0 && is_occupied(ps, b)=>Set()
    case _ =>
      m match{
        case U=>eval(incy(pc), m, en - 1, b)
        case D=>eval(decy(pc), m, en - 1, b)
        case L=>eval(decx(pc), m, en - 1, b)
        case R=>eval(incx(pc), m, en - 1, b)
        case RU=>
          eval(incx(pc), m, en - 1, b).union(eval(pc, U, en, b))
        case RD=>
          eval(incx(pc), m, en - 1, b).union(eval(pc, D, en, b))
        case LU=>
          eval(decx(pc), m, en - 1, b).union(eval(pc, U, en, b))
        case LD=>
          eval(decx(pc), m, en - 1, b).union(eval(pc, D, en, b))
        case UR=>
          eval(incy(pc), m, en - 1, b).union(eval(pc, R, en, b))
        case UL=>
          eval(incy(pc), m, en - 1, b).union(eval(pc, L, en, b))
        case DL=>
          eval(decy(pc), m, en - 1, b).union(eval(pc, L, en, b))
        case DR=>
          eval(decy(pc), m, en - 1, b).union(eval(pc, R, en, b))
      }
  }
}


/* Finds all onwards positions for a piece on a board */
def all_moves(pc: Piece, b: Board) : Set[Piece] = {
  val moves = List(U, D, L, R, LU, LD, RU, RD, UL, UR, DL, DR)
  moves.flatMap(move => eval(pc, move, pc.en, b-pc)).toSet
}


/* Takes a colour and a board and calculates all pieces of the 
opposite side that are attacked*/
def attacked(c: Colour, b: Board) : Set[Piece] = {
  val (coloured, opposites) = b.pces.partition(_.col == c)
  val pieces_of_colour = coloured.flatMap(piece => all_moves(piece, b)).map(_.pos).toSet
  opposites.filter(piece => pieces_of_colour.contains(piece.pos))

}



/* Takes a piece and a board and calculates the number of times
this pieces is attacked by pieces of the opposite colour. */
def attackedN(pc: Piece, b: Board) : Int = {
  b.pces.filter(_.col != pc.col).toList.flatMap(piece => all_moves(piece, b)).filter(_.pos == pc.pos).size
}


/* Takes a piece and a board and calculates the number of times this
piece is protected by pieces of the same colour. */
def protectedN(pc: Piece, b: Board) : Int = {
  (b.pces-pc).filter(_.col == pc.col).toList.flatMap(piece => all_moves(piece, b-pc)).filter(_.pos == pc.pos).size
}


/* Like all_moves function but also applies kings movement rules (cant
move to a field that is being attacked */ 
def legal_moves(pc: Piece, b: Board) : Set[Piece] = {
  pc match{
    case obj: Pawn => all_moves(pc, b)
    case obj: King => all_moves(pc, b).map(piece => (piece, make_new_board(piece, b))).filter(tup => attackedN(tup._1, tup._2) == 0).map(_._1)
  }
}

/* Creates imaginary board to help legal moves function */
def make_new_board(pc: Piece, b: Board) : Board = {
  Board(b.pces.filter(_.pos != pc.pos))
}


/*

val b_init = Board(Set(King(2,Wht,(4,1)), King(1,Red,(5,8)),
                  		 Pawn(4,Wht,(1,1)), Pawn(4,Red,(1,8)),
                  		 Pawn(3,Wht,(2,1)), Pawn(2,Red,(2,8)),
                  		 Pawn(2,Wht,(3,1)), Pawn(3,Red,(3,8)),
                  		 Pawn(1,Wht,(5,1)), Pawn(1,Red,(4,8)),
                  		 Pawn(4,Wht,(6,1)), Pawn(3,Red,(6,8)),
                  		 Pawn(3,Wht,(7,1)), Pawn(1,Red,(7,8)),
                  		 Pawn(2,Wht,(8,1)), Pawn(3,Red,(8,8))))
val pw1 = Pawn(4, Wht, (4,6))
legal_moves(pw1, b_init)



