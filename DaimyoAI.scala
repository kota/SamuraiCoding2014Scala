import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object DaimyoAI extends App {
  val seedList = List(0,1,2,3,4,5)
  def combinations(size: Int = 5) : List[List[Int]] = {
    if(size == 0)  {
      List(List())
    } else {
      for {
        x <- seedList.toList
        xs <- combinations(size-1)
      } yield x :: xs
    }
  }

  def generateLegalMoves:Array[Array[Int]] = {
    val moveList = combinations().toArray
    val moves = new Array[Array[Int]](moveList.length)
    for(i <- 0 to moveList.length-1){
      moves(i) = moveList(i).sorted.toArray
    }
    val uniqueMoves = ArrayBuffer.empty[Array[Int]]
    for(move <- moves) {
      if(!isDup(uniqueMoves.toArray,move)){
        uniqueMoves += move
      }
    }
    return uniqueMoves.toArray
  }

  def isDup(moves:Array[Array[Int]],move:Array[Int]):Boolean = {
    for(uniqueMove <- moves) {
      if(uniqueMove.sameElements(move)){
        return true
      }
    }
    return false
  }

  var legalMoves = generateLegalMoves

  val winCount = new Array[Int](legalMoves.size)
  for(i <- 0 to legalMoves.size-1) {
    for(j <- 0 to 1000){
      val game = new Game(1,Array(0,0,0,0),Array(Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0)))
      if(game.playout(legalMoves(i))){
        winCount(i) += 1
      }
    }
  }

  println(winCount.deep)
  val bestMove = legalMoves(winCount.indexOf(winCount.max))
  println("best move is " + bestMove.deep)
}

class Lord(strength:Int,real:Array[Int]) {
  var militaryStrength = strength
  var realIntimacy:Array[Int] = real
}

class Game(val initTurn:Int,val initDaimyos:Array[Double],val initIntimacies:Array[Array[Int]]) {
  var daimyos:Array[Double] = null
  var lords = new Array[Lord](6)
  var turn = initTurn
  val r = new Random
  var isMyFirstMove = false
  var myFirstMove:Array[Int] = null

  def playout(move:Array[Int]):Boolean = {
    myFirstMove = move
    init_game()
    while(turn <= 9) {
      processTurn
      turn += 1
    }
    isWin()
  }

  def init_game() {
    isMyFirstMove = true
    daimyos = initDaimyos.clone
    for (i <- 0 to 5) {
      lords(i) = new Lord(6,initIntimacies(i).clone)
    }
    turn = initTurn
  }

  def processTurn() {
    for (d <- 0 to 3) {
      for (t <- 0 to 4) {
        if(d == 0 && isMyFirstMove){
          for(l <- myFirstMove){
            lords(l).realIntimacy(d) += 1
          }
          isMyFirstMove = false
        } else {
          lords(r.nextInt(6)).realIntimacy(d) += 1
        }
      }
    }

    if(turn == 5 || turn == 9) {
      calcluateTotalMilitaryStrength()
    }
  }

  def isWin():Boolean = {
    for(d <- 1 to 3) {
      if(daimyos(d) >= daimyos(0)){
        return false
      }
    }
    return true
  }

  def calcluateTotalMilitaryStrength() {
    for(l <- 0 to 5) {
      val maxIntimacy = lords(l).realIntimacy.max
      val bestDaimyos = ArrayBuffer.empty[Int]
      for(d <- 0 to 3) {
        if(lords(l).realIntimacy(d) == maxIntimacy) {
          bestDaimyos += d
        }
      }
      for(d <- bestDaimyos) {
        daimyos(d) += lords(l).militaryStrength / bestDaimyos.length.toDouble
      }

      val worstIntimacy = lords(l).realIntimacy.min
      val worstDaimyos = ArrayBuffer.empty[Int]
      for(d <- 0 to 3) {
        if(lords(l).realIntimacy(d) == worstIntimacy) {
          worstDaimyos += d
        }
      }
      for(d <- worstDaimyos) {
        daimyos(d) -= lords(l).militaryStrength / worstDaimyos.length.toDouble
      }
    }
  }
}

