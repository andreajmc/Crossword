package main.scala.crossword

object CrosswordSolver extends App {

  println("Crossword Solver using simple backtracking algorithm, performance need to be improved")

  // splits the given character array to sub arrays wrt spaces
  def split(s: List[Char]): List[List[Char]] = {
    splitHelper(s, List(), List())
  }
  
  def splitHelper(s: List[Char], sagg: List[Char], agg: List[List[Char]]): List[List[Char]] = s match {
    case x :: xy => sagg match {
      case y :: yz => if (x == ' ') splitHelper(xy, List(), agg ::: List(sagg))
      else splitHelper(xy, sagg ::: List(x), agg)
      case List() => if (x == ' ') splitHelper(xy, List(), agg) else splitHelper(xy, List(x), agg)
    }
    case List() => agg ::: List(sagg)
  }
  
  // splits the given array to equal width sub arrays
  def splitByWidth(w: Int)(s: List[Char]): List[List[Char]] = {
    splitByWidthH(w, s, List(), List())
  }

  def splitByWidthH(w: Int, s: List[Char], sagg: List[Char], agg: List[List[Char]]): List[List[Char]] = s match {
    case x :: xy => if (sagg.length == w) splitByWidthH(w, s, List(), agg ::: List(sagg))
    else splitByWidthH(w, xy, sagg ::: List(x), agg)
    case List() => agg ::: List(sagg)

  }
  
  // rotates a given 2D array to 90 degrees
  def rotate2DArray90(l: List[List[Char]]): List[List[Char]] = {
    rotate2DArray90H(l, List())
  }

  def rotate2DArray90H(l: List[List[Char]], helper: List[List[Char]]): List[List[Char]] = l match {
    case x :: xy => rotate2DArray90H(xy, appendParallel(x, helper))
    case List() => helper
  }
  
  // 90 * 3 = 270
  def rotate2DArray270(array: List[List[Char]]): List[List[Char]] = {
    rotate2DArray90(rotate2DArray90(rotate2DArray90(array)))
  }  
  
  // appends an array in parallel to the given 2D array
  def appendParallel(x: List[Char], helper: List[List[Char]]): List[List[Char]] = x match {
    case y :: yz => helper match {
      case h :: hi => (h ::: List(y)) :: appendParallel(yz, hi)
      case List() => List(y) :: appendParallel(yz, List())
    }
    case List() => helper
  }  

  // Generates a list of identifiable sites which can hold words 
  def genWordSites(sites: List[List[Char]], rowNo: Int): List[(Int, Int, List[Char])] = sites match {
    case x :: xy => genWordsInRow(x, rowNo, 0, List(), List()) ::: genWordSites(xy, rowNo + 1)
    case List() => List()
  }
  
  // Generates word sites for each row
  def genWordsInRow(row: List[Char], rowNo: Int, colNo: Int,
    prevWord: List[Char], helper: List[(Int, Int, List[Char])]): List[(Int, Int, List[Char])] = row match {

    case x :: Nil => x match {
      case ' ' => prevWord match {
        case p :: Nil => helper
        case p :: pq => helper ::: List((rowNo, colNo - prevWord.length, prevWord))
        case List() => helper
      }
      case _ => prevWord match {
        case p :: pq => helper ::: List((rowNo, colNo - prevWord.length, prevWord ::: List(x)))
        case List() => helper
      }
    }
    case x :: xy => x match {
      case ' ' => prevWord match {
        case p :: Nil => genWordsInRow(xy, rowNo, colNo + 1, List(), helper)
        case p :: pq => genWordsInRow(xy, rowNo, colNo + 1, List(), helper ::: 
            List((rowNo, colNo - prevWord.length, prevWord)))
        case List() => genWordsInRow(xy, rowNo, colNo + 1, List(), helper)
      }
      case _ => genWordsInRow(xy, rowNo, colNo + 1, prevWord ::: List(x), helper)

    }
    case List() => helper
  }

  // puts the word on the specified site and returns the new sites 2d list
  def putWordOnSite(currentRowNo: Int, word: List[Char], site: (Int, Int, List[Char]), 
      sites: List[List[Char]]): List[List[Char]] = sites match {
    case x :: xy => if (currentRowNo != site._1) x :: putWordOnSite(currentRowNo + 1, word, site, xy)
    else putWordInRow(x, site._2, word, 0) :: xy
    case List() => sites
  }

  // puts the word in the given row of the 2d list
  def putWordInRow(row: List[Char], col: Int, word: List[Char], index: Int): List[Char] = row match {
    case x :: xy =>
      word match {
        case y :: yz => if (index == col) y :: putWordInRow(xy, col + 1, yz, index + 1)
        else x :: putWordInRow(xy, col, word, index + 1)
        case List() => x :: putWordInRow(xy, col, word, index + 1)
      }
    case List() => List()

  }
 
  // compares word and site of equal length
  def compareWordnSite(word: List[Char], site: List[Char]): Boolean = {
    if (site.isEmpty) true
    else if (site.head != '*' && site.head != word.head) false
    else compareWordnSite(word.tail, site.tail)
  }

  // generate a list of probable words that could sit on a site
  def genProbables(wordsList: List[List[Char]], site: List[Char], filled: List[List[Char]]): List[List[Char]] = {
    ((wordsList filter (x => if (x.length == site.length) true else false))
      filter (y => compareWordnSite(y, site))) filterNot (z => filled contains z)
  }

  // check if we got a solution for the crossword
  def checkCrosswordComplete(tempSites: List[Char]): Boolean = {
    if ((tempSites contains '*') || tempSites.length == 0) false
    else true
  }  
  
  // the main recursive function which solves
  def solveCrossword(tempSites: List[Char], filled: List[List[Char]]): List[Char] = {
    if (checkCrosswordComplete(tempSites))
      return tempSites

    val horiSites = splitByWidth(width)(tempSites)
    val wordSites = (genWordSites(horiSites, 0) map (x => (true, x))) :::
      (genWordSites(rotate2DArray90(horiSites), 0) map (x => (false, x)))

    for (wordSite <- wordSites) {
      val probables = genProbables(wordsList, wordSite._2._3, filled)
      for (probable <- probables) {
        if (wordSite._1) {
          val pSolution = solveCrossword(putWordOnSite(0, probable, 
              wordSite._2, horiSites).flatten, probable :: filled)
          if (checkCrosswordComplete(pSolution))
            return pSolution

        } else {
          val pSolution = solveCrossword(rotate2DArray270(putWordOnSite(0, probable, wordSite._2,
            rotate2DArray90(horiSites))).flatten, probable :: filled)
          if (checkCrosswordComplete(pSolution))
            return pSolution
        }
      }
    }

    List()
  }
  
  def words = "LINUX  PROLOG PERL ONLINE GNU XML NFS SQL EMACS WEB MAC"
  def sites = "......  .. .  .  .. ..... .. . . ...  . ... . ...     "
  def width = 9  
  def wordsList = split(words.toList)
  
  val solution = solveCrossword(sites.toList map (x => if (x == '.') '*' else x), List())
  println(sites)
  println(solution)

}