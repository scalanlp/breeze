package scalanlp.optimize
/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


/**
 * Algorithm to find a minimum cost matching on a bipartite graph.
 *
 * Implements the hungarian algorithm.
 *
 * @author dlwh
 */
object KuhnMunkres extends BipartiteMatching {
  // http://github.com/bmc/munkres/blob/master/munkres.py
  /**
   * Given a matrix of positive weights, finds the minimum weight bipartite matching between to arrays.
   * Returns a matching from the rows (the first index into the matrix) to the columns
   * (-1 for unmatched rows in the case of unbalanced entries) along
   * with the total score of the matching.
   */
  def extractMatching(costs: Seq[Seq[Double]]) = {

    // swap rows and columns if num rows > num cols
    val (costs2: Seq[Seq[Double]], inverted:Boolean) =
      if (costs.length > costs(0).length) {
        val newCosts = Array.fill(costs(0).length, costs.length)(0.0)
        for(i <- 0 until costs.length;
            j <- 0 until costs(0).length) {
          newCosts(j)(i) = costs(i)(j)
        }
        (newCosts.map{ row => row.toSeq }.toSeq, true)
      } else {
        (costs, false)
      }

    val C : Array[Array[Double]] = padMatrix(costs2);
    val n = C.size;
    val rowCovered = Array.fill(n)(false);
    val colCovered = Array.fill(n)(false);

    var primeR = 0;
    var primeC = 0;
    val path = Array.fill(2 * n, 2 * n)(0);
    val marked = Array.fill(n,n)(0);

    def findSmallestNotCovered() = {
      val mins = for(i <- 0 until n iterator;
                     j <- 0 until n iterator;
                     if !rowCovered(i) && !colCovered(j)
                    ) yield C(i)(j);
      mins.reduceLeft(_ min _)
    }

    def findZero() = {
      var row = -1
      var col = -1
      var i = 0
      var done = false

      while(!done && i < n) {
        var j = 0
        while (j < n) {
          if (C(i)(j) == 0 && !rowCovered(i) && !colCovered(j)) {
            row = i
            col = j
            done = true;
          }
          j += 1
        }

        i += 1
      }

      (row, col)
    }

    def erasePrimes() {
      for(i <- 0 until n; j <- 0 until n if marked(i)(j) == 2) {
        marked(i)(j) = 0;
      }
    }

    def findStarInRow(row: Int) = {
      marked(row).indexWhere(1 == _)
    }

    def findStarInCol(col: Int) = {
      Iterator.range(0,n).indexWhere( i => marked(i)(col) == 1)
    }

    def findPrimeInRow(row: Int) = {
      marked(row).indexWhere(2 == _)
    }

    def convertPath(path: Array[Array[Int]], count: Int) = {
      for(i <- 0 to count) {
        if(marked(path(i)(0))(path(i)(1)) == 1)
          marked(path(i)(0))(path(i)(1)) = 0
        else {
          marked(path(i)(0))(path(i)(1)) = 1
        }
      }
    }


    def step1() = {
      for {
        i <- 0 until n;
        min = C(i).reduceLeft(_ min _);
        j <- 0 until n
      }  {
        C(i)(j) -= min;
      }

      2;
    }

    def step2() = {
      for {
        r <- 0 until n;
        c <- 0 until n
        if C(r)(c) == 0 && !rowCovered(r) && !colCovered(c)
      }  {
        marked(r)(c) = 1;
        rowCovered(r) = true;
        colCovered(c) = true;
      }
      java.util.Arrays.fill(rowCovered,false);
      java.util.Arrays.fill(colCovered,false);
      3;
    }

    def step3() = {
      var count = 0;
      for {
        i <- 0 until n;
        j <- 0 until n
        if marked(i)(j) == 1
      } {
        colCovered(j) = true
        count += 1
      }

      if(count >= n) {
        7
      } else {
        4
      }
    }

    def step4() = {
      var star_col = -1;
      var done = false;
      var step = 0;
      while(!done) {
        val (row,col) = findZero();
        if(row == -1) {
          done = true;
          step = 6;
        } else {
          marked(row)(col) = 2;
          val starredCol = findStarInRow(row);
          if(starredCol == -1) {
            done = true;
            primeR = row;
            primeC = col;
            step = 5;
          } else {
            rowCovered(row) = true;
            colCovered(starredCol) = false;
          }
        }
      }
      step
    }

    def step5() = {
      var count = 0;
      path(count)(0) = primeR;
      path(count)(1) = primeC;
      var done = false;
      while(!done) {
        val row = findStarInCol(path(count)(1));
        if(row >= 0) {
          count += 1;
          path(count)(0) = row;
          path(count)(1) = path(count-1)(1);
        } else {
          done = true;
        }

        if(!done) {
          val col = findPrimeInRow(path(count)(0));
          count += 1
          path(count)(0) = path(count-1)(0);
          path(count)(1) = col;
        }
      }

      convertPath(path, count);
      java.util.Arrays.fill(rowCovered,false);
      java.util.Arrays.fill(colCovered,false);
      erasePrimes();

      3
    }

    def step6() = {
      val min = findSmallestNotCovered();
      for {
        r <- 0 until n;
        c <- 0 until n
      } {
        if(rowCovered(r)) C(r)(c) += min;
        if(!colCovered(c)) C(r)(c) -= min;
      }

      4
    }

    var step = 1;
    while(step < 7) {
      step = step match {
        case 1 => step1();
        case 2 => step2();
        case 3 => step3();
        case 4 => step4();
        case 5 => step5();
        case 6 => step6();
      }
    }

    var answers = Array.fill(costs2.length)(-1);
    var cost = 0.0;
    for(i <- 0 until answers.length) {
      val j = marked(i).indexWhere(_ == 1);
      if(j >= 0) {
        cost += costs2(i)(j);
        answers(i) = j;
      }
    }

    // invert rows with columns to their original layout
    if (inverted) {
      val answers2 = Array.fill(costs2(0).length)(-1)
      for (i <- 0 until answers.length) {
        val j = answers(i)
        if (j != -1) {
          answers2(j) = i
        }
      }
      answers = answers2
    }

    (answers,cost)
  }

  private def padMatrix(costs: Seq[Seq[Double]]) = {
    val rows = costs.length;
    val cols = costs(0).length;
    val n = rows max cols;
    val ret = Array.tabulate(n,n){ (i,j) =>
      if(i >= rows) 0.0;
      else if(j >= costs(i).length) 0.0
      else costs(i)(j);
    }

    ret
  }
}
