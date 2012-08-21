package breeze.inference.bp

import breeze.util.Index

/**
 * A Model is a factor graph with all variables and all factors.
 *
 * @param variables
 * @param factors
 */
case class Model(variables: IndexedSeq[Variable[_]], factors: IndexedSeq[Factor]) {
  def +(f: Factor) = copy(factors = factors :+ f)
  def +(v: Variable[_]) = copy(variables = variables :+ v)

  lazy val variableIndex = Index(variables)
  lazy val factorIndex = Index(factors)

  // factor Index -> list of variable indices
  lazy val factorVariablesByIndices: Array[Array[Int]] = {
    factors.toArray[Factor].map{ f =>
      f.variables.toArray.map(variableIndex)
    }
  }
}

object Model {
  def empty = Model(IndexedSeq.empty, IndexedSeq.empty)
}