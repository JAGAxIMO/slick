package scala.slick.compiler

import scala.slick.ast._
import scala.collection.mutable.HashMap
import Util._

/** Infer the structural types for all nominal table types which have their
  * structural view set to NoType. */
class InferTableTypes extends Phase {
  val name = "inferTableTypes"

  def apply(state: CompilerState) = state.map { tree =>
    val structs = new HashMap[TypeSymbol, HashMap[Symbol, Type]]
    def scanType(t: Type): Unit = t match {
      case n @ NominalType(sym) if n.structuralView == NoType =>
        structs += sym -> new HashMap
      case CollectionType(_, el) => scanType(el)
      case _ =>
        // We don't have to check other types. If we see a nominal table type
        // anywhere, there must be a place where it is introduced as a Table of
        // type CollectionType(_, NominalType(_)).
    }
    tree.foreach(n => scanType(n.nodeType))
    logger.debug("NominalTypes without structural view: "+structs.keySet.mkString(", "))
    tree//--
  }
}
