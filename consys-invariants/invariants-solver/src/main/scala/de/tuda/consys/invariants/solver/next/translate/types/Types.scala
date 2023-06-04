package de.tuda.consys.invariants.solver.next.translate.types

import de.tuda.consys.invariants.solver.next.ir._
import de.tuda.consys.invariants.solver.next.translate.types.TypeChecker.{TypeEnv, TypeException}

object Types {

  def resolveType(typ : Type, typeVars : TypeEnv) : Type = typ match {
    case TypeVar(x, _) => typeVars.getOrElse(x, throw TypeException(s"type variable not declared: " + x))
    case CompoundType(ClassType(classId, typeArgs), c, m) =>
      CompoundType(ClassType(classId, typeArgs.map(arg => resolveType(arg, typeVars))), c, m)
  }

}
