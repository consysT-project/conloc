package de.tuda.consys.invariants.solver.next.translate.types

import de.tuda.consys.invariants.solver.next.ir.IR._
import de.tuda.consys.invariants.solver.next.ir.Types._
import de.tuda.consys.invariants.solver.next.ir.IR
import de.tuda.consys.invariants.solver.next.ir._
import de.tuda.consys.invariants.solver.next.ir.{IR, Natives}
import de.tuda.consys.invariants.solver.next.translate.types.Types.resolveType

object TypeChecker {

  case class TypeException(s : String) extends Exception(s)

  trait M {
    def union(other : M) : M
  }

  private case object ImmutableContext extends M {
    override def union(other : M) : M = other
  }

  private case object MutableContext extends M {
    override def union(other : M) : M = MutableContext
  }

  type VarEnv = Map[VarId, Type]
  type TypeEnv = Map[TypeVarId, Type]

  def checkProgram(programDecl: ProgramDecl): Unit = {
    programDecl.classes.foreach(c => checkClass(c)(programDecl.classTable))
    typeOfExpr(programDecl.body, Map.empty)(null, MutableContext, programDecl.classTable)
  }

  def checkClass(classDecl : ClassDecl[_])(implicit classTable : ClassTable) : Unit = classDecl match {
    case ObjectClassDecl(name, typeParameters, invariant, fields, methods) =>

      val invariantType = TypeChecker.typeOfExpr(invariant, Map())(classDecl.asType, ImmutableContext, classTable).effectiveType()
      if (invariantType.baseType != Natives.BOOL_TYPE)
        throw TypeException(s"invariant is not Bool, but: " + invariantType)

      for ((methodId, methodDecl) <- methods) {
        val varEnv : VarEnv = methodDecl.declaredParameters.map(varDecl => (varDecl.name, varDecl.typ)).toMap
        methodDecl match {
          case q : QueryMethodDecl =>
            val returnTyp = TypeChecker.typeOfExpr(methodDecl.body, varEnv)(classDecl.asType, ImmutableContext, classTable)
            if (!(returnTyp <= q.returnTyp))
              throw TypeException(s"return type is wrong. Expected: ${q.returnTyp}, but was $returnTyp (in method $methodId)")
          case _ : UpdateMethodDecl =>
            val returnTyp = TypeChecker.typeOfExpr(methodDecl.body, varEnv)(classDecl.asType, MutableContext, classTable).effectiveType()
            if (returnTyp.baseType != Natives.UNIT_TYPE)
              throw TypeException(s"return type is wrong. Expected: ${Natives.UNIT_TYPE}, but was $returnTyp (in method $methodId)")
        }
      }

    case NativeClassDecl(name, typeVars, sort, methods) => // Native classes are expected to be fine
  }


  def typeOfExpr(expr: IRExpr, vars: VarEnv)(implicit thisType: ClassType, mutableContext: M, classTable: ClassTable): Type = expr match {
    case Num(_) => CompoundType(Natives.INT_TYPE, Local, Bottom)
    case True => CompoundType(Natives.BOOL_TYPE, Local, Bottom)
    case False => CompoundType(Natives.BOOL_TYPE, Local, Bottom)
    case Str(_) => CompoundType(Natives.STRING_TYPE, Local, Bottom)
    case UnitLiteral => CompoundType(Natives.UNIT_TYPE, Local, Bottom)

    case Var(id: VarId) => vars.getOrElse(id, throw TypeException("variable not declared: " + id))

    case Let(id: VarId, namedExpr: IRExpr, body: IRExpr) =>
      val namedType = typeOfExpr(namedExpr, vars)
      typeOfExpr(body, vars + (id -> namedType))

    case If(conditionExpr, thenExpr, elseExpr) =>
      val condType = typeOfExpr(conditionExpr, vars).effectiveType()

      if (condType.baseType != Natives.BOOL_TYPE)
        throw TypeException("condition must be Bool, but was: " + condType)

      // In the branches of the if, state changes are not allowed as we do not know which changes to apply
      val t1 = typeOfExpr(thenExpr, vars)(thisType, ImmutableContext, classTable)
      val t2 = typeOfExpr(elseExpr, vars)(thisType, ImmutableContext, classTable)

      if (t1 != t2)
        throw TypeException("branches have diverging types: " + t1 + " and " + t2)

      t1


    case Equals(e1 : IRExpr, e2 : IRExpr) =>
      val t1 = typeOfExpr(e1, vars)
      val t2 = typeOfExpr(e2, vars)

      if (t1 != t2) throw TypeException(s"non-matching types in 'equals': $t1 and $t2")

      val CompoundType(_, c, _) = t1.effectiveType()
      CompoundType(Natives.BOOL_TYPE, c, Bottom)

    case This =>
      CompoundType(thisType, Local, Mutable)

    case GetField(fieldId : FieldId) =>
      val classDecl = classTable
        .getOrElse(thisType.classId, throw TypeException("class of 'this' not available: " + thisType))

      val fieldDecl = classDecl
        .getField(fieldId).getOrElse(throw TypeException("field not available: " + fieldId + s" (in class $thisType)"))

      fieldDecl.typ


    case SetField(fieldId : FieldId, value : IRExpr) =>
      if (mutableContext != MutableContext) throw TypeException("assignment in immutable context: " + thisType)

      val valueType = typeOfExpr(value, vars)
      val cls = classTable.getOrElse(thisType.classId, throw TypeException("class of 'this' not available: " + thisType))
      val fieldDecl = cls.getField(fieldId).getOrElse(throw TypeException("field not available: " + fieldId + s" (in class $thisType)"))
      if (valueType != fieldDecl.typ)
        throw TypeException(s"assignment has wrong type. expected: ${fieldDecl.typ} (but was: ${valueType})")

      valueType


    case CallQuery(recv, methodId, arguments) =>
      val recvType = typeOfExpr(recv, vars)

      recvType match {
        case CompoundType(recvClassType@ClassType(classId, typeArguments), _, _) =>

          val (mthdDecl, typeEnv) = checkMethodCall(recvClassType, methodId, vars, arguments)

          val queryDecl = mthdDecl match {
            case q : QueryMethodDecl => q
            case _ => throw TypeException(s"expected query method: $methodId")
          }

          resolveType(queryDecl.returnTyp, typeEnv)

        case _ => throw TypeException(s"receiver not a class type: " + recv)
      }

    case CallUpdate(recv, methodId, arguments) =>
      val recvType = typeOfExpr(recv, vars)

      recvType match {
        case CompoundType(recvClassType@ClassType(classId, typeArguments), _, _) =>

          val (mthdDecl, typeEnv) = checkMethodCall(recvClassType, methodId, vars, arguments)

          val updateDecl = mthdDecl match {
            case u: UpdateMethodDecl => u
            case _ => throw TypeException(s"expected update method: $methodId")
          }

          CompoundType(Natives.UNIT_TYPE, Local, Bottom)

        case _ => throw TypeException(s"receiver not a class type: " + recv)
      }

    case CallUpdateThis(methodId, arguments) =>
      if (mutableContext != MutableContext)
        throw TypeException(s"cannot call update on immutable type: $methodId")

      val (mthdDecl, _) = checkMethodCall(thisType, methodId, vars, arguments)

      val updateDecl = mthdDecl match {
        case u : UpdateMethodDecl => u
        case _ => throw TypeException(s"expected update method: $methodId")
      }

      CompoundType(Natives.UNIT_TYPE, Local, Bottom)


    case CallUpdateField(fieldId, methodId, arguments) =>
      if (mutableContext != MutableContext)
        throw TypeException(s"cannot call update on immutable type: $methodId")

      val thisClass = classTable.getOrElse(thisType.classId, throw TypeException("class not available: " + thisType))
      val fieldDecl = thisClass.getField(fieldId).getOrElse(throw TypeException(s"field not available: $fieldId (in class ${thisClass.classId}) )"))

      fieldDecl.typ match {
        case CompoundType(fieldClassType@ClassType(classId, typeArguments), _, _) =>
          val (mthdDecl, _) = checkMethodCall(fieldClassType, methodId, vars, arguments)

          val updateDecl = mthdDecl match {
            case u : UpdateMethodDecl => u
            case _ => throw TypeException(s"expected update method: $methodId")
          }

          CompoundType(Natives.UNIT_TYPE, Local, Bottom)

        case _ => throw TypeException(s"expected class type, but got: " + fieldDecl.typ)
      }

    case Sequence(exprs) =>
      exprs.foldLeft(null: Type)((r, e) => typeOfExpr(e, vars))

    case New(classId, typeArgs) =>
      val classDecl = classTable.getOrElse(classId, throw TypeException("class not available: " + classId))
      if (typeArgs.length != classDecl.typeParameters.length) throw TypeException(s"wrong number of type arguments: " + classId)
      (typeArgs zip classDecl.typeParameters).foreach(e => {
        val (arg, param) = e
        if (!(arg.effectiveType() <= param.effectiveType())) throw TypeException(s"wrong type argument for type variable: $arg (expected: $param)")
      })

      CompoundType(ClassType(classId, typeArgs), Local, Bottom)

    case _ => ???
  }

  private def checkMethodCall(recvType : ClassType, methodId : MethodId, vars : VarEnv, arguments : Seq[IRExpr])
                             (implicit thisType : ClassType, mutableContext : M, classTable : ClassTable) : (MethodDecl, TypeEnv) = {

    val recvClassDecl = classTable.getOrElse(recvType.classId, throw TypeException("class not available: " + thisType))

    if (recvClassDecl.typeParameters.length != recvType.typeArguments.length)
      throw TypeException(s"wrong number of type arguments: " + recvType)

    val typeEnv : TypeEnv =
      recvClassDecl.typeParameters.zip(recvType.typeArguments).map(p => (p._1.typeVarId, p._2)).toMap

    val methodDecl : MethodDecl = recvClassDecl
      .getMethod(methodId).getOrElse(throw TypeException("method not available: " + methodId + s" (in class $thisType)"))

    if (arguments.size != methodDecl.declaredParameters.size)
      throw TypeException(s"wrong number of arguments for method $methodId: ${arguments.size} (expected: ${methodDecl.declaredParameters.size}")

    val argTypes = arguments.map(argExpr => typeOfExpr(argExpr, vars))

    argTypes.zip(methodDecl.declaredParameterTypes).foreach(t => {
      val argType = t._1
      val parameterType = resolveType(t._2, typeEnv)
      if (!(argType <= parameterType))
        throw TypeException(s"wrong argument type for method $methodId: $argType (expected: $parameterType)")
    })

    (methodDecl, typeEnv)
  }
}
