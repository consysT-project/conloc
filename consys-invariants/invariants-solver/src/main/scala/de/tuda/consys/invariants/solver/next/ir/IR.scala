package de.tuda.consys.invariants.solver.next.ir


import com.microsoft.z3.{Context, Expr, Sort}
import de.tuda.consys.invariants.solver.next.ir.Types.{ClassId, TypeVarId}
import de.tuda.consys.invariants.solver.next.translate.types.FieldInference

import scala.collection.mutable


object IR {

	type ClassTable = Map[(ClassId, ConsistencyType), ClassDecl[_ <: MethodDecl]]

	type FieldId = String
	type MethodId = String
	type VarId = String

	case class FieldDecl(name : FieldId, typ : Type)
	case class VarDecl(name : VarId, typ : Type)

	trait MethodDecl {
		def name : MethodId
		def declaredParameters : Seq[VarDecl]

		def declaredParameterTypes : Seq[Type] =
			declaredParameters.map(varDecl => varDecl.typ)

		def operationLevel: OperationLevel
	}

	trait ObjectMethodDecl extends MethodDecl {
		def body : IRExpr
	}

	trait NativeMethodDecl extends MethodDecl {
		def impl : (Context, Expr[_ <: Sort], Seq[Expr[_ <: Sort]]) => Expr[_ <: Sort]
	}

	trait QueryMethodDecl extends MethodDecl {
		def returnTyp : Type
	}

	trait UpdateMethodDecl extends MethodDecl {

	}

	case class ObjectQueryMethodDecl(
		override val name : MethodId,
		override val operationLevel: OperationLevel,
		override val declaredParameters : Seq[VarDecl],
		override val returnTyp : Type,
		override val body : IRExpr
	) extends ObjectMethodDecl with QueryMethodDecl

	case class ObjectUpdateMethodDecl(
		override val name : MethodId,
		override val operationLevel: OperationLevel,
		override val declaredParameters : Seq[VarDecl],
		override val body : IRExpr
	) extends ObjectMethodDecl with UpdateMethodDecl

	case class NativeQueryMethodDecl(
		override val name : MethodId,
		override val operationLevel: OperationLevel,
		override val declaredParameters : Seq[VarDecl],
		override val returnTyp : Type,
		override val impl : (Context, Expr[_ <: Sort], Seq[Expr[_ <: Sort]]) => Expr[_ <: Sort]
	) extends NativeMethodDecl with QueryMethodDecl

	trait ClassDecl[MDecl <: MethodDecl] {
		def classId : ClassId
		def typeParameters : Seq[TypeVar]
		def methods : Map[MethodId, MDecl]

		def getField(fieldId : FieldId) : Option[FieldDecl]
		def getMethod(methodId : MethodId) : Option[MDecl]

		lazy val asType : ClassType =
			ClassType(classId, typeParameters)

		def toType(typeArgs : Seq[Type]) : ClassType = {
			require(typeArgs.length == typeParameters.length)
			ClassType(classId, typeArgs)
		}

		def typeParametersMapTo[A](others : Seq[A]) : Map[TypeVarId, A] =
			typeParameters.map(typeVar => typeVar.typeVarId).zip(others).toMap
	}

	case class ObjectClassDecl(
		override val classId : ClassId,
		override val typeParameters : Seq[TypeVar],
		invariant : IRExpr,
		fields : Map[FieldId, FieldDecl],
		override val methods : Map[MethodId, ObjectMethodDecl]
	) extends ClassDecl[ObjectMethodDecl] {

		override def getField(fieldId : FieldId) : Option[FieldDecl] =
			fields.get(fieldId)
		override def getMethod(methodId : MethodId) : Option[ObjectMethodDecl] =
			methods.get(methodId)
	}

	case class NativeClassDecl(
		override val classId : ClassId,
		override val typeParameters : Seq[TypeVar],
		sortImpl : (Context, Seq[Sort]) => Sort,
		override val methods : Map[MethodId, NativeMethodDecl]
	) extends ClassDecl[NativeMethodDecl] {

		override def getField(fieldId : FieldId) : Option[FieldDecl] = None
		override def getMethod(methodId : MethodId) : Option[NativeMethodDecl] =
			methods.get(methodId)
	}


	trait TypedExpr {
		def typ : Type
	}

	trait Value
	case class NumV(n: Int) extends Value
	case class BoolV(b: Boolean) extends Value
	case class StringV(s: String) extends Value
	case class ObjectV(classId: ClassId, consistency: ConsistencyType) extends Value
	case object UnitV extends Value

	trait IRExpr
	trait IRLiteral extends IRExpr
	case class Num(n : Int) extends IRLiteral
	case object True extends IRLiteral
	case object False extends IRLiteral
	case class Str(s : String) extends IRLiteral
	case object UnitLiteral extends IRLiteral

	case class Var(id : VarId) extends IRExpr
	case class Let(id : VarId, namedExpr : IRExpr, body : IRExpr) extends IRExpr

	case class If(conditionExpr : IRExpr, thenExpr : IRExpr, elseExpr : IRExpr) extends IRExpr

	case class Equals(e1 : IRExpr, e2 : IRExpr) extends IRExpr

	case class New(classId: ClassId, typeArguments: Seq[Type], consistency: ConsistencyType) extends IRExpr

	case class Sequence(exprs: Seq[IRExpr]) extends IRExpr

	case object This extends IRExpr
	case class GetField(fieldId : FieldId) extends IRExpr
	case class SetField(fieldId : FieldId, value : IRExpr) extends IRExpr

	trait IRMethodCall extends IRExpr {
		def arguments : Seq[IRExpr]
		def methodId : MethodId
	}

	case class CallQuery(recv : IRExpr, methodId : MethodId, arguments : Seq[IRExpr]) extends IRMethodCall
	case class CallUpdate(recv : IRExpr, methodId : MethodId, arguments : Seq[IRExpr]) extends IRMethodCall

	case class CallUpdateThis(methodId : MethodId, arguments : Seq[IRExpr]) extends IRMethodCall
	case class CallUpdateField(fieldId : FieldId, methodId : MethodId, arguments : Seq[IRExpr]) extends IRMethodCall


	case class ProgramDecl(var classTable : ClassTable, body: IRExpr) {
		lazy val classes : Iterable[(ClassDecl[_ <: MethodDecl], ConsistencyType)] = makeClassTableIterable

		classTable = classTable.map {
			case ((id, consistency), decl: ObjectClassDecl) => ((id, consistency), FieldInference.infer(decl, consistency))
			case ((id, consistency), decl) => ((id, consistency), decl)  // TODO: PolyConsistency substitution
		}

		private def makeClassTableIterable : Iterable[(ClassDecl[_ <: MethodDecl], ConsistencyType)] = {
			def classesInType(typ : Type) : Set[(ClassId, ConsistencyType)] = typ match {
				case TypeVar(typeVarId, upperBound) => classesInType(upperBound)
				case CompoundType(ClassType(classId, typeArguments), consistencyType, _)=>
					Set((classId, consistencyType)) ++ typeArguments.foldLeft(Set.empty[(ClassId, ConsistencyType)])((set, typArg) => set ++ classesInType(typArg))
			}

			val classDecls = classTable.toSeq.map {
				case ((_, consistency), decl: ObjectClassDecl) => (FieldInference.infer(decl, consistency), consistency)
				case ((_, consistency), decl) => (decl, consistency)
			}
			val classDependenciesBuilder = Map.newBuilder[(ClassId, ConsistencyType), Set[(ClassId, ConsistencyType)]]

			for (classDecl <- classDecls) {
				classDecl match {
					case (NativeClassDecl(name, typeParameters, sort, methods), consistency) =>
						classDependenciesBuilder.addOne((name, consistency), Set())
					case (ObjectClassDecl(name, typeParameters, invariant, fields, methods), consistency) =>
						val dependencies : Set[(ClassId, ConsistencyType)] = fields.values.flatMap(decl => classesInType(decl.typ)).toSet
						classDependenciesBuilder.addOne((name, consistency), dependencies)
				}
			}
			val classDependencies = classDependenciesBuilder.result()

			val iterable = Iterable.newBuilder[(ClassDecl[_ <: MethodDecl], ConsistencyType)]
			val resolvedDependencies = mutable.Set.empty[(ClassId, ConsistencyType)]
			while (resolvedDependencies.size < classDecls.size) {
				val before = resolvedDependencies.size
				for (classDecl <- classDecls) {
					if (classDependencies(classDecl._1.classId, classDecl._2).subsetOf(resolvedDependencies)) {
						iterable.addOne(classDecl)
						resolvedDependencies.addOne(classDecl._1.classId, classDecl._2)
					}
				}
				if (resolvedDependencies.size == before)
					throw new Exception("cyclic dependency when resolving classes")
			}

			iterable.result()
		}
	}
}
