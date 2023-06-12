package de.tuda.consys.invariants.solver.next.translate.types

import de.tuda.consys.invariants.solver.next.ir._
import de.tuda.consys.invariants.solver.next.ir.IR._

// TODO: add PolyConsistent substitution
object FieldInference {
    def infer(classDecl: ObjectClassDecl, thisType: ConsistencyType): ObjectClassDecl = {
        thisType match {
            case Strong => inferNonMixed(classDecl, Strong)
            case Weak => inferNonMixed(classDecl, Weak)
            case Mixed => inferMixed(classDecl)
        }
    }

    private def inferNonMixed(classDecl: ObjectClassDecl, thisType: ConsistencyType): ObjectClassDecl = {
        val newMethods = classDecl.methods.map(m => {
            val (id, decl) = m
            val newDecl = decl match {
                case ObjectQueryMethodDecl(name, _, declaredParameters, returnTyp, body) =>
                    ObjectQueryMethodDecl(name, thisType.operationLevel(), declaredParameters, returnTyp, body)
                case ObjectUpdateMethodDecl(name, _, declaredParameters, body) =>
                    ObjectUpdateMethodDecl(name, thisType.operationLevel(), declaredParameters, body)
                case _ => ???
            }

            (id, newDecl)
        })

        val newFields = classDecl.fields.map(f => {
            val (id, FieldDecl(fid, typ)) = f
            val newType = typ match {
                case CompoundType(baseType, _, mutabilityType) =>
                    CompoundType(baseType, thisType, mutabilityType)
                case TypeVar(typeVarId, upperBound) => ???
                case _ => ???
            }

            (id,  FieldDecl(fid, newType))
        })

        classDecl.copy(methods = newMethods, fields = newFields)
    }

    private def inferMixed(classDecl: ObjectClassDecl): ObjectClassDecl = {
        classDecl.methods.filter(m => m._2 match {
            case _: UpdateMethodDecl => true
            case _ => false
        }).foldLeft(classDecl)((c, m) => inferMethod(c, m._2.body, m._2.operationLevel))
    }

    private def inferMethod(classDecl: ObjectClassDecl, body: IRExpr, methodOp: OperationLevel): ObjectClassDecl = {
        body match {
            case SetField(fieldId, _) =>
                val newFields = classDecl.fields.map {
                    case (id, decl) if id == fieldId =>
                        val newTyp = decl.typ match {
                            case CompoundType(b, c, m) => CompoundType(b, c lub methodOp.consistencyType(), m)
                            case _ => ???
                        }
                        id -> FieldDecl(id, newTyp)
                    case x => x
                }
                classDecl.copy(fields = newFields)

            case CallUpdateField(fieldId, _, _) =>
                val newFields = classDecl.fields.map {
                    case (id, decl) if id == fieldId =>
                        val newTyp = decl.typ match {
                            case CompoundType(b, c, m) => CompoundType(b, c lub methodOp.consistencyType(), m)
                            case _ => ???
                        }
                        id -> FieldDecl(id, newTyp)
                    case x => x
                }
                classDecl.copy(fields = newFields)

            case Let(_, namedExpr, body) =>
                val r = inferMethod(classDecl, namedExpr, methodOp)
                inferMethod(r, body, methodOp)

            case If(conditionExpr, thenExpr, elseExpr) =>
                var r = inferMethod(classDecl, conditionExpr, methodOp)
                r = inferMethod(r, thenExpr, methodOp)
                inferMethod(r, elseExpr, methodOp)

            case _ => classDecl // TODO
        }
    }
}
