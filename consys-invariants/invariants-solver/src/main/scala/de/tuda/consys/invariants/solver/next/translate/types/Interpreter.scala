package de.tuda.consys.invariants.solver.next.translate.types

import de.tuda.consys.invariants.solver.next.ir.IR._
import de.tuda.consys.invariants.solver.next.ir.Types.ClassId
import de.tuda.stg.consys.core.store.akka.AkkaStore
import de.tuda.stg.consys.core.store.akka.levels.Strong

object Interpreter {

    private type Env = Map[VarId, Value]

    private val store = AkkaStore.fromAddress("127.0.0.1", 4445, 2181)

    def interp(programDecl: ProgramDecl): Value = {
        interp(programDecl.body, Map.empty)(programDecl.classTable)
    }

    // TODO: local side effects?
    def interp(expr: IRExpr, env: Env)(implicit classTable: ClassTable): Value = expr match {
        case Num(n) => NumV(n)

        case True => BoolV(true)

        case False => BoolV(false)

        case Str(s) => StringV(s)

        case UnitLiteral => UnitV

        case Var(id) => env(id)

        case Let(id, namedExpr, body) =>
            val e = interp(namedExpr, env)
            interp(body, env + (id -> e))

        case Equals(e1, e2) =>
            val v1 = interp(e1, env)
            val v2 = interp(e2, env)
            (v1, v2) match {
                case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
                case (BoolV(b1), BoolV(b2)) => BoolV(b1 == b2)
                case (StringV(s1), StringV(s2)) => BoolV(s1 == s2)
                case (UnitV, UnitV) => BoolV(true)
                case _ => ???
            }

        case If(conditionExpr, thenExpr, elseExpr) =>
            val c = interp(conditionExpr, env)
            c match {
                case BoolV(true) => interp(thenExpr, env)
                case BoolV(false) => interp(elseExpr, env)
            }

        case Sequence(exprs) => exprs.foldLeft[Value](UnitV)((r, e) => interp(e, env))

        case New(objectId, classId, typeArgs, consistency, args) =>
            val classDecl = classTable((classId, consistency))
            val argValues = args.map(arg => arg._1 -> interp(arg._2, env))
            val ref = store.transaction(ctx => Option.apply(ctx.replicate[ObjectV](objectId, Strong, objectId, classId, consistency, argValues))) // TODO
            ObjectV(objectId, classId, consistency, argValues)

        case CallQuery(recv, methodId, arguments) =>
            interp(recv, env) match {
                case obj@ObjectV(objectId, classId, consistency, argValues) =>
                    val classDecl = classTable((classId, consistency))
                    classDecl.methods(methodId) match {
                        case decl: NativeMethodDecl => ???
                        case decl: ObjectMethodDecl =>
                            val newEnv = (arguments zip decl.declaredParameters).map(e => {
                                val (arg, param) = e
                                param.name -> interp(arg, env)
                            }).toMap
                            interp(decl.body, newEnv.updated("this", obj))
                        case _ => ???
                    }
                case _ => ???
            }

        case CallUpdate(recv, methodId, arguments) =>
            interp(recv, env) match {
                case obj@ObjectV(objectId, classId, consistency, argValues) =>
                    val classDecl = classTable((classId, consistency))
                    classDecl.methods(methodId) match {
                        case decl: NativeMethodDecl => ???
                        case decl: ObjectMethodDecl =>
                            val newEnv = (arguments zip decl.declaredParameters).map(e => {
                                val (arg, param) = e
                                param.name -> interp(arg, env)
                            }).toMap
                            interp(decl.body, newEnv.updated("this", obj))
                        case _ => ???
                    }
                case _ => ???
            }

        case GetField(fieldId) =>
            val ObjectV(objectId, classId, consistency, argValues) = env("this")
            val res = store.transaction(ctx => {
                val ref = ctx.lookup[ObjectV](objectId, Strong) // TODO
                val fields = ref.resolve(ctx).getField[Map[FieldId, Value]]("fields")
                Option.apply(fields(fieldId))
            })
            res.get

        case SetField(fieldId, valueExpr) =>
            val ObjectV(objectId, classId, consistency, argValues) = env("this")
            val value = interp(valueExpr, env)
            val res = store.transaction(ctx => {
                val ref = ctx.lookup[ObjectV](objectId, Strong) // TODO
                val fields = ref.resolve(ctx).getField[Map[FieldId, Value]]("fields")
                ref.resolve(ctx).setField("fields", fields.updated(fieldId, value))
                Option.apply(0)
            })
            UnitV

        case _ => ???
    }
}
