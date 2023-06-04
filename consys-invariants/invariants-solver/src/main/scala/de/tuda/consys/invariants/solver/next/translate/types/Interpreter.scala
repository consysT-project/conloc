package de.tuda.consys.invariants.solver.next.translate.types

import de.tuda.consys.invariants.solver.next.ir.IR._
import de.tuda.consys.invariants.solver.next.ir.Types.ClassId
import de.tuda.stg.consys.core.store.akka.AkkaStore

object Interpreter {

    type Env = Map[VarId, Value]

    val store = AkkaStore.fromAddress("127.0.0.1", 4445, 2181)

    def interp(programDecl: ProgramDecl): Value = {
        interp(programDecl.body, Map.empty, programDecl.classTable)
    }

    // TODO: local side effects?
    // TODO: add this tracker
    def interp(expr: IRExpr, env: Env, classTable: ClassTable): Value = expr match {
        case Num(n) => NumV(n)

        case True => BoolV(true)

        case False => BoolV(false)

        case Str(s) => StringV(s)

        case UnitLiteral => UnitV

        case Var(id) => env(id)

        case Let(id, namedExpr, body) =>
            val e = interp(namedExpr, env, classTable)
            interp(body, env + (id -> e), classTable)

        case Equals(e1, e2) =>
            val v1 = interp(e1, env, classTable)
            val v2 = interp(e2, env, classTable)
            (v1, v2) match {
                case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
                case (BoolV(b1), BoolV(b2)) => BoolV(b1 == b2)
                case (StringV(s1), StringV(s2)) => BoolV(s1 == s2)
                case (UnitV, UnitV) => BoolV(true)
                case _ => ???
            }

        case If(conditionExpr, thenExpr, elseExpr) =>
            val c = interp(conditionExpr, env, classTable)
            c match {
                case BoolV(true) => interp(thenExpr, env, classTable)
                case BoolV(false) => interp(elseExpr, env, classTable)
            }

        case Sequence(exprs) => exprs.foldLeft[Value](UnitV)((r, e) => interp(e, env, classTable))

        case New(classId) =>
            // store.transaction(ctx => ctx.replicate()) TODO: how to use backend when we don't have jvm classes?
            ???

        case CallQuery(recv, methodId, arguments) =>
            interp(recv, env, classTable) match {
                case ObjectV(classId) =>
                    val classDecl = classTable(classId)
                    classDecl.methods(methodId) match {
                        case decl: NativeMethodDecl => ???
                        case decl: ObjectMethodDecl =>
                            val newEnv = (arguments zip decl.declaredParameters).map(e => {
                                val (arg, param) = e
                                param.name -> interp(arg, env, classTable)
                            }).toMap
                            interp(decl.body, newEnv, classTable)
                        case _ => ???
                    }
                case _ => ???
            }

        case _ => ???
    }
}
