package de.tuda.consys.invariants.solver.next

import de.tuda.consys.invariants.solver.next.ir.IR.{ProgramDecl, SetField}
import de.tuda.consys.invariants.solver.next.ir.{CompoundType, Immutable, Mixed, Mutable, Natives, PolyConsistent, Strong, StrongOp, Weak, WeakOp}
import de.tuda.consys.invariants.solver.next.ir.Natives.{BOOL_TYPE, INT_TYPE, STRING_TYPE}
import de.tuda.consys.invariants.solver.next.translate.types.{Interpreter, TypeChecker}
//import de.tuda.consys.invariants.solver.next.translate.{ProgramModel, Z3Env}
import de.tuda.stg.consys.logging.Logger

import java.nio.file.{Path, Paths}

object Exec {

	{
		//loadZ3Libs()
	}

	private def loadLib(lib : Path) : Unit = {
		val libAbsolute = lib.toAbsolutePath
		Logger.info("load lib: " + libAbsolute)
		System.load(libAbsolute.toString)
	}

	private def loadZ3Libs() : Unit = { // Set the correct lib folder
		val libFolder = Paths.get("consys-invariants", "invariants-solver", "lib")
		// Load the correct libs depending on OS
		val osname = System.getProperty("os.name").toLowerCase
		if (osname.contains("mac")) {
			loadLib(libFolder.resolve("libz3.dylib"))
			loadLib(libFolder.resolve("libz3java.dylib"))
		}
		else if (osname.contains("linux")) {
			loadLib(libFolder.resolve("libz3.so"))
			loadLib(libFolder.resolve("libz3java.so"))
		}
		else throw new RuntimeException("unsupported OS: " + osname)
	}

/*
	def exampleProgram1() : ProgramDecl = {
		import ir.IR._

		val boxCls = ObjectClassDecl(
			"Box",
			Seq(),
			Equals(GetField("value"), Num(0)),
			Map(
				"value" -> FieldDecl("value", Type(INT_TYPE, Strong(), Mutable())),
				"name" -> FieldDecl("name", Type(STRING_TYPE, Strong(), Mutable()))
			),
			Map(
				"setVal" -> ObjectUpdateMethodDecl("setVal", Seq(VarDecl("x", Type(INT_TYPE, Strong(), Mutable()))),
					Let("a0", SetField("value", Var("x")),
						UnitLiteral
					)
				),
				"nameLength" -> ObjectUpdateMethodDecl("nameLength", Seq(),
					Let("a0",
						SetField("value",
							If(Equals(CallQuery(GetField("name"), "length", Seq()), Num(0)),
								CallQuery(GetField("name"), "length", Seq()),
								Num(0)
							)
						),
						UnitLiteral
					)
				),
				"foo2" -> ObjectUpdateMethodDecl("foo2", Seq(),
					Let("a0", CallUpdateThis("setVal", Seq(Num(42))),
						UnitLiteral
					)
				),
				"foo3" -> ObjectUpdateMethodDecl("foo3", Seq(),
					Let("a0", CallUpdateThis("setVal", Seq(Num(0))),
						UnitLiteral
					)
				),
				"getVal" -> ObjectQueryMethodDecl("getVal", Seq(), Type(INT_TYPE, Strong(), Mutable()), GetField("value")),
				"getVal2" -> ObjectQueryMethodDecl("getVal2", Seq(), Type(INT_TYPE, Strong(), Mutable()), CallQuery(This, "getVal", Seq()))
			)
		)

		val box2Cls = ObjectClassDecl(
			"Box2",
			Seq(),
			Equals(CallQuery(GetField("box"), "getVal", Seq()), Num(0)),
			Map(
				"box" -> FieldDecl("box", Type(boxCls.toType(Seq()), Strong(), Mutable()))
			),
			Map(
				"setVal" -> ObjectUpdateMethodDecl("setVal", Seq(VarDecl("x", Type(INT_TYPE, Strong(), Mutable()))),
					CallUpdateField("box", "setVal", Seq(Num(3)))
				)
			)
		)

		ProgramDecl(Map(
			"Int" -> Natives.INT_CLASS,
			"Bool" -> Natives.BOOL_CLASS,
			"String" -> Natives.STRING_CLASS,
			"Unit" -> Natives.UNIT_CLASS,
			"Box" -> boxCls,
			"Box2" -> box2Cls
		))
	}
 */
/*
	def exampleProgram2() : ProgramDecl = {
		import ir.IR._

		val boxCls = ObjectClassDecl(
			"Box",
			Seq(TypeVar("A")),
			True,
			Map(
				"value" -> FieldDecl("value", TypeVar("A"))
			),
			Map(
				"setVal" -> ObjectUpdateMethodDecl("setVal", Seq(VarDecl("x", TypeVar("A"))),
					Let("a0", SetField("value", Var("x")),
						UnitLiteral
					)
				),
				"getVal" -> ObjectQueryMethodDecl("getVal", Seq(), TypeVar("A"), GetField("value")),
			)
		)

		val userCls = ObjectClassDecl(
			"User",
			Seq(TypeVar("B"), TypeVar("A")),
			True,
			Map(
				"name" -> FieldDecl("name", ClassType("Box", Seq(TypeVar("B")))),
				"friends" -> FieldDecl("friends", ClassType("Set", Seq(TypeVar("A"))))
			),
			Map(
				"setName" -> ObjectUpdateMethodDecl("setName", Seq(VarDecl("x", TypeVar("B"))),
					Let("a0", CallUpdateField("name", "setVal", Seq(Var("x"))),
						UnitLiteral
					)
				),
				"getName" -> ObjectQueryMethodDecl("getName", Seq(), TypeVar("B"),
					CallQuery(GetField("name"), "getVal", Seq())
				),
				"hasFriend" -> ObjectQueryMethodDecl("hasFriend", Seq(VarDecl("x", TypeVar("A"))), BOOL_TYPE,
					CallQuery(GetField("friends"), "contains", Seq(Var("x")))
				)
			)
		)

		val nameCls = ObjectClassDecl(
			"Name",
			Seq(),
			True,
			Map(
				"value" -> FieldDecl("value", STRING_TYPE)
			),
			Map(
				"getValue" -> ObjectQueryMethodDecl("getValue", Seq(), STRING_TYPE,
					GetField("value")
				)
			)
		)


		ProgramDecl(Map(
			"Int" -> Natives.INT_CLASS,
			"Bool" -> Natives.BOOL_CLASS,
			"String" -> Natives.STRING_CLASS,
			"Unit" -> Natives.UNIT_CLASS,
			"Set" -> Natives.SET_CLASS,
			"Box" -> boxCls,
			"User" -> userCls,
			"Name" -> nameCls
		))
	}
*/
/*
	def exampleProgram3(): ProgramDecl = {
		import ir.IR._

		val boxCls = ObjectClassDecl(
			"Box",
			Seq(),
			True,
			Map(
				"value" -> FieldDecl("value", Type(INT_TYPE, Strong, Mutable)),
			),
			Map(
				"setVal" -> ObjectUpdateMethodDecl("setVal", Seq(VarDecl("x", Type(INT_TYPE, Strong, Mutable))),
					Let("a0", SetField("value", Var("x")),
						UnitLiteral
					)
				),
				"foo2" -> ObjectUpdateMethodDecl("foo2", Seq(),
					Let("a0", CallUpdateThis("setVal", Seq(Num(42))),
						UnitLiteral
					)
				),
				"getVal" -> ObjectQueryMethodDecl("getVal", Seq(), Type(INT_TYPE, Strong, Mutable),
					GetField("value")
				),
			)
		)

		ProgramDecl(Map(
			"Int" -> Natives.INT_CLASS,
			"Bool" -> Natives.BOOL_CLASS,
			"String" -> Natives.STRING_CLASS,
			"Unit" -> Natives.UNIT_CLASS,
			"Box" -> boxCls,
		))
	}
*/

	def exampleProgram3(): ProgramDecl = {
		import ir.IR._

		val boxCls = ObjectClassDecl(
			"Box",
			Seq(),
			True,
			Map(
				"value" -> FieldDecl("value", CompoundType(INT_TYPE, PolyConsistent, Mutable)),
			),
			Map(
				"setVal" -> ObjectUpdateMethodDecl("setVal", StrongOp, Seq(VarDecl("x", CompoundType(INT_TYPE, Strong, Mutable))),
					Sequence(
						Seq(SetField("value", Var("x")),
						UnitLiteral
					))
				),
				"getVal" -> ObjectQueryMethodDecl("getVal", WeakOp, Seq(), CompoundType(INT_TYPE, Strong, Mutable),
					GetField("value")
				),
			)
		)

		ProgramDecl(
			Map(
				("Int", Weak) -> Natives.INT_CLASS,
				("Int", Strong) -> Natives.INT_CLASS,
				("Bool", Weak) -> Natives.BOOL_CLASS,
				("Bool", Strong) -> Natives.BOOL_CLASS,
				("Unit", Weak) -> Natives.UNIT_CLASS,
				("Unit", Strong) -> Natives.UNIT_CLASS,
				//("Box", Mixed) -> boxCls,
				//("Box", Weak) -> boxCls,
				("Box", Strong) -> boxCls,
			),
			Let("x", New("b", "Box", Seq(), Strong, Map("value" -> Num(1))),
				Let("n", CallQuery(Var("x"), "getVal", Seq()),
					Sequence(Seq(
						CallUpdate(Var("x"), "setVal", Seq(Var("n"))),
					))
				)
			)
		)
	}

	def main(args : Array[String]) : Unit = {
		val p = exampleProgram3()
		TypeChecker.checkProgram(p)
		Interpreter.interp(p)

		/*
		val prog = exampleProgram3()

		val env = new Z3Env()
		val model = new ProgramModel(env, prog)
		model.create()

		 */
	}

}
