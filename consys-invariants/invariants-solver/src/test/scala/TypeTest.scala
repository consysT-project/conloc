import de.tuda.consys.invariants.solver.next.ir.Natives.INT_TYPE
import de.tuda.consys.invariants.solver.next.ir._

object TypeTest {
    def main(args : Array[String]): Unit = {
        println(CompoundType(INT_TYPE, Weak, Immutable) >= CompoundType(INT_TYPE, Strong, Mutable))
    }
}
