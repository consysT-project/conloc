import de.tuda.consys.invariants.solver.next.ir._

object TypeTest {
    def main(args : Array[String]): Unit = {
        println(Weak <= Strong)
        println(Strong <= Weak)
        println(Mixed <= Mixed)
    }
}
