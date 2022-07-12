//package de.tuda.stg.consys.demo.invariantdemos.schema.roundcounter;
//
//import de.tuda.stg.consys.demo.invariantdemos.Schema;
//import de.tuda.stg.consys.japi.legacy.JRef;
//
//import java.util.Random;
//
//public class ResettableCounterSchema extends Schema<ResettableCounterWithRound> {
//	private final Random random = new Random();
//
//
//	@Override
//	public ResettableCounterWithRound newInstance() {
//		return new ResettableCounterWithRound();
//	}
//
//	@Override
//	public Class<ResettableCounterWithRound> instanceClass() {
//		return ResettableCounterWithRound.class;
//	}
//
//	@Override
//	public void doOperation(JRef<ResettableCounterWithRound> ref) {
//		int rand = random.nextInt(100);
//		if (rand < 33) {
//			ref.invoke("inc");
//		} else if (rand < 66) {
//			ref.invoke("getValue");
//		} else if (rand < 100) {
//			ref.invoke("reset");
//		}
//	}
//}
