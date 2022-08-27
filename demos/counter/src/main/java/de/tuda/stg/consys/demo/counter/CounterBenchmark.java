package de.tuda.stg.consys.demo.counter;

import com.typesafe.config.Config;
import de.tuda.stg.consys.bench.OutputFileResolver;
import de.tuda.stg.consys.checker.qual.Mutable;
import de.tuda.stg.consys.demo.CassandraDemoBenchmark;
import de.tuda.stg.consys.demo.counter.schema.Counter;
import de.tuda.stg.consys.japi.Ref;
import scala.Option;

import java.util.Random;

/**
 * Created on 10.10.19.
 *
 * @author Mirko Köhler
 */
//@SuppressWarnings({"consistency"})
public class CounterBenchmark extends CassandraDemoBenchmark {
	public static void main(String[] args) {
		start(CounterBenchmark.class, args);
	}

	public CounterBenchmark(Config config, Option<OutputFileResolver> outputResolver) {
		super(config, outputResolver);
	}

	private final Random random = new Random();
	private Ref<@Mutable Counter> counter;

	@Override
	public String getName() {
		return "CounterBenchmark";
	}

	@Override
	public void setup() {
		super.setup();

		if (processId() == 0) {
			counter = store().<Ref<@Mutable Counter>>transaction(ctx -> Option.apply(
					ctx.replicate("counter", getWeakLevel(), Counter.class, 0)
			)).get();
		}
		barrier("counter_added");
		if (processId() != 0) {
			counter = store().<Ref<@Mutable Counter>>transaction(ctx -> Option.apply(
					ctx.lookup("counter", getWeakLevel(), Counter.class)
			)).get();
		}
	}

	@Override
	public void operation() {
		int roll = random.nextInt(100);
		store().transaction(ctx -> {
			if (roll < 50) {
				counter.ref().inc();
			} else {
				counter.ref().get();
			}
			return Option.apply(0);
		});
		System.out.print(".");
	}

	@Override
	public void cleanup() {
		super.cleanup();
	}
}
