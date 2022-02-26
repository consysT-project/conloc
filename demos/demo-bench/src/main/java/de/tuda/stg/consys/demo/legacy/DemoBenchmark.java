package de.tuda.stg.consys.demo.legacy;

import com.typesafe.config.Config;
import de.tuda.stg.consys.bench.legacy.DistributedBenchmark;
import de.tuda.stg.consys.bench.OutputFileResolver;
import de.tuda.stg.consys.core.legacy.ConsistencyLabel;
import de.tuda.stg.consys.japi.legacy.JConsistencyLevels;
import scala.Option;

import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import static de.tuda.stg.consys.japi.legacy.JConsistencyLevels.MIXED;
import static de.tuda.stg.consys.japi.legacy.JConsistencyLevels.WEAK;

/**
 * Created on 19.11.19.
 *
 * @author Mirko Köhler
 */
public abstract class DemoBenchmark extends DistributedBenchmark {

	private enum BenchmarkType {
		WEAK, MIXED, STRONG, OP_MIXED
	}

	private final BenchmarkType benchType;


	// An executor to use for asynchronous syncs.
	private ExecutorService executor = Executors.newCachedThreadPool(); //Currently unused
	private final Random random = new Random();


	public DemoBenchmark(String name, Config config, Option<OutputFileResolver> outputResolver) {
		super(name, config, outputResolver);
		String typeString = config.getString("consys.bench.demo.type");
		if (typeString == null) {
			throw new IllegalArgumentException("config key not found: consys.bench.demo.type");
		}
		benchType = BenchmarkType.valueOf(typeString.toUpperCase());
	}

	public DemoBenchmark(Config config, Option<OutputFileResolver> outputResolver) {
		this("default", config, outputResolver);
	}

	protected void doSync(Runnable f)  {
//		final JAkkaReplicaSystem sys = system();
//		executor.execute(JReplicaSystems.withSystem(sys).use(() -> f));
		if (shouldSync()) f.run();
	}

	protected boolean shouldSync() {
		return random.nextInt(100) < 20;
	}

	protected ConsistencyLabel getStrongLevel() {
		switch (benchType) {
			case WEAK: return WEAK;
			case OP_MIXED: return MIXED;
			default: return JConsistencyLevels.STRONG;
		}
	}

	protected ConsistencyLabel getWeakLevel() {
		switch (benchType) {
			case STRONG: return JConsistencyLevels.STRONG;
			case OP_MIXED: return MIXED;
			default: return JConsistencyLevels.WEAK;
		}
	}

	protected ConsistencyLabel getCausalLevel() {
		switch (benchType) {
			case MIXED: return JConsistencyLevels.CAUSAL;
			case STRONG: return JConsistencyLevels.STRONG;
			case WEAK: return JConsistencyLevels.WEAK;
		}

		throw new IllegalArgumentException("unsupported benchtype " + benchType);
	}

	@Override
	public void closeOperations() {
		try {
			executor.shutdown();
			executor.awaitTermination(5, TimeUnit.MINUTES);
			executor = Executors.newCachedThreadPool();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}
}
