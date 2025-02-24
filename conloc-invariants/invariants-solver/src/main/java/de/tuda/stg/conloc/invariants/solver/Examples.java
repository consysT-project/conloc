package de.tuda.stg.conloc.invariants.solver;

import de.tuda.stg.conloc.invariants.solver.subset.ProgramConfig;

import java.nio.file.Path;
import java.nio.file.Paths;

public class Examples {

	public static final ProgramConfig DEFAULT_CONFIG = new ProgramConfig(
			false,
			false,
			true,
			1,
			"replica-01",
			3
	);

	public static final ProgramConfig STATEFUL_CONFIG = new ProgramConfig(
			true,
			false,
			true,
			1,
			"replica-01",
			3
	);


	public static final Path[] TEST = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/Test.java")
	};


	public static final Path[] BANK_ACCOUNT = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/bankaccount/BankAccount.java")
	};

	public static final Path[] BANK_ACCOUNT_LWW = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/bankaccountlww/BankAccountLWW.java")
	};

	public static final Path[] CREDIT_ACCOUNT = new Path[] {
//			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/creditaccount/SequentialCounter.java"),
//			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/creditaccount/SequentialCreditAccount.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/PNCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/creditaccount/ReplicatedCreditAccount.java")
	};

	public static final Path[] REPLICATED_CREDIT_ACCOUNT = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/immutable/GCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/immutable/PNCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/creditaccount/ReplicatedCreditAccount.java")
	};


	public static final Path[] JOINT_BANK_ACCOUNT = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/PNCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/jointbankaccount/JointBankAccount.java")
	};

	public static final Path[] RESETTABLE_COUNTER = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/resettablecounter/ResettableCounter.java")
	};


	public static final Path[] CONSENSUS = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/consensus/Consensus.java"),
	};

	public static final Path[] DISTRIBUTED_LOCK = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/distributedlock/DistributedLock.java"),
	};

	public static final Path[] TOURNAMENT = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/Player.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/Tournament.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/GSetPlayer.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/TwoPhaseSetPlayer.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/GSetTournament.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/TwoPhaseSetTournament.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/tournament/Tournaments.java")
	};
	public static final Path[] SHOPPING_CART = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/GSet.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/TwoPhaseSet.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/shoppingcart/Item.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/shoppingcart/ShoppingCart.java")
	};

	public static final Path[] SHOP = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/PNCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/shoppingcart/Shop.java")
	};
	
	public static final Path[] CRDTLIB = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/GCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/PNCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/GSet.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/TwoPhaseSet.java"),
	};

	public static final Path[] GCOUNTER = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/GCounter.java"),
	};

	public static final Path[] PNCOUNTER = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/PNCounter.java"),
	};

	public static final Path[] GSET = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/GSet.java"),
	};

	public static final Path[] TWOPHASESET = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/GSet.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/crdts/TwoPhaseSet.java"),
	};

	public static final Path[] RIAK_GCOUNTER = new Path[] {
		Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/GCounter.java")
	};

	public static final Path[] RIAK_GSET = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/GSet.java")
	};

	public static final Path[] RIAK_PNCOUNTER = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/GCounter.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/PNCounter.java")
	};

	public static final Path[] RIAK_TWOPHASESET = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/GSet.java"),
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/TwoPhaseSet.java")
	};

	public static final Path[] RIAK_ORSET = new Path[] {
			Paths.get("conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/riak/ORSet.java")
	};


}
