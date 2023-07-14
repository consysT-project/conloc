package de.tuda.stg.conloc.invariants.lib.examples.shoppingcart;

import com.google.common.collect.Maps;
import de.tuda.stg.conloc.annotations.invariants.ReplicatedModel;
import de.tuda.stg.conloc.invariants.lib.crdts.PNCounter;

import java.util.List;
import java.util.Map;

import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.numOfReplicas;
import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.replicaId;
import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.stateful;

@ReplicatedModel public class Shop {

	public final Map<Integer, PNCounter> availableItems = Maps.newHashMap();


	//@ invariant (\forall long l; true; availableItems.get(l).getValue() >= 0);


	public Shop() {
	}

	//@ assignable \nothing;
	//@ ensures \result == availableItems.get(itemId).getValue();
	public int numItemsAvailable(int itemId) {
		if (availableItems.containsKey(itemId))
			return availableItems.get(itemId).getValue();
		else
			return 0;
	}

	//Note: While the code changes availableItems, the change does not change the specification of availableItems.
	//@ assignable \nothing;
	//@ ensures \result == availableItems.get(itemId);
	private PNCounter retrieveCounter(int itemId) {
		PNCounter counter;

		if (availableItems.containsKey(itemId))
			counter = availableItems.get(itemId);
		else {
			counter = new PNCounter();
			availableItems.put(itemId, counter);
		}

		return counter;
	}

	//@ requires availableItems.get(itemId).getValue() >= amount;
	//@ assignable availableItems;
	//@ ensures stateful( availableItems.get(itemId).dec(amount) );
	public Void lockItems(int itemId, int amount) {
		PNCounter counter = retrieveCounter(itemId);

		if (counter.getValue() < amount)
			throw new RuntimeException();

		counter.dec(amount);

		return null;
	}

	//@ assignable \nothing;
	public Void paymentProcess(Map<Integer, Integer> order) {
		return null;
	}

	//@ assignable availableItems;
	//@ ensures \result == (\forall int i; true; availableItems.get(i).getValue() >= order.get(i));
	//@ ensures (\forall int i; true; availableItems.get(i).getValue() >= order.get(i)) ? (\forall int i; true; availableItems.get(i).getValue() == availableItems.get(i).getValue() - order.get(i)) : true;
	public boolean checkout(Map<Integer, Integer> order) {
		boolean errorFlag = false;

		for (Map.Entry<Integer, Integer> item : order.entrySet()) {
			int remaining = numItemsAvailable(item.getKey());
			if (remaining >= item.getValue())
				lockItems(item.getKey(), item.getValue());
			else
				errorFlag = true;
		}

		if (!errorFlag) {
			paymentProcess(order);
			return true;
		}

		return false;

//		remaining = NUM_MAX
//		order foreach {
//			i =>
//			var remaining = numItemsAvailable(i.productId) //@ava-labeled
//			if (remaining > i.num) //@ava-labeled condition
//				lockItems(i.productId, i.num, timer)
//			else errorFlag = true
//		}
//		if (!errorFlag) paymentProcess(order, timer)
//		else display("Please adjust number!")
	}

	//@ requires
	public Void merge(Shop other) {
		for (Map.Entry<Integer, PNCounter> entry : other.availableItems.entrySet()) {
			PNCounter thisCounter = retrieveCounter(entry.getKey());
			thisCounter.merge(entry.getValue());
		}
		return null;
	}
}

