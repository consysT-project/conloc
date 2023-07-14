package de.tuda.stg.conloc.invariants.lib.examples.shoppingcart;

import de.tuda.stg.conloc.annotations.invariants.ReplicatedModel;
import de.tuda.stg.conloc.invariants.lib.crdts.PNCounter;
import de.tuda.stg.conloc.invariants.lib.crdts.TwoPhaseSet;

import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.stateful;


@ReplicatedModel
public class ShoppingCart {

	private final TwoPhaseSet<Item> items;
	private final PNCounter value;

	public ShoppingCart() {
		items = new TwoPhaseSet<>();
		value = new PNCounter();
	}

	//@ ensures stateful( items.add(item) );
	public Void addItem(Item item) {
		items.add(item);
		value.inc(item.getValue());
		return null;
	}

	public int getValue() {
		return value.getValue();
	}

	public Void merge(ShoppingCart other) {
		items.merge(other.items);
		return null;
	}
}
