package de.tuda.stg.conloc.invariants.lib.examples.shoppingcart;

import de.tuda.stg.conloc.annotations.invariants.ReplicatedModel;
import de.tuda.stg.conloc.invariants.lib.crdts.TwoPhaseSet;

import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.stateful;


@ReplicatedModel
public class ShoppingCart {

	private final TwoPhaseSet<Item> items;

	public ShoppingCart() {
		items = new TwoPhaseSet<>();
	}

	//@ ensures stateful( items.add(item) );
	public Void addItem(Item item) {
		items.add(item);
		return null;
	}

	public Void merge(ShoppingCart other) {
		items.merge(other.items);
		return null;
	}
}
