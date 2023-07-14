package de.tuda.stg.conloc.invariants.lib.examples.shoppingcart;

import de.tuda.stg.conloc.annotations.invariants.DataModel;

@DataModel
public class Item {

	private final String name;
	private final int value;

	public Item(String name, int value) {
		this.name = name;
		this.value = value;
	}

	public int getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		return obj instanceof Item && ((Item) obj).name.equals(name);
	}
}
