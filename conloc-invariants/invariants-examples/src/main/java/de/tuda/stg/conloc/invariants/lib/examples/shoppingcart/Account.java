package de.tuda.stg.conloc.invariants.lib.examples.shoppingcart;

import de.tuda.stg.conloc.invariants.lib.crdts.PNCounter;

public class Account {

	private final PNCounter credits;


	public Account() {
		credits = new PNCounter();
	}

	public Void deposit(int amount) {
		credits.inc(amount);
		return null;
	}

	public Void checkout(ShoppingCart cart) {
		credits.dec(cart.getValue());
		return null;
	}
}


