package de.tuda.stg.conloc.annotations.invariants;

public class OnlyUsableInConstraintsException extends RuntimeException {

	public OnlyUsableInConstraintsException() {
		super("element is only usable in constraints");
	}
}
