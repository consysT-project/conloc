package de.tuda.stg.conloc.invariants.lib.examples.tournament;

import de.tuda.stg.conloc.annotations.invariants.DataModel;

@DataModel public class Player {

    private final String name;
    private int budget = 0;

    public Player(String name) {
        this.name = name;
    }

    //@ assignable \nothing;
    public int getBudget() {
        return budget;
    }

    //@ assignable budget;
    public void incBudget(int amount) {
        budget += amount;
    }
}
