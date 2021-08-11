import de.tuda.stg.consys.annotations.invariants.ReplicatedModel;

import java.lang.Math;

@ReplicatedModel public class BankAccountCRDT {
    /* Constants */
    // Constants have to be declared with static final.
    public static final int numOfReplicas = 3;


    /* Fields */
    // Virtual fields that can be accessed in constraints with `this` or using normal field references.
    public final int[] incs = new int[numOfReplicas], decs = new int[numOfReplicas];
    public int replicaId;

    /* Invariants */
    // Invariant definitions can use constants and fields.
    // Pure methods (i.e. methods that do not change the object state) that only use Z3 types can be used in constraints.
    //@ public invariant getValue() >= 0;
    //@ public invariant replicaId >= 0 && replicaId < numOfReplicas;


    /* Constructors */
    // Constructors define the initial state of an object.
    //@ requires id >= 0 && id < numOfReplicas;
    //@ ensures (\forall int i; i >= 0 && i < numOfReplicas; incs[i] == 0 && decs[i] == 0);
    //@ ensures replicaId == id;
    public BankAccountCRDT(int id) {
        if (!(id >= 0 && id < numOfReplicas))
            throw new IllegalArgumentException("id not in range.");
        this.replicaId = id;
    }


    /* Methods */
    // Methods define the interface for a replicated object.
    // Constraints on methods can use fields, constants, and method parameters.
    // Methods need an @assignable clause which specifies the fields that can change.
    // \old in postconditions defines the state before the method call.
    // \result in postconditions defines the return value of the method.

    //@ assignable replicaId;
    //@ ensures replicaId == id;
    public void setReplicaId(int id) {
        this.replicaId = id;
    }

    //@ assignable incs;
    //@ ensures (\forall int i; i >= 0 && i < numOfReplicas; \old(incs[i]) == 0);
    public void resetIncs() {
        for (int i = 0; i < incs.length; i++) incs[i] = 0;
    }

    //@ assignable incs, decs;
    //@ ensures (\forall int i; i >= 0 && i < numOfReplicas; incs[i] == 0);
    //@ ensures (\forall int i; i >= 0 && i < numOfReplicas; decs[i] == 0);
    public void reset() {
        for (int i = 0; i < incs.length; i++) incs[i] = 0;
        for (int i = 0; i < decs.length; i++) decs[i] = 0;
    }

    //@ assignable \nothing;
    //@ ensures \result == (\sum int i; i >= 0 && i < numOfReplicas; \old(incs[i]));
    public int sumIncs() {
        int res = 0;
        for (int inc : incs) {
            res += inc;
        }
        return res;
    }

    //@ assignable \nothing;
    //@ ensures \result == (\sum int i; i >= 0 && i < numOfReplicas; \old(decs[i]));
    public int sumDecs() {
        int result = 0;
        for (int dec : decs) {
            result += dec;
        }
        return result;
    }

    //@ assignable \nothing;
    //@ ensures \result == sumIncs() - sumDecs();
    public int getValue() {
        return sumIncs() - sumDecs();
    }


    //@ requires val >= 0;
    //@ assignable incs[replicaId];
    //@ ensures incs[replicaId] == \old(incs[replicaId]) + val;
    public void deposit(int val) {
        incs[replicaId] = incs[replicaId] + val;
    }


    //@ requires val >= 0;
    //@ requires  getValue() >= val;
    //@ assignable decs[replicaId];
    //@ ensures decs[\old(replicaId)] == \old(decs[replicaId]) + val;
    //@ ensures \result == \old(this);
    public BankAccountCRDT withdraw(int val) {
        if (val > getValue())
            throw new IllegalArgumentException("not enough balance to withdraw");

        decs[replicaId] = decs[replicaId] + val;
        return this;
    }

//    //@ requires  getValue() >= 1;
//    //@ assignable decs[replicaId];
//    //@ ensures this == withdraw(1);
//    public void withdrawOne() {
//        withdraw(1);
//    }


    /* Merge method */
    // Merge defines the conflict resolution of replicated objects.
    // Constraints can use fields, constants, and the other parameter.
    /*@
    @ requires (\sum int i; i >= 0 && i < numOfReplicas; Math.max(incs[i], other.incs[i]))
      - (\sum int i; i >= 0 && i < numOfReplicas; Math.max(decs[i], other.decs[i])) >= 0;
    @ ensures (\forall int i; i >= 0 && i < numOfReplicas;
                   incs[i] == Math.max(\old(incs[i]), other.incs[i])
                    && decs[i] == Math.max(\old(decs[i]), other.decs[i]));
    @*/
    public void merge(BankAccountCRDT other) {
        for (int i = 0; i < numOfReplicas; i++) {
            incs[i] = Math.max(incs[i], other.incs[i]);
            decs[i] = Math.max(decs[i], other.decs[i]);
        }
    }
}