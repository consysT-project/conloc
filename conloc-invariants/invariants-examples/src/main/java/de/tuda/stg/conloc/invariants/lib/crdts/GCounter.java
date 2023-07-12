package de.tuda.stg.conloc.invariants.lib.crdts;


import de.tuda.stg.conloc.Mergeable;
import de.tuda.stg.conloc.annotations.invariants.ReplicatedModel;
import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.numOfReplicas;
import static de.tuda.stg.conloc.invariants.utils.InvariantUtils.replicaId;

import de.tuda.stg.conloc.annotations.invariants.ArrayUtils;



@ReplicatedModel public class GCounter implements Mergeable<GCounter> {

    public int[] incs;


    /* Constructors */
    //@ ensures (\forall int i; i >= 0 && i < numOfReplicas(); incs[i] == 0);
    public GCounter() {
        this.incs = new int[numOfReplicas()];
    }


    /*@
    @ assignable \nothing;
    @ ensures \result == (\sum int incInd; incInd >= 0 && incInd < numOfReplicas(); incs[incInd]);
    @*/
    public int sumIncs() {
        int res = 0;
        for (int inc : incs) {
            res += inc;
        }
        return res;
    }

    /*@
    @ assignable \nothing;
    @ ensures \result == (\sum int i; i >= 0 && i < numOfReplicas(); incs[i]);
    @*/
    public int getValue() { return sumIncs(); }

    /*@
    @ assignable incs[replicaId()];
    @ ensures incs[replicaId()] == \old(incs[replicaId()]) + 1;
    @*/
    public Void inc() {
        incs[replicaId()] = incs[replicaId()] + 1;
        return null;
    }

    /*@
    @ requires n >= 0;
    @ assignable incs[replicaId()];
    @ ensures incs[replicaId()] == \old(incs[replicaId()]) + n;
    @*/
    public Void inc(int n) {
        incs[replicaId()] = incs[replicaId()] + n;
        return null;
    }



    //@ ensures (\forall int i; i >= 0 && i < numOfReplicas(); (\old(incs[i]) >= other.incs[i] ? incs[i] == \old(incs[i]) : incs[i] == other.incs[i]) );
    // ensures (\forall int i; i >= 0 && i < numOfReplicas(); incs == ArrayUtils.update(\old(incs), i, \old(incs[i]) >= other.incs[i] ? \old(incs[i]) : other.incs[i]));
    public Void merge(GCounter other) {
        for (int i = 0; i < numOfReplicas(); i++) {
            incs[i] = Math.max(incs[i], other.incs[i]);
        }
        return null;
    }
}
