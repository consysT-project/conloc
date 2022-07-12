import de.tuda.stg.consys.annotations.invariants.ReplicatedModel;

@ReplicatedModel class ResettableCounterWithRound {
    public static final int numOfReplicas = 10;
    public final int replicaId = 3;

    public int[] incs;
    public int round;


    /*@
    @ ensures round == 0;
    @ ensures (\forall int init; init>=0 && init<numOfReplicas; incs[init] == 0);
    @*/
    public ResettableCounterWithRound() {
        this.incs = new int[numOfReplicas];
        this.round = 0;
    }

    /*@
    @ assignable incs[replicaId];
    @ ensures incs[replicaId] == \old(incs[replicaId]) + 1;
    @ ensures (\forall int b; b>=0 && b<numOfReplicas && b!=replicaId; incs[b] == \old(incs[b]));
    @*/
    public void inc() { incs[replicaId] = incs[replicaId] + 1;}


    /*@
    @ assignable round, incs;
    @ ensures round == \old(round) + 1;
    @ ensures (\forall int a; a>=0 && a<numOfReplicas; incs[a] == 0);
    @*/
    public void reset() {
        round += 1;
        for(int i = 0; i < numOfReplicas; ++i)
            incs[i] = 0;
    }

    /*@
    @ assignable \nothing;
    @ ensures \result == (\sum int res; res>=0 && res<numOfReplicas; incs[res]);
    @*/
    public int getValue() {
        int val = 0;
        for(int i = 0; i < numOfReplicas; ++i)
            val += incs[i];
        return val;
    }


    /*@
    @ ensures (\old(round) < other.round) ==> (round == other.round) && (\forall int i; i >= 0 && i<numOfReplicas; incs[i] == other.incs[i]);
    @ ensures (\old(round) > other.round) ==> (round == \old(round)) && (\forall int i; i >= 0 && i<numOfReplicas; incs[i] == \old(incs[i]));
    @ ensures (\old(round) == other.round) ==> ((round == \old(round)) && (round == other.round)) && (\forall int i; i >= 0 && i < numOfReplicas;
                                                                                   (\old(incs[i]) >= other.incs[i] ? incs[i] == \old(incs[i]) : incs[i] == other.incs[i]));
    @*/
    public void merge(ResettableCounterWithRound other) {
        if(round < other.round) {
            round = other.round;
            for (int i = 0; i < numOfReplicas; i++)
                incs[i] = other.incs[i];
        }
        else if (round == other.round) {
            for (int i = 0; i < numOfReplicas; i++)
                incs[i] = Math.max(incs[i], other.incs[i]);
        }
    }
}