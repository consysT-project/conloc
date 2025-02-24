# *ConLoc*: Safe Consistency for Local-First Software

This is the repository of the research project **Con**sistent **Loc**al-first Software.

The project contains an annotation language for Java to automatically enforce
safety and maintain invariants in local-first applications.

The following example of a non-negative bank account showcases the usage of the language.
The complete file is available [here](https://github.com/consysT-project/conloc/tree/master/conloc-invariants/invariants-examples/src/main/java/de/tuda/stg/conloc/invariants/lib/examples/bankaccount).

```java
@ReplicatedModel public class BankAccount {
    /* Constants */
    // Constants have to be declared with static final.
    public static final String ACCOUNT_TYPE = "BANK";

    /* Fields */
    // Virtual fields that can be accessed in constraints with `this` or using normal field references.
    public final int[] incs, decs ;

    /* Invariants */
    // Invariant definitions can use constants and fields.
    // Pure methods (i.e. methods that do not change the object state) that only use Z3 types can be used in constraints.

    //@ public invariant getValue() >= 0;

    /* Constructors */
    // Constructors define the initial state of an object.

    //@ ensures (\forall int i; true; incs[i] == 0);
    //@ ensures (\forall int i; true; decs[i] == 0);
    public BankAccount() {
        incs = new int[numOfReplicas()];
        decs = new int[numOfReplicas()];
    }


    /* Methods */
    // Methods define the interface for a replicated object.
    // Constraints on methods can use fields, constants, and method parameters.
    // Methods need an @assignable clause which specifies the fields that can change.
    // \old in postconditions defines the state before the method call.
    // \result in postconditions defines the return value of the method.

    //@ assignable \nothing;
    //@ ensures \result == (\sum int i; i >= 0 && i < numOfReplicas(); \old(incs[i]));
    public int sumIncs() {
        int res = 0;
        for (int inc : incs) {
            res += inc;
        }
        return res;
    }

    //@ assignable \nothing;
    //@ ensures \result == (\sum int i; i >= 0 && i < numOfReplicas(); \old(decs[i]));
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
    //@ assignable incs[replicaId()];
    //@ ensures incs[replicaId()] == \old(incs[replicaId()]) + val;
    public void deposit(int val) {
        incs[replicaId()] = incs[replicaId()] + val;
    }


    //@ requires val >= 0;
    //@ requires  getValue() >= val;
    //@ assignable decs[replicaId()];
    //@ ensures decs[replicaId()] == \old(decs[replicaId()]) + val;
    public void withdraw(int val) {
        if (val > getValue())
            throw new IllegalArgumentException("not enough balance to withdraw");

        decs[replicaId()] = decs[replicaId()] + val;
    }


    /* Merge method */
    // Merge defines the conflict resolution of replicated objects.
    // Constraints can use fields, constants, and the other parameter.
    /*@
    @ requires (\sum int i; i >= 0 && i < numOfReplicas(); Math.max(incs[i], other.incs[i]))
    - (\sum int i; i >= 0 && i < numOfReplicas(); Math.max(decs[i], other.decs[i])) >= 0;
    @ ensures (\forall int i; i >= 0 && i < numOfReplicas();
            incs[i] == Math.max(\old(incs[i]), other.incs[i]) && decs[i] == Math.max(\old(decs[i]), other.decs[i]));
    @*/
    public void merge(BankAccount other) {
        for (int i = 0; i < numOfReplicas(); i++) {
            incs[i] = Math.max(incs[i], other.incs[i]);
            decs[i] = Math.max(decs[i], other.decs[i]);
        }
    }
}
```

## Execution

The project is built with Apache Maven. It requires Z3 and its Java bindings to be installed as a library, e.g., 
on Linux `libz3.so` and `libz3java.so` have to be available in the library path. The libraries can be obtained from
the official [Z3 Github](https://github.com/Z3Prover/z3).

After building the project, the ConLoc solver can be started via the [Main class](https://github.com/consysT-project/conloc/blob/master/conloc-invariants/invariants-solver/src/main/java/de/tuda/stg/conloc/invariants/solver/Main.java) in 
`conloc-invariants/invariants-solver`. This can be done in your Java IDE, e.g., IntelliJ, as well.


## Contact

The project is maintained by [Mirko Köhler](https://programming-group.com/members/koehler).

