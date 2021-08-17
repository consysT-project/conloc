import de.tuda.stg.consys.annotations.methods.StrongOp;
import de.tuda.stg.consys.annotations.methods.WeakOp;
import de.tuda.stg.consys.checker.qual.Mixed;
import de.tuda.stg.consys.checker.qual.Strong;
import de.tuda.stg.consys.checker.qual.Weak;

import java.util.LinkedList;

public class DefaultTest { }
// ------------------------------------------------------------------------------------------------------
// Case where base class is not compatible with derived instantiation

class DefaultTestBase {
    private int i;

    // TODO: better error message, i.e. info about which derived class leads to error
    void setI(@Weak int j) {
        // :: error: assignment.type.incompatible
        i = j;
    }

    int getI() { return i; }
}

@Mixed(withDefault = StrongOp.class) class DefaultTestDerived extends DefaultTestBase {
    // since the base class methods are now @StrongOp, there is an error at setI()
}

// ------------------------------------------------------------------------------------------------------
// Cases for each default option

@Mixed(withDefault = StrongOp.class) class MixedStrong {
    private int i; // inferred strong

    void setI(@Weak int j, @Strong int k) {
        k = i;
        // :: error: assignment.type.incompatible
        i = j;
    }
}

@Mixed(withDefault = WeakOp.class) class MixedWeak {
    private int i; // inferred weak

    void setI(@Weak int j, @Strong int k) {
        // :: error: assignment.type.incompatible
        k = i;
        i = j;
    }
}

@Mixed class MixedNoDefault {
    private int i; // inferred weak

    void setI(@Weak int j, @Strong int k) {
        // :: error: assignment.type.incompatible
        k = i;
        i = j;
    }
}

// ---------------------------------------------------------------------------------------
// Case where inferred base class field is used in derived

@Mixed class Der extends LinkedList<String> {
    private @Strong int i;

    @StrongOp
    void test() {
        i = 0;
        // :: error: assignment.type.incompatible
        i = modCount; // modCount inferred weak
    }
}
