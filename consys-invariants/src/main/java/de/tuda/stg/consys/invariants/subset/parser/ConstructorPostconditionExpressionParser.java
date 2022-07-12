package de.tuda.stg.consys.invariants.subset.parser;

import com.microsoft.z3.Expr;
import de.tuda.stg.consys.invariants.subset.model.BaseClassModel;
import de.tuda.stg.consys.invariants.subset.model.ConstructorModel;
import de.tuda.stg.consys.invariants.subset.model.ProgramModel;

/**
 * Parser for parsing expression inside of methods.
 */
public class ConstructorPostconditionExpressionParser extends MethodExpressionParser {

	public ConstructorPostconditionExpressionParser(ProgramModel model, BaseClassModel classModel, ConstructorModel constructorModel, Expr thisConst) {
		super(model, classModel, constructorModel, thisConst);
	}

	@Override
	public String toString() {
		return "ConstructorPostconditionExpressionParser{" +
				"methodModel=" + methodModel +
				'}';
	}

}
