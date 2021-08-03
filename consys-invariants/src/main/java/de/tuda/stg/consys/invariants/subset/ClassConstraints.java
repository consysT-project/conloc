package de.tuda.stg.consys.invariants.subset;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.microsoft.z3.BoolExpr;
import com.microsoft.z3.Expr;
import de.tuda.stg.consys.invariants.subset.model.ClassModel;
import de.tuda.stg.consys.invariants.subset.model.MergeMethodModel;
import de.tuda.stg.consys.invariants.subset.model.MethodModel;
import de.tuda.stg.consys.invariants.subset.model.ProgramModel;
import de.tuda.stg.consys.invariants.subset.parser.*;
import de.tuda.stg.consys.invariants.subset.utils.*;
import org.eclipse.jdt.internal.compiler.lookup.MethodBinding;
import org.jmlspecs.jml4.ast.*;

import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class ClassConstraints {

	private final ProgramModel model;
	private final ClassModel classModel;

	/** The invariant of the class */
	private final InvariantModel invariant;
	/** The initial predicate of the class */
	private final InitialConditionModel initial;

	/** Method preconditions */
	private final Map<MethodBinding, PreconditionModel> preconditions;
	/** Method postconditions */
	private final Map<MethodBinding, PostconditionModel> postconditions;

	/** Merge precondtion */
	private final MergePreconditionModel mergePrecondition;
	/** Merge postcondition */
	private final MergePostconditionModel mergePostcondition;


	/* Helper classes for predicate models. */
	public static class InvariantModel extends Z3Predicate1 {
		InvariantModel(Expr thisConst, Expr body) {
			super("I", thisConst, body);
		}
	}

	public static class InitialConditionModel extends Z3Predicate1 {
		InitialConditionModel(Expr thisConst, Expr body) {
			super("init", thisConst, body);
		}
	}

	public static class PreconditionModel extends Z3Predicate1 {
		PreconditionModel(Expr thisConst, Expr body) {
			super("pre", thisConst, body);
		}
	}

	public static class PostconditionModel extends Z3Predicate3 {
		PostconditionModel(Expr oldConst, Expr thisConst, Expr resultConst, Expr body) {
			super("post", oldConst, thisConst, resultConst, body);
		}
	}

	public static class MergePreconditionModel extends Z3Predicate2 {
		MergePreconditionModel(Expr thisConst, Expr otherConst, Expr body) {
			super("pre_merge", thisConst, otherConst, body);
		}
	}

	public static class MergePostconditionModel extends Z3Predicate3 {
		MergePostconditionModel(Expr oldConst, Expr otherConst, Expr thisConst, Expr body) {
			super("post_merge", oldConst, otherConst, thisConst, body);
		}
	}



	public ClassConstraints(ProgramModel model, ClassModel classModel) {
		this.model = model;
		this.classModel = classModel;

		JmlTypeDeclaration typ = classModel.getJmlType();

		// Setup the invariant
		Expr invariantVar = model.ctx.mkFreshConst("s", classModel.getClassSort());
		ClassExpressionParser parser = new ClassExpressionParser(model, classModel, invariantVar);
		Expr invariantExpr = parser.parseExpression(typ.getInvariant());
		invariant = new InvariantModel(invariantVar, invariantExpr);

		// Setup the initial condition
		initial = handleInitialConditions(classModel);

		// Setup the method pre/postconditions
		preconditions = Maps.newHashMap();
		postconditions = Maps.newHashMap();
		for(MethodModel methodModel : classModel.getMethods()) {
			preconditions.put(methodModel.getDecl().binding, handlePrecondition(methodModel));
			var postCondition = handlePostcondition(methodModel);
			postconditions.put(methodModel.getDecl().binding, postCondition);

			// If the method is pure.
			if (methodModel.isZ3Usable()) {
				Expr[] args = methodModel.getArguments().stream()
						.map(argModel -> argModel.getConst().orElseThrow())
						.toArray(Expr[]::new);

				var s0 = classModel.getFreshConst("s0");

				Expr[] applyArguments = Z3Utils.arrayPrepend(Expr[]::new, args, s0, s0);
				Expr[] forallArguments = Z3Utils.arrayPrepend(Expr[]::new, args, s0);

				var assertion =
						model.ctx.mkForall(
								forallArguments,
								postCondition.apply(
										s0,
										s0,
										model.ctx.mkApp(methodModel.getZ3FuncDecl().get(), applyArguments)
								),
								1,
								null,
								null,
								null,
								null
						);

				model.solver.add(assertion);
			}

		}

		// Setup the merge pre/postcondition.
		MergeMethodModel mergeModel = classModel.getMergeMethod();
		mergePrecondition = handleMergePrecondition(mergeModel);
		mergePostcondition = handleMergePostcondition(mergeModel);

	}

	private InitialConditionModel handleInitialConditions(ClassModel classModel) {
		Expr thisConst = model.ctx.mkFreshConst("s", classModel.getClassSort());

		List<Expr> initialConditions = Lists.newLinkedList();
		for (var constructor : classModel.getConstructors()) {
			var preParser = new ConstructorPreconditionExpressionParser(model, classModel, constructor);
			var preExpr = preParser.parseExpression(constructor.getDecl().getSpecification().getPrecondition());

			var postParser = new ConstructorPostconditionExpressionParser(model, classModel, constructor, thisConst);
			var postExpr = postParser.parseExpression(constructor.getDecl().getSpecification().getPostcondition());

			initialConditions.add(model.ctx.mkITE(preExpr, postExpr, model.ctx.mkFalse()));
		}

		var initialCondition = model.ctx.mkAnd(initialConditions.toArray(Expr[]::new));
		return new InitialConditionModel(thisConst, initialCondition);
	}

	private PreconditionModel handlePrecondition(MethodModel methodModel) {
		var specification = methodModel.getDecl().getSpecification();

		Expr thisConst = model.ctx.mkFreshConst("s", classModel.getClassSort());
		var parser = new MethodPreconditionExpressionParser(model, classModel, methodModel, thisConst);
		Expr expr = parser.parseExpression(specification.getPrecondition());
		return new PreconditionModel(thisConst, expr);
	}

	private MergePreconditionModel handleMergePrecondition(MergeMethodModel methodModel) {
		JmlMethodSpecification specification = methodModel.getDecl().getSpecification();

		Expr thisConst = model.ctx.mkFreshConst("s", classModel.getClassSort());
		Expr otherConst = model.ctx.mkFreshConst("s_other", classModel.getClassSort());
		var parser = new MergeMethodPreconditionExpressionParser(model, classModel, methodModel, thisConst, otherConst);
		Expr expr = parser.parseExpression(specification.getPrecondition());
		return new MergePreconditionModel(thisConst, otherConst, expr);
	}

	private MergePostconditionModel handleMergePostcondition(MergeMethodModel methodModel) {
		JmlMethodSpecification specification = methodModel.getDecl().getSpecification();

		Expr oldConst = model.ctx.mkFreshConst("s_old", classModel.getClassSort());
		Expr thisConst = model.ctx.mkFreshConst("s_new", classModel.getClassSort());
		Expr otherConst = model.ctx.mkFreshConst("s_other", classModel.getClassSort());

		var parser = new MergeMethodPostconditionExpressionParser(model, classModel, methodModel, thisConst, oldConst, otherConst);
		Expr expr = parser.parseExpression(specification.getPostcondition());
		return new MergePostconditionModel(oldConst, otherConst, thisConst, expr);
	}


	private PostconditionModel handlePostcondition(MethodModel methodModel) {
		JmlMethodSpecification specification = methodModel.getDecl().getSpecification();

		// Var for `\old(this)` references
		Expr oldConst = model.ctx.mkFreshConst("s_old", classModel.getClassSort());
		// Var for `this` references
		Expr thisConst = model.ctx.mkFreshConst("s_new", classModel.getClassSort());
		// Var for \result references
		Expr resultConst = methodModel.getReturnType().getSort()
			.map(sort -> model.ctx.mkFreshConst("res", sort))
			.orElse(null);

		// Parse the postcondition from JML @ensures specification
		var parser = new MethodPostconditionExpressionParser(model, classModel, methodModel, thisConst, oldConst, resultConst);
		Expr expr = parser.parseExpression(specification.getPostcondition());
		// Parse the assignable clause
		BoolExpr assignable;
		var maybeClause = methodModel.getAssignableClause();
		if (maybeClause.isEmpty()) {
			throw new IllegalArgumentException("no assignable clause for " + methodModel.getDecl());
		} else {
			assignable = parser.parseJmlAssignableClause(maybeClause.get());
		}
		// Combine the exprs for the postcondition
		Expr postcondition = model.ctx.mkAnd(expr, assignable);
		return new PostconditionModel(oldConst, thisConst, resultConst, postcondition);
	}

	public InvariantModel getInvariant() {
		return invariant;
	}

	public InitialConditionModel getInitialCondition() { return initial; }

	public MergePreconditionModel getMergePrecondition() {
		return mergePrecondition;
	}

	public MergePostconditionModel getMergePostcondition() {
		return mergePostcondition;
	}

	public PreconditionModel getPrecondition(MethodBinding method) {
		return preconditions.get(method);
	}

	public PostconditionModel getPostcondition(MethodBinding method) {
		return postconditions.get(method);
	}

	public ClassModel getClassModel() {
		return classModel;
	}

//	private Expr getForallInvariant(InvariantModel inv) {
//		var forallVar = model.ctx.mkFreshConst("s_inv", classModel.getClassSort());
//		model.ctx.mkForall(
//				new Expr[] {forallVar},
//				model.ctx.mkTrue(),
//
//
//		)
//	}

	@Override
	public String toString() {
		return "Class" + classModel.getClassName() + "====\n"
				+ "Invariant ====\n" + getInvariant() + "\n"
				+ "Initial ====\n" + getInitialCondition() + "\n"
				+ "Preconditions ====\n" + preconditions.entrySet().stream().map(entry -> String.valueOf(entry.getKey().selector) + ": " + entry.getValue() + "\n").collect(Collectors.joining())
				+ "Postconditions ====\n" + postconditions.entrySet().stream().map(entry -> String.valueOf(entry.getKey().selector) + ": " + entry.getValue() + "\n").collect(Collectors.joining())
				+ "Merge Precondition ====\n" +  getMergePrecondition()  + "\n"
				+ "Merge Postcondition ====\n" +  getMergePostcondition();
	}



}
