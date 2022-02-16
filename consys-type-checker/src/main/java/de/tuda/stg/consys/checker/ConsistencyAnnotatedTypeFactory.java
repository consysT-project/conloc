package de.tuda.stg.consys.checker;

import com.sun.source.tree.MemberSelectTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.Tree;
import de.tuda.stg.consys.annotations.MethodWriteList;
import org.checkerframework.common.basetype.BaseAnnotatedTypeFactory;
import org.checkerframework.common.basetype.BaseTypeChecker;
import org.checkerframework.framework.type.*;
import org.checkerframework.framework.type.treeannotator.ListTreeAnnotator;
import org.checkerframework.framework.type.treeannotator.TreeAnnotator;
import org.checkerframework.framework.type.typeannotator.ListTypeAnnotator;
import org.checkerframework.framework.type.typeannotator.TypeAnnotator;
import org.checkerframework.javacutil.AnnotationBuilder;
import org.checkerframework.javacutil.TreeUtils;
import scala.Tuple2;
import scala.jdk.javaapi.CollectionConverters;

import javax.lang.model.element.*;
import java.util.ArrayList;
import java.util.Set;
import java.util.Stack;

public class ConsistencyAnnotatedTypeFactory extends BaseAnnotatedTypeFactory {

	public final MixedInferenceVisitor mixedInferenceVisitor;

	private final Stack<Tuple2<TypeElement, AnnotationMirror>> visitClassContext;
	private AnnotationMirror methodReceiverContext;

	public ConsistencyAnnotatedTypeFactory(BaseTypeChecker checker) {
        /*
        	Set useFlow to false if the flow analysis should be used.
         */
		super(checker, false);
		if (this.getClass().equals(ConsistencyAnnotatedTypeFactory.class)) {
			this.postInit();
		}

		this.mixedInferenceVisitor = new MixedInferenceVisitor(this);
		this.visitClassContext = new Stack<>();
	}


	@Override
	protected TreeAnnotator createTreeAnnotator() {
		TreeAnnotator others = super.createTreeAnnotator();
		return new ListTreeAnnotator(others, new ConsistencyTreeAnnotator(this));
	}

	@Override
	protected TypeAnnotator createTypeAnnotator() {
		TypeAnnotator others = super.createTypeAnnotator();
		return new ListTypeAnnotator(others, new ConsistencyTypeAnnotator(this));
	}

	@Override
	protected TypeHierarchy createTypeHierarchy() {
		DefaultTypeHierarchy hierarchy = new DefaultTypeHierarchy(
			checker, getQualifierHierarchy(), checker.getBooleanOption("ignoreRawTypeArguments", true), checker.hasOption("invariantArrays"));

		return new ConsistencyTypeHierarchy(hierarchy, this);
	}

	@Override
	protected QualifierHierarchy createQualifierHierarchy() {
		return new ConsistencyQualifierHierarchy(getSupportedTypeQualifiers(), getElementUtils(), this);
	}

	@Override
	public AnnotatedTypeMirror getAnnotatedType(Tree tree) {
		if (tree.getKind() == Tree.Kind.METHOD && ((MethodTree)tree).getName().toString().toLowerCase().startsWith("get")) {
			// disable cache when querying methods, so that we don't skip the return type adaptation
			// fields are never cached, so we don't need additional rules there
			boolean prevShouldCache = super.shouldCache;
			super.shouldCache = false;
			var result = super.getAnnotatedType(tree);
			super.shouldCache = prevShouldCache;
			return result;
		}

		return super.getAnnotatedType(tree);
	}

	@Override
	protected void addComputedTypeAnnotations(Tree tree, AnnotatedTypeMirror type, boolean iUseFlow) {
		var prevMethodReceiverContext = methodReceiverContext;
		// adapts the receiver context, so that the TypeAnnotator has the correct information when inferring
		// return types on mixed getters
		if (tree.getKind() == Tree.Kind.METHOD) {
			if (!visitClassContext.isEmpty())
				methodReceiverContext = visitClassContext.peek()._2;
		} else if (tree.getKind() == Tree.Kind.METHOD_INVOCATION) {
			var selectTree = ((MethodInvocationTree) tree).getMethodSelect();
			if (selectTree.getKind() == Tree.Kind.MEMBER_SELECT &&
					!TreeUtils.isExplicitThisDereference(((MemberSelectTree) selectTree).getExpression())) {
				methodReceiverContext = getAnnotatedType(((MemberSelectTree) selectTree).getExpression()).
						getAnnotationInHierarchy(TypeFactoryUtils.inconsistentAnnotation(this));
			} else if (!visitClassContext.isEmpty()) {
				methodReceiverContext = visitClassContext.peek()._2;
			}
		}

		super.addComputedTypeAnnotations(tree, type, iUseFlow);

		if (tree.getKind() == Tree.Kind.METHOD || tree.getKind() == Tree.Kind.METHOD_INVOCATION) {
			methodReceiverContext = prevMethodReceiverContext;
		}
	}

	@Override
	public void addComputedTypeAnnotations(Element elt, AnnotatedTypeMirror type) {
		var prevMethodReceiverContext = methodReceiverContext;
		// adapts the receiver context, so that the TypeAnnotator has the correct information when inferring
		// return types on mixed getters
		if (elt.getKind() == ElementKind.METHOD && !visitClassContext.isEmpty()) {
			methodReceiverContext = peekVisitClassContext()._2;
		}

		super.addComputedTypeAnnotations(elt, type);
		methodReceiverContext = prevMethodReceiverContext;
	}

	@Override
	public Set<AnnotationMirror> getDeclAnnotations(Element elt) {
		var result = super.getDeclAnnotations(elt);

		if (elt.getKind() == ElementKind.METHOD) {
			// add @MethodWriteList annotation containing each field the method updates (persists in .class files)
			var fieldSet = mixedInferenceVisitor.getMethodWriteList((ExecutableElement) elt);
			var annotationValue = new ArrayList<AnnotationValue>();
			if (fieldSet.isDefined()) {
				for (var field : CollectionConverters.asJava(fieldSet.get())) {
					// for some reason we have to provide the string array annotation value
					// as a List of AnnotationValues of strings
					annotationValue.add(AnnotationBuilder.elementNamesValues(
							"", TypeFactoryUtils.getQualifiedName(field)).
							get(""));
				}

				result.add(AnnotationBuilder.fromClass(getElementUtils(), MethodWriteList.class,
						AnnotationBuilder.elementNamesValues("value", annotationValue)));
			}
		}

		return result;
	}

	public boolean isInMixedClassContext() {
		return !visitClassContext.empty() && TypeFactoryUtils.isMixedQualifier(visitClassContext.peek()._2, this);
	}

	public void pushVisitClassContext(TypeElement clazz, AnnotationMirror type) {
		visitClassContext.push(new Tuple2<>(clazz, type));
	}

	public void popVisitClassContext() {
		visitClassContext.pop();
	}

	public Tuple2<TypeElement, AnnotationMirror> peekVisitClassContext() {
		return visitClassContext.peek();
	}

	public boolean isVisitClassContextEmpty() {
		return visitClassContext.isEmpty();
	}

	public AnnotationMirror getMethodReceiverContext() {
		return methodReceiverContext;
	}

	public ConsistencyVisitorImpl getVisitor() {
		return (ConsistencyVisitorImpl) checker.getVisitor();
	}
}
