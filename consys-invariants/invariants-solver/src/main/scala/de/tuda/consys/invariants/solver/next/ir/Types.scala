package de.tuda.consys.invariants.solver.next.ir

import de.tuda.consys.invariants.solver.next.ir.Types.{ClassId, TypeVarId}

trait Type {
    def <=(t: Type): Boolean

    def >=(t: Type): Boolean

    def lub(t: Type): Type

    def glb(t: Type): Type
}

trait ConsistencyType {
    def <=(t: ConsistencyType): Boolean = ConsistencyTypeLattice(this).hasUpperBound(t)

    def >=(t: ConsistencyType): Boolean = ConsistencyTypeLattice(this).hasLowerBound(t)

    def lub(t: ConsistencyType): ConsistencyType = ???

    def glb(t: ConsistencyType): ConsistencyType = ???
}

case object Local extends ConsistencyType

case object Strong extends ConsistencyType

case object Mixed extends ConsistencyType

case object Weak extends ConsistencyType

case object Inconsistent extends ConsistencyType

object ConsistencyTypeLattice {
    private lazy val local: LatticeNode[ConsistencyType] = new LatticeNode(Local, List(strong), Nil)
    private lazy val strong: LatticeNode[ConsistencyType] = new LatticeNode(Strong, List(mixed), List(local))
    private lazy val mixed: LatticeNode[ConsistencyType] = new LatticeNode(Mixed, List(weak), List(strong))
    private lazy val weak: LatticeNode[ConsistencyType] = new LatticeNode(Weak, List(inconsistent), List(mixed))
    private lazy val inconsistent: LatticeNode[ConsistencyType] = new LatticeNode(Inconsistent, Nil, List(weak))

    def apply(t: ConsistencyType): LatticeNode[ConsistencyType] = t match {
        case Local => local
        case Strong => strong
        case Mixed => mixed
        case Weak => weak
        case Inconsistent => inconsistent
        case _ => sys.error("lattice node for consistency type not found")
    }
}

trait MutabilityType{
    def <=(t: MutabilityType): Boolean = MutabilityTypeLattice(this).hasUpperBound(t)

    def >=(t: MutabilityType): Boolean = MutabilityTypeLattice(this).hasLowerBound(t)

    def lub(t: MutabilityType): MutabilityType = ???

    def glb(t: MutabilityType): MutabilityType = ???
}

case object Mutable extends MutabilityType

case object Immutable extends MutabilityType

case object Bottom extends MutabilityType

object MutabilityTypeLattice {
    private lazy val bottom: LatticeNode[MutabilityType] = new LatticeNode(Bottom, List(mutable), Nil)
    private lazy val mutable: LatticeNode[MutabilityType] = new LatticeNode(Mutable, List(immutable), List(bottom))
    private lazy val immutable: LatticeNode[MutabilityType] = new LatticeNode(Immutable, Nil, List(mutable))

    def apply(t: MutabilityType): LatticeNode[MutabilityType] = t match {
        case Bottom => bottom
        case Mutable => mutable
        case Immutable => immutable
        case _ => sys.error("lattice node for consistency type not found")
    }
}

trait BaseType

case class ClassType(classId: ClassId, typeArguments: Seq[Type]) extends BaseType

case class CompoundType(baseType: BaseType, consistencyType: ConsistencyType, mutabilityType: MutabilityType) extends Type {
    if (mutabilityType == Bottom && consistencyType != Local)
        sys.error("invalid bottom type")

    def <=(t: Type): Boolean = t match {
        case CompoundType(baseType1, consistencyType1, mutabilityType1) =>
            if (baseType != baseType1)
                false // TODO: inheritance
            else if (consistencyType == Local && mutabilityType == Bottom)
                true
            else if (mutabilityType1 == Immutable)
                mutabilityType <= mutabilityType1 && consistencyType <= consistencyType1
            else
                mutabilityType <= mutabilityType1 && consistencyType == consistencyType1
        case _ => false
    }

    def >=(t: Type): Boolean = t match {
        case CompoundType(baseType, consistencyType, mutabilityType) => this == t || !(this <= t)
        case _ => false
    }

    def lub(t: Type): Type = t match {
        case CompoundType(baseType, consistencyType, mutabilityType) => ???
        case _ => ???
    }

    def glb(t: Type): Type = t match {
        case CompoundType(baseType, consistencyType, mutabilityType) => ???
        case _ => ???
    }
}

// TODO
case class TypeVar(typeVarId: TypeVarId) extends Type {
    def <=(t: Type): Boolean = ???

    def >=(t: Type): Boolean = ???

    def lub(t: Type): Type = ???

    def glb(t: Type): Type = ???
}

class LatticeNode[T](value: T, parents: => List[LatticeNode[T]], children: => List[LatticeNode[T]]) {
    def hasUpperBound(t: T): Boolean = t match {
        case `value` => true
        case _ => parents.exists(p => p.hasUpperBound(t))
    }

    def hasLowerBound(t: T): Boolean = t match {
        case `value` => true
        case _ => children.exists(p => p.hasLowerBound(t))
    }
}

object Types {
    type ClassId = String
    type TypeVarId = String
}
