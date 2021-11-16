package io.kaitai.struct.precompile

import io.kaitai.struct.{ClassTypeProvider, Log}
import io.kaitai.struct.datatype.DataType
import io.kaitai.struct.datatype.DataType.{ArrayType, SwitchType, UserType}
import io.kaitai.struct.format._
import io.kaitai.struct.translators.TypeDetector

class ParentTypes(classSpecs: ProtocolSpecs) {
  def run(): Unit = {
    classSpecs.forEachTopLevel { case (_, curClass) => markup(curClass) }
  }

  def markup(curClass: ClassSpec): Unit = {
    Log.typeProcParent.info(() => s"markupParentTypes(${curClass.nameAsStr})")

    curClass match {
      case curClass: ClassWithSeqSpec =>
        if (curClass.seq.nonEmpty)
          Log.typeProcParent.info(() => s"... seq")
        curClass.seq.foreach { attr =>
          markupParentTypesAdd(curClass, attr.dataType)
        }
      case curClass: ProtocolSpec =>
        curClass.types.foreach { case  (_, curClass) => markup(curClass) }
        curClass.inputs.foreach { case  (_, curClass) => markup(curClass) }
        return
    }
    curClass match {
      case curClass: StructSpec =>
        if (curClass.instances.nonEmpty)
          Log.typeProcParent.info(() => s"... instances")
        curClass.instances.foreach { case (_, instSpec) =>
          instSpec match {
            case pis: ParseInstanceSpec =>
              markupParentTypesAdd(curClass, pis.dataTypeComposite)
            case _: ValueInstanceSpec =>
            // value instances have no effect on parenting, just do nothing
          }
        }
      case _ =>
    }
  }

  private
  def markupParentTypesAdd(curClass: ClassSpec, dt: DataType): Unit = {
    dt match {
      case userType: UserType =>
        (userType.forcedParent match {
          case None =>
            Some(curClass)
          case Some(DataType.USER_TYPE_NO_PARENT) =>
            Log.typeProcParent.info(() => s"..... no parent type added")
            None
          case Some(parent) =>
            val provider = new ClassTypeProvider(classSpecs, curClass)
            val detector = new TypeDetector(provider)
            val parentType = detector.detectType(parent)
            Log.typeProcParent.info(() => s"..... enforced parent type = $parentType")
            parentType match {
              case ut: UserType =>
                Some(ut.classSpec.get)
              case other =>
                throw new TypeMismatchError(s"parent=$parent is expected to be either of user type or `false`, but $other found")
            }
        }).foreach((parentClass) => markupParentAs(parentClass, userType))
      case switchType: SwitchType =>
        switchType.cases.foreach {
          case (_, ut: UserType) =>
            markupParentAs(curClass, ut)
          case (_, _) =>
            // ignore everything else
        }
      case ArrayType(innerType) =>
        markupParentTypesAdd(curClass, innerType)
      case _ => // ignore, it's standard type
    }
  }

  def markupParentAs(curClass: ClassSpec, ut: UserType): Unit = {
    Log.typeProcParent.info(() => s"..... class=$ut has parent=${curClass.nameAsStr}")
    ut.classSpec match {
      case Some(usedClass) =>
        markupParentAs(curClass, usedClass)
      case None =>
        // TODO: replace with proper warning API
        Console.println(s"warning: tried to mark up parent=${curClass.name} for user type ${ut.name.mkString("::")}, but that type wasn't found, so doing nothing");
    }
  }

  def markupParentAs(parent: ClassSpec, child: ClassSpec): Unit = {
    child.parentClass match {
      case UnknownSpecClass$ =>
        child.parentClass = parent
        markup(child)
      case otherClass: StructSpec =>
        if (otherClass == parent) {
          // already done, don't do anything
        } else {
          // conflicting types, would be bad for statically typed languages
          // throw new RuntimeException(s"type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
          child.parentClass = GenericSpecStructClass$
        }
      case otherClass: InputSpec =>
        if (otherClass == parent) {
          // already done, don't do anything
        } else {
          // conflicting types, would be bad for statically typed languages
          // throw new RuntimeException(s"type '${attr.dataType}' has more than 1 conflicting parent types: ${otherName} and ${curClassName}")
          child.parentClass = GenericSpecStructClass$
        }
      case GenericSpecStructClass$ =>
      // already most generic case, do nothing
    }
  }
}
